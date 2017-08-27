--  Copyright (c) 2017 onox <denkpadje@gmail.com>
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

with System.Multiprocessors.Dispatching_Domains;

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Synchronous_Barriers;
with Ada.Text_IO;

with Orka.OS;

package body Orka.Workers is

   Sync_Barrier : Ada.Synchronous_Barriers.Synchronous_Barrier (Count);

   protected body Barrier is
      procedure Update
        (Scene         : not null Behaviors.Behavior_Array_Access;
         Delta_Time    : Duration;
         View_Position : Transforms.Vector4) is
      begin
         Barrier.Scene := Scene;
         DT := Delta_Time;
         VP := View_Position;

         Updated := True;
      end Update;

      procedure Shutdown is
      begin
         Stop := True;
      end Shutdown;

      entry Wait_For_Release
        (Scene         : out not null Behaviors.Behavior_Array_Access;
         Delta_Time    : out Duration;
         View_Position : out Transforms.Vector4;
         Shutdown      : out Boolean)
      when (Wait_For_Release'Count = Count and Updated) or Ready or Stop is
      begin
         Scene         := Barrier.Scene;
         Delta_Time    := DT;
         View_Position := VP;
         Shutdown      := Stop;

         Ready    := Wait_For_Release'Count > 0;
         Updated  := False;
      end Wait_For_Release;
   end Barrier;

   procedure Get_Indices
     (Scene : Natural;
      ID    : Positive;
      Start_Index, End_Index : out Natural)
   is
      Job_Count : constant Natural := Scene / Count;
      Rem_Count : constant Natural := Scene rem Count;

      Count : constant Natural := Job_Count + (if ID <= Rem_Count then 1 else 0);

      Offset : constant Natural := (if ID <= Rem_Count then ID else 1 + Rem_Count);
   begin
      Start_Index := (ID - 1) * Job_Count + Offset;
      End_Index   := Start_Index + Count - 1;
   end Get_Indices;

   task body Worker_Task is
      package SM renames System.Multiprocessors;
      package SF renames Ada.Strings.Fixed;

      ID_Image : constant String := Positive'Image (Data.ID);

      Scene : not null Behaviors.Behavior_Array_Access := new Behaviors.Behavior_Array (1 .. 0);
      DT    : Duration;
      VP    : Transforms.Vector4;

      Stop, DC : Boolean := False;
      Start_Index, End_Index : Natural;
   begin
      --  Set the CPU affinity of the task to its corresponding CPU core
      SM.Dispatching_Domains.Set_CPU (SM.CPU (Data.ID));
      Orka.OS.Set_Task_Name ("Worker #" & SF.Trim (ID_Image, Ada.Strings.Left));

      loop
         Barrier.Wait_For_Release (Scene, DT, VP, Stop);
         exit when Stop;

         Get_Indices (Scene.all'Length, Data.ID, Start_Index, End_Index);

         for Behavior of Scene.all (Start_Index .. End_Index) loop
            Behavior.Update (DT);
         end loop;

         Ada.Synchronous_Barriers.Wait_For_Release (Sync_Barrier, DC);

         for Behavior of Scene.all (Start_Index .. End_Index) loop
            Behavior.After_Update (DT, VP);
         end loop;

         Ada.Synchronous_Barriers.Wait_For_Release (Sync_Barrier, DC);
      end loop;
   exception
      when Error : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
   end Worker_Task;

   function Make_Workers return Worker_Array is
   begin
      return Result : Worker_Array (1 .. Count) do
         for Index in Result'Range loop
            Result (Index).ID := Index;
         end loop;
      end return;
   end Make_Workers;

end Orka.Workers;
