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

with System.Multiprocessors;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Synchronous_Barriers;

with Orka.Buffer_Fences;
with Orka.Transforms.Singles.Vectors;

with Orka.Workers;

package body Orka.Loops is

   use Ada.Real_Time;

   package Transforms renames Orka.Transforms.Singles.Vectors;

   -----------------------------------------------------------------------------

   procedure Fixed_Update
     (Scene         : Behaviors.Behavior_Array;
      Barrier       : in out Ada.Synchronous_Barriers.Synchronous_Barrier;
      Delta_Time    : Time_Span;
      View_Position : Transforms.Vector4)
   is
      DT : constant Duration := To_Duration (Delta_Time);
      DC : Boolean;
   begin
      for Behavior of Scene loop
         Behavior.Fixed_Update (DT);
      end loop;

      Ada.Synchronous_Barriers.Wait_For_Release (Barrier, DC);
   end Fixed_Update;

   procedure Update
     (Scene         : Behaviors.Behavior_Array;
      Barrier       : in out Ada.Synchronous_Barriers.Synchronous_Barrier;
      Delta_Time    : Time_Span;
      View_Position : Transforms.Vector4)
   is
      DT : constant Duration := To_Duration (Delta_Time);
      DC : Boolean;
   begin
      for Behavior of Scene loop
         Behavior.Update (DT);
      end loop;

      Ada.Synchronous_Barriers.Wait_For_Release (Barrier, DC);

      for Behavior of Scene loop
         Behavior.After_Update (DT, View_Position);
      end loop;

      Ada.Synchronous_Barriers.Wait_For_Release (Barrier, DC);
   end Update;

   package Fixed_Workers is new Orka.Workers
     (Positive (System.Multiprocessors.Number_Of_CPUs), "Physics", Fixed_Update);

   package Workers is new Orka.Workers
     (Positive (System.Multiprocessors.Number_Of_CPUs), "Worker", Update);

   -----------------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Behaviors.Behavior_Array, Behaviors.Behavior_Array_Access);

   function "<" (Left, Right : Behaviors.Behavior_Ptr) return Boolean is
      function Convert is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => Long_Integer);
   begin
      return Convert (Left.all'Address) < Convert (Right.all'Address);
   end "<";

   protected body Handler is
      procedure Stop is
      begin
         Stop_Flag := True;
      end Stop;

      procedure Set_Frame_Limit (Value : Time_Span) is
      begin
         Limit := Value;
      end Set_Frame_Limit;

      function Frame_Limit return Time_Span is
        (Limit);

      function Should_Stop return Boolean is
        (Stop_Flag);
   end Handler;

   protected body Scene is
      procedure Add (Object : Behaviors.Behavior_Ptr) is
      begin
         Behaviors_Set.Insert (Object);
         Modified_Flag := True;
      end Add;

      procedure Remove (Object : Behaviors.Behavior_Ptr) is
      begin
         Behaviors_Set.Delete (Object);
         Modified_Flag := True;
      end Remove;

      procedure Replace_Array (Target : in out Behaviors.Behavior_Array_Access) is
         Index : Positive := 1;
         Count : constant Positive := Positive (Behaviors_Set.Length);
      begin
         Free (Target);
         Target := new Behaviors.Behavior_Array'(1 .. Count => Behaviors.Null_Behavior);

         --  Copy the elements from the set to the array
         --  for faster iteration by the game loop
         for Element of Behaviors_Set loop
            Target (Index) := Element;
            Index := Index + 1;
         end loop;

         Modified_Flag := False;
      end Replace_Array;

      function Modified return Boolean is
        (Modified_Flag);

      procedure Set_Camera (Camera : Cameras.Camera_Ptr) is
      begin
         Scene_Camera := Camera;
      end Set_Camera;

      function Camera return Cameras.Camera_Ptr is
        (Scene_Camera);
   end Scene;

   procedure Run_Loop is
      Previous_Time : Time := Clock;
      Lag : Time_Span := Time_Span_Zero;
      Next_Time : Time := Previous_Time;

      Scene_Array : Behaviors.Behavior_Array_Access := Behaviors.Empty_Behavior_Array;
      Dummy_VP : constant Transforms.Vector4 := Transforms.Zero_Point;

      package Fences is new Orka.Buffer_Fences (Region_Type);

      Fence : Fences.Buffer_Fence := Fences.Create_Buffer_Fence;
   begin
      --  Based on http://gameprogrammingpatterns.com/game-loop.html
      loop
         --  TODO Move Window.Swap_Buffers to here?
         declare
            Current_Time : constant Time := Clock;
            Elapsed : constant Time_Span := Current_Time - Previous_Time;
         begin
            Previous_Time := Current_Time;
            Lag := Lag + Elapsed;

            Window.Process_Input;

            exit when Handler.Should_Stop or Window.Should_Close;

            Fence.Prepare_Index;

            while Lag > Time_Step loop
               Fixed_Workers.Barrier.Update (Scene_Array, Time_Step, Dummy_VP);
               Lag := Lag - Time_Step;
            end loop;

            Scene.Camera.Update (To_Duration (Lag));
            Workers.Barrier.Update (Scene_Array, Lag, Scene.Camera.View_Position);

            Window.Swap_Buffers;

            --  Render the scene *after* having swapped buffers (sync point)
            --  so that rendering on GPU happens in parallel with CPU work
            --  during the next frame
            Render (Scene_Array, Scene.Camera);

            Fence.Advance_Index;

            if Scene.Modified then
               Scene.Replace_Array (Scene_Array);
            end if;

            declare
               New_Elapsed : constant Time_Span := Clock - Current_Time;
            begin
               if New_Elapsed > Handler.Frame_Limit then
                  Next_Time := Current_Time;
               else
                  Next_Time := Next_Time + Handler.Frame_Limit;
                  delay until Next_Time;
               end if;
            end;
         end;
      end loop;

      Workers.Barrier.Shutdown;
      Fixed_Workers.Barrier.Shutdown;
   exception
      when others =>
         Workers.Barrier.Shutdown;
         Fixed_Workers.Barrier.Shutdown;
         raise;
   end Run_Loop;

end Orka.Loops;
