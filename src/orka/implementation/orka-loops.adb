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

with System;

with Ada.Synchronous_Task_Control;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Orka.Buffer_Fences;
with Orka.Futures;
with Orka.Simulation_Jobs;

package body Orka.Loops is

   use Ada.Real_Time;

   package STC renames Ada.Synchronous_Task_Control;

   Frame_Sync_Point : STC.Suspension_Object;

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

   protected body Input_Processor is
      procedure Reset is
      begin
         Processed := False;
      end Reset;

      procedure Process_Input is
      begin
         if not Processed then
            Window.Process_Input;
            Processed := True;
         end if;
      end Process_Input;

      entry Wait_Until_Processed when Processed is
      begin
         null;
      end Wait_Until_Processed;
   end Input_Processor;

   procedure Run_Simulation_Loop is
      package SJ renames Simulation_Jobs;

      Previous_Time : Time := Clock;
      Next_Time     : Time := Previous_Time;

      Lag : Time_Span := Time_Span_Zero;

      Scene_Array : Behaviors.Behavior_Array_Access := Behaviors.Empty_Behavior_Array;
   begin
      STC.Set_False (Frame_Sync_Point);

      --  Based on http://gameprogrammingpatterns.com/game-loop.html
      loop
         declare
            Current_Time : constant Time := Clock;
            Elapsed : constant Time_Span := Current_Time - Previous_Time;
         begin
            Previous_Time := Current_Time;
            Lag := Lag + Elapsed;

            exit when Handler.Should_Stop;

            --  Because the render loop needs to set and wait for a GL
            --  fence, wait until the render loop says we can continue
            STC.Suspend_Until_True (Frame_Sync_Point);
            STC.Set_False (Frame_Sync_Point);

            declare
               Iterations : constant Natural := Lag / Time_Step;

               Fixed_Update_Job : constant Jobs.Job_Ptr
                 := Jobs.Parallelize (SJ.Create_Fixed_Update_Job
                   (Scene_Array, Time_Step, Iterations),
                   Scene_Array'Length, 10);
            begin
               Lag := Lag - Iterations * Time_Step;
               Scene.Camera.Update (To_Duration (Lag));

               declare
                  Finished_Job : constant Jobs.Job_Ptr := SJ.Create_Finished_Job
                    (Scene_Array, Time_Step, Scene.Camera.View_Position);

                  Handle : Futures.Pointers.Pointer;
                  Status : Futures.Status;
               begin
                  Finished_Job.Set_Dependencies ((1 => Fixed_Update_Job));
                  Job_Manager.Queue.Enqueue (Fixed_Update_Job, Handle);

                  select
                     Handle.Get.all.Wait_Until_Done (Status);
                  or
                     delay until Clock + Handler.Frame_Limit;
                  end select;
               end;
            end;
            --  TODO Tell the render loop that it can swap the buffers

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

      Job_Manager.Shutdown;
   exception
      when others =>
         Job_Manager.Shutdown;
         raise;
   end Run_Simulation_Loop;

   procedure Run_Render_Loop is
      package Fences is new Orka.Buffer_Fences (Region_Type);

      Fence : Fences.Buffer_Fence := Fences.Create_Buffer_Fence;
   begin
      loop
         Input_Processor.Reset;

         if Window.Should_Close then
            Handler.Stop;
            exit;
         end if;

         Fence.Prepare_Index;
         --  Notify the simulation loop that it can continue
         STC.Set_True (Frame_Sync_Point);

         --  TODO Drain render queue
         --  TODO And call Process_Input.Process_Input when a message asks us
         --  TODO Render (Scene_Array, Scene.Camera);

         Fence.Advance_Index;
         Window.Swap_Buffers;
      end loop;
   end Run_Render_Loop;

end Orka.Loops;
