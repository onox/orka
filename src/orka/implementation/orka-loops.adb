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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
with Ada.Exceptions;

with Orka.Futures;
with Orka.Simulation_Jobs;

package body Orka.Loops is

   use Ada.Real_Time;

   Render_Job_Start, Render_Job_Finish : Jobs.Job_Ptr := Jobs.Null_Job;

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

   package SJ renames Simulation_Jobs;

   procedure Run_Game_Loop is
      Previous_Time : Time := Clock;
      Next_Time     : Time := Previous_Time;

      Lag : Time_Span := Time_Span_Zero;

      Scene_Array : Behaviors.Behavior_Array_Access := Behaviors.Empty_Behavior_Array;
      Batch_Length : constant := 10;
   begin
      Scene.Replace_Array (Scene_Array);

      --  Based on http://gameprogrammingpatterns.com/game-loop.html
      loop
         declare
            Current_Time : constant Time := Clock;
            Elapsed : constant Time_Span := Current_Time - Previous_Time;
         begin
            Previous_Time := Current_Time;
            Lag := Lag + Elapsed;

            exit when Handler.Should_Stop;

            declare
               Iterations : constant Natural := Lag / Time_Step;
            begin
               Lag := Lag - Iterations * Time_Step;
               Scene.Camera.Update (To_Duration (Lag));

               declare
                  Fixed_Update_Job : constant Jobs.Job_Ptr
                    := Jobs.Parallelize (SJ.Create_Fixed_Update_Job
                         (Scene_Array, Time_Step, Iterations), Scene_Array'Length, Batch_Length);

                  Finished_Job : constant Jobs.Job_Ptr := SJ.Create_Finished_Job
                    (Scene_Array, Time_Step, Scene.Camera.View_Position, Batch_Length);

                  Render_Start_Job : constant Jobs.Job_Ptr
                    := new Jobs.GPU_Job'Class'(Jobs.GPU_Job'Class (Render_Job_Start.all));

                  Render_Scene_Job : constant Jobs.Job_Ptr
                    := SJ.Create_Scene_Render_Job (Render, Scene_Array, Scene.Camera);

                  Render_Finish_Job : constant Jobs.Job_Ptr
                    := new Jobs.GPU_Job'Class'(Jobs.GPU_Job'Class (Render_Job_Finish.all));

                  Handle : Futures.Pointers.Mutable_Pointer;
                  Status : Futures.Status;
               begin
                  Orka.Jobs.Chain
                    ((Render_Start_Job, Fixed_Update_Job, Finished_Job,
                      Render_Scene_Job, Render_Finish_Job));

                  Job_Manager.Queue.Enqueue (Render_Start_Job, Handle);

                  Handle.Get.Wait_Until_Done (Status);
               end;
            end;

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
   end Run_Game_Loop;

   procedure Stop_Loop is
   begin
      Handler.Stop;
   end Stop_Loop;

   procedure Run_Loop is
      Fence : aliased SJ.Fences.Buffer_Fence := SJ.Fences.Create_Buffer_Fence;
   begin
      Render_Job_Start  := SJ.Create_Start_Render_Job (Fence'Unchecked_Access, Window);
      Render_Job_Finish := SJ.Create_Finish_Render_Job (Fence'Unchecked_Access, Window,
        Stop_Loop'Unrestricted_Access);

      declare
         task Simulation;

         use Ada.Exceptions;

         task body Simulation is
         begin
            Run_Game_Loop;
         exception
            when Error : others =>
               Ada.Text_IO.Put_Line ("Exception game loop: " & Exception_Information (Error));
         end Simulation;
      begin
         Job_Manager.Executors.Execute_Jobs
           ("Renderer", Job_Manager.Queues.GPU, Job_Manager.Queue'Access);
      end;
   end Run_Loop;

end Orka.Loops;
