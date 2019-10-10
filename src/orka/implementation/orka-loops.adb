--  SPDX-License-Identifier: Apache-2.0
--
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;

with Orka.Futures;
with Orka.Logging;
with Orka.Simulation_Jobs;

package body Orka.Loops is

   use Orka.Logging;
   package Messages is new Orka.Logging.Messages (Game_Loop);

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

      function Frame_Limit return Time_Span is (Limit);

      procedure Enable_Limit (Enable : Boolean) is
      begin
         Limit_Flag := Enable;
      end Enable_Limit;

      function Limit_Enabled return Boolean is (Limit_Flag);

      function Should_Stop return Boolean is (Stop_Flag);
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
         pragma Assert (Modified);

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

   procedure Stop_Loop is
   begin
      Handler.Stop;
   end Stop_Loop;

   procedure Run_Game_Loop (Fence : not null access SJ.Fences.Buffer_Fence) is
      Previous_Time : Time := Clock;
      Next_Time     : Time := Previous_Time;

      Lag : Time_Span := Time_Span_Zero;

      Scene_Array : Behaviors.Behavior_Array_Access := Behaviors.Empty_Behavior_Array;
      Batch_Length : constant := 10;

      One_Second  : constant Time_Span := Seconds (1);

      Frame_Counter          : Natural := 0;
      Exceeded_Frame_Counter : Natural := 0;
      Clock_FPS_Start : Time := Clock;

      Stat_Sum : Time_Span := Time_Span_Zero;
      Stat_Min : Duration  := To_Duration (One_Second);
      Stat_Max : Duration  := To_Duration (-One_Second);
   begin
      Scene.Replace_Array (Scene_Array);

      Messages.Insert (Debug, "Simulation tick resolution: " & Trim (Image (Tick)));

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

                  Render_Scene_Job : constant Jobs.Job_Ptr
                    := SJ.Create_Scene_Render_Job (Render, Scene_Array, Scene.Camera);

                  Render_Start_Job  : constant Jobs.Job_Ptr
                    := SJ.Create_Start_Render_Job (Fence, Window);
                  Render_Finish_Job : constant Jobs.Job_Ptr
                    := SJ.Create_Finish_Render_Job (Fence, Window,
                      Stop_Loop'Unrestricted_Access);

                  Handle : Futures.Pointers.Mutable_Pointer;
                  Status : Futures.Status;
               begin
                  Orka.Jobs.Chain
                    ((Render_Start_Job, Fixed_Update_Job, Finished_Job,
                      Render_Scene_Job, Render_Finish_Job));

                  Job_Manager.Queue.Enqueue (Render_Start_Job, Handle);

                  declare
                     Frame_Future : constant Orka.Futures.Future_Access := Handle.Get.Value;
                  begin
                     select
                        Frame_Future.Wait_Until_Done (Status);
                     or
                        delay until Current_Time + Maximum_Frame_Time;
                        raise Program_Error with
                          "Maximum frame time of " & Trim (Image (Maximum_Frame_Time)) &
                          " exceeded";
                     end select;
                  end;
               end;
            end;

            if Scene.Modified then
               Scene.Replace_Array (Scene_Array);
            end if;

            declare
               Total_Elapsed  : constant Time_Span := Clock - Clock_FPS_Start;
               Limit_Exceeded : constant Time_Span := Elapsed - Handler.Frame_Limit;
            begin
               Frame_Counter := Frame_Counter + 1;

               if Limit_Exceeded > Time_Span_Zero then
                  Stat_Sum := Stat_Sum + Limit_Exceeded;
                  Stat_Min := Duration'Min (Stat_Min, To_Duration (Limit_Exceeded));
                  Stat_Max := Duration'Max (Stat_Max, To_Duration (Limit_Exceeded));
                  Exceeded_Frame_Counter := Exceeded_Frame_Counter + 1;
               end if;

               if Total_Elapsed > One_Second then
                  declare
                     FPS : constant Duration
                       := Duration (Frame_Counter) / To_Duration (Total_Elapsed);
                  begin
                     Messages.Insert (Debug, "FPS: " & Trim (FPS'Image));
                  end;

                  if Exceeded_Frame_Counter > 0 then
                     declare
                        Avg : constant Time_Span := Stat_Sum / Exceeded_Frame_Counter;
                     begin
                        Messages.Insert (Debug, "Frame time limit (" &
                          Trim (Image (Handler.Frame_Limit)) & ") exceeded " &
                          Trim (Exceeded_Frame_Counter'Image) & " times by:");
                        Messages.Insert (Debug, "  avg: " & Image (Avg));
                        Messages.Insert (Debug, "  min: " & Image (To_Time_Span (Stat_Min)));
                        Messages.Insert (Debug, "  max: " & Image (To_Time_Span (Stat_Max)));
                     end;
                  end if;

                  Clock_FPS_Start := Clock;
                  Frame_Counter          := 0;
                  Exceeded_Frame_Counter := 0;

                  Stat_Sum := Time_Span_Zero;
                  Stat_Min := To_Duration (One_Second);
                  Stat_Max := To_Duration (Time_Span_Zero);
               end if;
            end;

            if Handler.Limit_Enabled then
               --  Do not sleep if Next_Time fell behind more than one frame
               --  due to high workload (FPS dropping below limit), otherwise
               --  the FPS will be exceeded during a subsequent low workload
               --  until Next_Time has catched up
               if Next_Time < Current_Time - Handler.Frame_Limit then
                  Next_Time := Current_Time;
               else
                  Next_Time := Next_Time + Handler.Frame_Limit;
                  delay until Next_Time;
               end if;
            end if;
         end;
      end loop;

      Job_Manager.Shutdown;
   exception
      when others =>
         Job_Manager.Shutdown;
         raise;
   end Run_Game_Loop;

   procedure Run_Loop is
      Fence : aliased SJ.Fences.Buffer_Fence := SJ.Fences.Create_Buffer_Fence;
   begin
      declare
         --  Create a separate task for the game loop. The current task
         --  will be used to dequeue and execute GPU jobs.
         task Simulation;

         use Ada.Exceptions;

         task body Simulation is
         begin
            System.Multiprocessors.Dispatching_Domains.Set_CPU (1);
            Run_Game_Loop (Fence'Unchecked_Access);
         exception
            when Error : others =>
               Logging.Insert_Message
                 (Logging.Game_Loop, Logging.Error, 0, Exception_Information (Error));
         end Simulation;
      begin
         System.Multiprocessors.Dispatching_Domains.Set_CPU (1);

         --  Execute GPU jobs in the current task
         Job_Manager.Execute_GPU_Jobs;
      end;
   end Run_Loop;

end Orka.Loops;
