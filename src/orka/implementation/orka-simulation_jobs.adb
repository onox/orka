--  Copyright (c) 2018 onox <denkpadje@gmail.com>
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

package body Orka.Simulation_Jobs is

   type Fixed_Update_Job is new Jobs.Abstract_Parallel_Job with record
      Scene     : not null Behaviors.Behavior_Array_Access;
      Time_Step : Time_Span;
      Count     : Natural;
   end record;

   type Update_Job is new Jobs.Abstract_Parallel_Job with record
      Scene     : not null Behaviors.Behavior_Array_Access;
      Time_Step : Time_Span;
   end record;

   type After_Update_Job is new Jobs.Abstract_Parallel_Job with record
      Scene     : not null Behaviors.Behavior_Array_Access;
      Time_Step : Time_Span;
      View_Position : Transforms.Vector4;
   end record;

   type Finished_Fixed_Update_Job is new Jobs.Abstract_Job with record
      Scene     : not null Behaviors.Behavior_Array_Access;
      Time_Step : Time_Span;
      View_Position : Transforms.Vector4;
   end record;

   overriding
   procedure Execute (Object : Fixed_Update_Job; From, To : Positive);

   overriding
   procedure Execute (Object : Update_Job; From, To : Positive);

   overriding
   procedure Execute (Object : After_Update_Job; From, To : Positive);

   overriding
   procedure Execute
     (Object  : Finished_Fixed_Update_Job;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr));

   -----------------------------------------------------------------------------

   type Start_Render_Job is new Jobs.Abstract_Job and Jobs.GPU_Job with record
      Fence  : not null access Fences.Buffer_Fence;
      Window : Windows.Window_Ptr;
   end record;

   type Scene_Render_Job is new Jobs.Abstract_Job and Jobs.GPU_Job with record
      Render : Simulation.Render_Ptr;
      Scene  : Behaviors.Behavior_Array_Access;
      Camera : Cameras.Camera_Ptr;
   end record;

   type Finish_Render_Job is new Jobs.Abstract_Job and Jobs.GPU_Job with record
      Fence  : not null access Fences.Buffer_Fence;
      Window : Windows.Window_Ptr;
      Stop   : Simulation.Stop_Ptr;
   end record;

   overriding
   procedure Execute
     (Object  : Start_Render_Job;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr));

   overriding
   procedure Execute
     (Object  : Scene_Render_Job;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr));

   overriding
   procedure Execute
     (Object  : Finish_Render_Job;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr));

   -----------------------------------------------------------------------------
   --                              CONSTRUCTORS                               --
   -----------------------------------------------------------------------------

   function Create_Fixed_Update_Job
     (Scene     : not null Behaviors.Behavior_Array_Access;
      Time_Step : Time_Span;
      Count     : Natural) return Jobs.Parallel_Job_Ptr
   is (new Fixed_Update_Job'
     (Jobs.Abstract_Parallel_Job with
       Scene => Scene, Time_Step => Time_Step, Count => Count));

   function Create_Update_Job
     (Scene     : not null Behaviors.Behavior_Array_Access;
      Time_Step : Time_Span) return Jobs.Parallel_Job_Ptr
   is (new Update_Job'
     (Jobs.Abstract_Parallel_Job with Scene => Scene, Time_Step => Time_Step));

   function Create_After_Update_Job
     (Scene     : not null Behaviors.Behavior_Array_Access;
      Time_Step : Time_Span;
      Position  : Transforms.Vector4) return Jobs.Parallel_Job_Ptr
   is (new After_Update_Job'
     (Jobs.Abstract_Parallel_Job with
       Scene => Scene, Time_Step => Time_Step, View_Position => Position));

   function Create_Finished_Job
     (Scene     : not null Behaviors.Behavior_Array_Access;
      Time_Step : Time_Span;
      Position  : Transforms.Vector4) return Jobs.Job_Ptr
   is (new Finished_Fixed_Update_Job'
     (Jobs.Abstract_Job with
       Scene => Scene, Time_Step => Time_Step, View_Position => Position));

   -----------------------------------------------------------------------------
   --                              CONSTRUCTORS                               --
   -----------------------------------------------------------------------------

   function Create_Start_Render_Job
     (Fence  : not null access Fences.Buffer_Fence;
      Window : Windows.Window_Ptr) return Jobs.Job_Ptr
   is (new Start_Render_Job'(Jobs.Abstract_Job with
        Fence => Fence, Window => Window));

   function Create_Scene_Render_Job
     (Render : Simulation.Render_Ptr;
      Scene  : not null Behaviors.Behavior_Array_Access;
      Camera : Cameras.Camera_Ptr) return Jobs.Job_Ptr
   is (new Scene_Render_Job'(Jobs.Abstract_Job with
        Render => Render, Scene => Scene, Camera => Camera));

   function Create_Finish_Render_Job
     (Fence  : not null access Fences.Buffer_Fence;
      Window : Windows.Window_Ptr;
      Stop   : Simulation.Stop_Ptr) return Jobs.Job_Ptr
   is (new Finish_Render_Job'(Jobs.Abstract_Job with
        Fence => Fence, Window => Window, Stop => Stop));

   -----------------------------------------------------------------------------
   --                            EXECUTE PROCEDURES                           --
   -----------------------------------------------------------------------------

   overriding
   procedure Execute (Object : Fixed_Update_Job; From, To : Positive) is
      DT : constant Duration := To_Duration (Object.Time_Step);
   begin
      for Behavior of Object.Scene (From .. To) loop
         for Iteration in 1 .. Object.Count loop
            Behavior.Fixed_Update (DT);
         end loop;
      end loop;
   end Execute;

   overriding
   procedure Execute (Object : Update_Job; From, To : Positive) is
      DT : constant Duration := To_Duration (Object.Time_Step);
   begin
      for Behavior of Object.Scene (From .. To) loop
         Behavior.Update (DT);
      end loop;
   end Execute;

   overriding
   procedure Execute (Object : After_Update_Job; From, To : Positive) is
      DT : constant Duration := To_Duration (Object.Time_Step);
   begin
      for Behavior of Object.Scene (From .. To) loop
         Behavior.After_Update (DT, Object.View_Position);
      end loop;
   end Execute;

   overriding
   procedure Execute
     (Object  : Finished_Fixed_Update_Job;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr))
   is
      Update_Job : constant Jobs.Job_Ptr
        := Jobs.Parallelize (Create_Update_Job (Object.Scene, Object.Time_Step),
          Object.Scene'Length, 10);

      After_Update_Job : constant Jobs.Job_Ptr
        := Jobs.Parallelize (Create_After_Update_Job
             (Object.Scene, Object.Time_Step, Object.View_Position),
          Object.Scene'Length, 10);
   begin
      After_Update_Job.Set_Dependency (Update_Job);
      Enqueue (Update_Job);
   end Execute;

   -----------------------------------------------------------------------------
   --                            EXECUTE PROCEDURES                           --
   -----------------------------------------------------------------------------

   overriding
   procedure Execute
     (Object  : Start_Render_Job;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr)) is
   begin
      Object.Fence.Prepare_Index;
      Object.Window.Process_Input;
   end Execute;

   overriding
   procedure Execute
     (Object  : Scene_Render_Job;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr)) is
   begin
      Object.Render (Object.Scene, Object.Camera);
   end Execute;

   overriding
   procedure Execute
     (Object  : Finish_Render_Job;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr)) is
   begin
      Object.Fence.Advance_Index;
      Object.Window.Swap_Buffers;

      if Object.Window.Should_Close then
         Object.Stop.all;
      end if;
   end Execute;

end Orka.Simulation_Jobs;
