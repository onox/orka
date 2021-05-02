--  SPDX-License-Identifier: Apache-2.0
--
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
      View_Position : Behaviors.Vector4;
   end record;

   type Finished_Fixed_Update_Job is new Jobs.Abstract_Job with record
      Scene     : not null Behaviors.Behavior_Array_Access;
      Time_Step : Time_Span;
      View_Position : Behaviors.Vector4;
      Batch_Length : Positive;
   end record;

   overriding
   procedure Execute
     (Object   : Fixed_Update_Job;
      Context  : Jobs.Execution_Context'Class;
      From, To : Positive);

   overriding
   procedure Execute
     (Object   : Update_Job;
      Context  : Jobs.Execution_Context'Class;
      From, To : Positive);

   overriding
   procedure Execute
     (Object   : After_Update_Job;
      Context  : Jobs.Execution_Context'Class;
      From, To : Positive);

   overriding
   procedure Execute
     (Object  : Finished_Fixed_Update_Job;
      Context : Jobs.Execution_Context'Class);

   -----------------------------------------------------------------------------

   type Start_Render_Job is new Jobs.Abstract_Job and Jobs.GPU_Job with record
      Fence  : not null access Fences.Buffer_Fence;
   end record;

   type Scene_Render_Job is new Jobs.Abstract_Job and Jobs.GPU_Job with record
      Render : Simulation.Render_Ptr;
      Scene  : Behaviors.Behavior_Array_Access;
      Camera : Cameras.Camera_Ptr;
   end record;

   type Finish_Render_Job is new Jobs.Abstract_Job and Jobs.GPU_Job with record
      Fence  : not null access Fences.Buffer_Fence;
   end record;

   overriding
   procedure Execute
     (Object  : Start_Render_Job;
      Context : Jobs.Execution_Context'Class);

   overriding
   procedure Execute
     (Object  : Scene_Render_Job;
      Context : Jobs.Execution_Context'Class);

   overriding
   procedure Execute
     (Object  : Finish_Render_Job;
      Context : Jobs.Execution_Context'Class);

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
      Position  : Behaviors.Vector4) return Jobs.Parallel_Job_Ptr
   is (new After_Update_Job'
     (Jobs.Abstract_Parallel_Job with
       Scene => Scene, Time_Step => Time_Step, View_Position => Position));

   function Create_Finished_Job
     (Scene     : not null Behaviors.Behavior_Array_Access;
      Time_Step : Time_Span;
      Position  : Behaviors.Vector4;
      Batch_Length : Positive) return Jobs.Job_Ptr
   is (new Finished_Fixed_Update_Job'
     (Jobs.Abstract_Job with
       Scene => Scene, Time_Step => Time_Step, View_Position => Position,
       Batch_Length => Batch_Length));

   -----------------------------------------------------------------------------
   --                              CONSTRUCTORS                               --
   -----------------------------------------------------------------------------

   function Create_Start_Render_Job
     (Fence  : not null access Fences.Buffer_Fence) return Jobs.Job_Ptr
   is (new Start_Render_Job'(Jobs.Abstract_Job with Fence => Fence));

   function Create_Scene_Render_Job
     (Render : Simulation.Render_Ptr;
      Scene  : not null Behaviors.Behavior_Array_Access;
      Camera : Cameras.Camera_Ptr) return Jobs.Job_Ptr
   is (new Scene_Render_Job'(Jobs.Abstract_Job with
        Render => Render, Scene => Scene, Camera => Camera));

   function Create_Finish_Render_Job
     (Fence  : not null access Fences.Buffer_Fence) return Jobs.Job_Ptr
   is (new Finish_Render_Job'(Jobs.Abstract_Job with Fence => Fence));

   -----------------------------------------------------------------------------
   --                            EXECUTE PROCEDURES                           --
   -----------------------------------------------------------------------------

   overriding
   procedure Execute
     (Object   : Fixed_Update_Job;
      Context  : Jobs.Execution_Context'Class;
      From, To : Positive)
   is
      DT : constant Duration := To_Duration (Object.Time_Step);
   begin
      for Behavior of Object.Scene (From .. To) loop
         for Iteration in 1 .. Object.Count loop
            Behavior.Fixed_Update (DT);
         end loop;
      end loop;
   end Execute;

   overriding
   procedure Execute
     (Object   : Update_Job;
      Context  : Jobs.Execution_Context'Class;
      From, To : Positive)
   is
      DT : constant Duration := To_Duration (Object.Time_Step);
   begin
      for Behavior of Object.Scene (From .. To) loop
         Behavior.Update (DT);
      end loop;
   end Execute;

   overriding
   procedure Execute
     (Object   : After_Update_Job;
      Context  : Jobs.Execution_Context'Class;
      From, To : Positive)
   is
      DT : constant Duration := To_Duration (Object.Time_Step);
   begin
      for Behavior of Object.Scene (From .. To) loop
         Behavior.After_Update (DT, Object.View_Position);
      end loop;
   end Execute;

   function Clone_Update_Job
     (Job    : Jobs.Parallel_Job_Ptr;
      Length : Positive) return Jobs.Dependency_Array
   is
      Object : constant Update_Job := Update_Job (Job.all);
   begin
      return Result : constant Jobs.Dependency_Array (1 .. Length)
        := (others => new Update_Job'(Object));
   end Clone_Update_Job;

   function Clone_After_Update_Job
     (Job    : Jobs.Parallel_Job_Ptr;
      Length : Positive) return Jobs.Dependency_Array
   is
      Object : constant After_Update_Job := After_Update_Job (Job.all);
   begin
      return Result : constant Jobs.Dependency_Array (1 .. Length)
        := (others => new After_Update_Job'(Object));
   end Clone_After_Update_Job;

   function Clone_Fixed_Update_Job
     (Job    : Jobs.Parallel_Job_Ptr;
      Length : Positive) return Jobs.Dependency_Array
   is
      Object : constant Fixed_Update_Job := Fixed_Update_Job (Job.all);
   begin
      return Result : constant Jobs.Dependency_Array (1 .. Length)
        := (others => new Fixed_Update_Job'(Object));
   end Clone_Fixed_Update_Job;

   overriding
   procedure Execute
     (Object  : Finished_Fixed_Update_Job;
      Context : Jobs.Execution_Context'Class)
   is
      Update_Job : constant Jobs.Job_Ptr :=
        Jobs.Parallelize (Create_Update_Job (Object.Scene, Object.Time_Step),
          Clone_Update_Job'Access, Object.Scene'Length, Object.Batch_Length);

      After_Update_Job : constant Jobs.Job_Ptr :=
        Jobs.Parallelize (Create_After_Update_Job
          (Object.Scene, Object.Time_Step, Object.View_Position),
          Clone_After_Update_Job'Access, Object.Scene'Length, Object.Batch_Length);
   begin
      After_Update_Job.Set_Dependency (Update_Job);
      Context.Enqueue (Update_Job);
   end Execute;

   -----------------------------------------------------------------------------
   --                            EXECUTE PROCEDURES                           --
   -----------------------------------------------------------------------------

   overriding
   procedure Execute
     (Object  : Start_Render_Job;
      Context : Jobs.Execution_Context'Class)
   is
      Status : Fences.Fence_Status;
   begin
      Object.Fence.Prepare_Index (Status);
   end Execute;

   overriding
   procedure Execute
     (Object  : Scene_Render_Job;
      Context : Jobs.Execution_Context'Class) is
   begin
      Object.Render (Object.Scene, Object.Camera);
   end Execute;

   overriding
   procedure Execute
     (Object  : Finish_Render_Job;
      Context : Jobs.Execution_Context'Class) is
   begin
      Object.Fence.Advance_Index;
   end Execute;

end Orka.Simulation_Jobs;
