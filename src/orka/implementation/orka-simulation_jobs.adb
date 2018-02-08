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

   function Create_Fixed_Update_Job
     (Scene     : not null Behaviors.Behavior_Array_Access;
      Time_Step : Time_Span;
      Count     : Natural) return Jobs.Parallel_Job_Ptr
   is (new Fixed_Update_Job'
     (Jobs.Slice_Job with Scene => Scene, Time_Step => Time_Step, Count => Count));

   function Create_Update_Job
     (Scene     : not null Behaviors.Behavior_Array_Access;
      Time_Step : Time_Span) return Jobs.Parallel_Job_Ptr
   is (new Update_Job'
     (Jobs.Slice_Job with Scene => Scene, Time_Step => Time_Step));

   function Create_After_Update_Job
     (Scene     : not null Behaviors.Behavior_Array_Access;
      Time_Step : Time_Span;
      Position  : Transforms.Vector4) return Jobs.Parallel_Job_Ptr
   is (new After_Update_Job'
     (Jobs.Slice_Job with Scene => Scene, Time_Step => Time_Step, View_Position => Position));

   function Create_Finished_Job
     (Scene     : not null Behaviors.Behavior_Array_Access;
      Time_Step : Time_Span;
      Position  : Transforms.Vector4) return Jobs.Job_Ptr
   is (new Finished_Fixed_Update_Job'
     (Jobs.Abstract_Job with Scene => Scene, Time_Step => Time_Step, View_Position => Position));

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
      After_Update_Job.Set_Dependencies ((1 => Update_Job));
      Enqueue (Update_Job);
   end Execute;

end Orka.Simulation_Jobs;
