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

with Ada.Real_Time;

with Orka.Behaviors;
with Orka.Jobs;
with Orka.Transforms.Singles.Vectors;

private package Orka.Simulation_Jobs is

   type Fixed_Update_Job is new Jobs.Abstract_Parallel_Job with private;

   overriding
   procedure Execute (Object : Fixed_Update_Job; From, To : Positive);

   type Update_Job is new Jobs.Abstract_Parallel_Job with private;

   overriding
   procedure Execute (Object : Update_Job; From, To : Positive);

   type After_Update_Job is new Jobs.Abstract_Parallel_Job with private;

   overriding
   procedure Execute (Object : After_Update_Job; From, To : Positive);

   type Finished_Fixed_Update_Job is new Jobs.Abstract_Job with private;

   overriding
   procedure Execute
     (Object  : Finished_Fixed_Update_Job;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr));

   -----------------------------------------------------------------------------

   use Ada.Real_Time;

   package Transforms renames Orka.Transforms.Singles.Vectors;

   function Create_Fixed_Update_Job
     (Scene     : not null Behaviors.Behavior_Array_Access;
      Time_Step : Time_Span;
      Count     : Natural) return Jobs.Parallel_Job_Ptr;

   function Create_Finished_Job
     (Scene     : not null Behaviors.Behavior_Array_Access;
      Time_Step : Time_Span;
      Position  : Transforms.Vector4) return Jobs.Job_Ptr;

private

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

end Orka.Simulation_Jobs;
