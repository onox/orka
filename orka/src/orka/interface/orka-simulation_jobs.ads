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

with Ada.Real_Time;

with Orka.Behaviors;
with Orka.Cameras;
with Orka.Jobs;
with Orka.Rendering.Fences;
with Orka.Simulation;
with Orka.Transforms.Singles.Vectors;
with Orka.Windows;

private package Orka.Simulation_Jobs is

   use Ada.Real_Time;

   type Buffer_Region_Type is mod 4;

   package Transforms renames Orka.Transforms.Singles.Vectors;

   package Fences is new Orka.Rendering.Fences (Buffer_Region_Type);

   -----------------------------------------------------------------------------

   function Clone_Fixed_Update_Job
     (Job    : Jobs.Parallel_Job_Ptr;
      Length : Positive) return Jobs.Dependency_Array;

   function Create_Fixed_Update_Job
     (Scene     : not null Behaviors.Behavior_Array_Access;
      Time_Step : Time_Span;
      Count     : Natural) return Jobs.Parallel_Job_Ptr;

   function Create_Finished_Job
     (Scene     : not null Behaviors.Behavior_Array_Access;
      Time_Step : Time_Span;
      Position  : Behaviors.Vector4;
      Batch_Length : Positive) return Jobs.Job_Ptr;

   -----------------------------------------------------------------------------

   function Create_Start_Render_Job
     (Fence  : not null access Fences.Buffer_Fence;
      Window : Windows.Window_Ptr) return Jobs.Job_Ptr;

   function Create_Scene_Render_Job
     (Render : Simulation.Render_Ptr;
      Scene  : not null Behaviors.Behavior_Array_Access;
      Camera : Cameras.Camera_Ptr) return Jobs.Job_Ptr;

   function Create_Finish_Render_Job
     (Fence  : not null access Fences.Buffer_Fence;
      Window : Windows.Window_Ptr;
      Stop   : Simulation.Stop_Ptr) return Jobs.Job_Ptr;

end Orka.Simulation_Jobs;
