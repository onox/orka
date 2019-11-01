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

with Orka.Behaviors;
with Orka.Culling;
with Orka.Jobs.System;
with Orka.Resources.Loader;
with Orka.Resources.Models;

package Orka_Package_glTF is

   package Job_System is new Orka.Jobs.System
     (Maximum_Queued_Jobs => 16,
      Maximum_Job_Graphs  => 4);

   package Loader is new Orka.Resources.Loader
     (Job_System.Queues, Job_System.Queue'Unchecked_Access, Maximum_Requests => 10);

   -----------------------------------------------------------------------------

   type Create_Group_Job is new Orka.Jobs.Abstract_Job and Orka.Jobs.GPU_Job with record
      Model  : Orka.Resources.Models.Model_Ptr;
      Culler : Orka.Culling.Culler_Ptr;
      Group  : access Orka.Resources.Models.Group_Access;
   end record;

   overriding
   procedure Execute
     (Object  : Create_Group_Job;
      Context : Orka.Jobs.Execution_Context'Class);

   -----------------------------------------------------------------------------

   type No_Behavior is new Orka.Resources.Models.Model_Instance with record
      Position : Orka.Behaviors.Transforms.Vector4 := Orka.Behaviors.Null_Behavior.Position;
   end record;

   overriding
   function Position (Object : No_Behavior) return Orka.Behaviors.Transforms.Vector4 is
     (Object.Position);

   overriding
   procedure After_Update
     (Object        : in out No_Behavior;
      Delta_Time    : Duration;
      View_Position : Orka.Behaviors.Transforms.Vector4);

end Orka_Package_glTF;
