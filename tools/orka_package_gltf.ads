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
with Orka.Jobs.Boss;
with Orka.Resources.Loader;
with Orka.Resources.Models;

package Orka_Package_glTF is

   package Boss is new Orka.Jobs.Boss
     (Maximum_Queued_Jobs     => 50,
      Maximum_Job_Graphs      => 10,
      Maximum_Enqueued_By_Job => 32);

   package Loader is new Orka.Resources.Loader
     (Boss.Queues, Boss.Queue'Unchecked_Access, Maximum_Requests => 10);

   Add_Resource : access procedure (Object : Orka.Behaviors.Behavior_Ptr);

   type Create_Instance_Job is new Orka.Jobs.Abstract_Job and Orka.Jobs.GPU_Job with record
      Model  : Orka.Resources.Models.Model_Ptr;
      Culler : Orka.Culling.Culler_Ptr;
   end record;

   overriding
   procedure Execute
     (Object  : Create_Instance_Job;
      Enqueue : not null access procedure (Element : Orka.Jobs.Job_Ptr));

end Orka_Package_glTF;
