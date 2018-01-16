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

with System.Multiprocessors;

with Orka.Jobs.Workers;
with Orka.Jobs.Queues;

generic
   Queue_Size : Positive;
package Orka.Jobs.Boss is

   Queue : aliased Queues.Queue (Queue_Size);

   Number_Of_Workers : constant System.Multiprocessors.CPU;

   procedure Shutdown;

private

   package SM renames System.Multiprocessors;

   use type SM.CPU;

   Number_Of_Workers : constant SM.CPU := SM.Number_Of_CPUs - 1;

   package Workers is new Orka.Jobs.Workers
     ("Worker", Queue'Unchecked_Access, Number_Of_Workers);
   --  For n logical CPU's we spawn n - 1 workers (1 CPU is dedicated
   --  to rendering)
   --  TODO Or allocate Queue with new keyword?

   procedure Shutdown renames Workers.Shutdown;

end Orka.Jobs.Boss;
