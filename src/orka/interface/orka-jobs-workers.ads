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

with Orka.Jobs.Executors;

generic
   with package Executors is new Orka.Jobs.Executors (<>);

   Queue : Executors.Queues.Queue_Ptr;

   Task_Name : String;
   --  Name of a worker task in system's process viewer

   Count : System.Multiprocessors.CPU;
   --  Number of workers to spawn
package Orka.Jobs.Workers is

   procedure Shutdown;
   --  Ask all workers to stop dequeuing jobs and terminate

private

   type Worker;

   task type Worker_Task (Data : not null access constant Worker);

   type Worker is limited record
      ID : Positive;
      T  : Worker_Task (Worker'Access);
   end record;

   type Worker_Array is array (Positive range <>) of Worker;

   function Make_Workers return Worker_Array;

   Workers : constant Worker_Array := Make_Workers;

end Orka.Jobs.Workers;
