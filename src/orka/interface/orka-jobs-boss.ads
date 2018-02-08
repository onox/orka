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
   Maximum_Queued_Jobs : Positive;
   --  Maximum number of jobs that can be enqueued
   --
   --  Should be less than the largest width (number of jobs at a
   --  particular level) of any job graph.

   Maximum_Job_Graphs  : Positive;
   --  Maximum number of separate job graphs
   --
   --  For each job graph, a Future object is acquired. The number of
   --  concurrent acquired objects is bounded by this number.

   Maximum_Enqueued_By_Job : Positive;
   --  Maximum number of extra jobs that a job can enqueue
package Orka.Jobs.Boss is

   package Queues is new Jobs.Queues (Maximum_Graphs => Maximum_Job_Graphs);

   Queue : aliased Queues.Queue (Maximum_Queued_Jobs);

   Number_Of_Workers : constant System.Multiprocessors.CPU;

   procedure Shutdown;

private

   package SM renames System.Multiprocessors;

   use type SM.CPU;

   Number_Of_Workers : constant SM.CPU := SM.Number_Of_CPUs - 1;
   --  For n logical CPU's we spawn n - 1 workers (1 CPU is dedicated
   --  to rendering)

   package Workers is new Orka.Jobs.Workers
     (Queues, Queue'Access, "Worker", Number_Of_Workers, Maximum_Enqueued_By_Job);

   procedure Shutdown renames Workers.Shutdown;

end Orka.Jobs.Boss;
