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

with Orka.Containers.Ring_Buffers;
with Orka.Futures.Slots;

generic
   Maximum_Graphs : Positive;
   --  Maximum number of separate job graphs

   Capacity : Positive;
package Orka.Jobs.Queues is

   type Executor_Kind is (CPU, GPU);

   type Pair is record
      Job    : Job_Ptr := Null_Job;
      Future : Futures.Pointers.Mutable_Pointer;
   end record;

   package Buffers is new Orka.Containers.Ring_Buffers (Pair);

   type Buffer_Array is array (Executor_Kind) of Buffers.Buffer (Capacity);

   protected type Queue is
      entry Enqueue (Element : Job_Ptr; Future : in out Futures.Pointers.Mutable_Pointer);
      --  with Pre => not Element.Has_Dependencies
      --     and then Element.all not in Parallel_Job'Class
      --     and then Element /= Null_Job

      entry Dequeue (Executor_Kind)
        (Element : out Pair; Stop : out Boolean)
      with Post => Stop or else not Element.Job.Has_Dependencies;

      procedure Shutdown;

      function Length (Kind : Executor_Kind) return Natural;
   private
      entry Enqueue_Job (Executor_Kind)
        (Element : Job_Ptr; Future : in out Futures.Pointers.Mutable_Pointer);

      Buffers     : Buffer_Array;
      Should_Stop : Boolean := False;
   end Queue;

   type Queue_Ptr is not null access all Queue;

   -----------------------------------------------------------------------------

   package Slots is new Orka.Futures.Slots (Count => Maximum_Graphs);

end Orka.Jobs.Queues;
