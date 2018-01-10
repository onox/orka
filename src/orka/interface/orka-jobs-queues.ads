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

package Orka.Jobs.Queues is
   pragma Preelaborate;

   type Priority is (Normal, Low);

   package Buffers is new Orka.Containers.Ring_Buffers (Job_Ptr, Get_Null_Job);

   protected type Queue (Capacity : Positive) is
      entry Enqueue (Element : Job_Ptr);
      --  TODO Pre => not Element.Has_Dependencies
      --         and then Element.all not in Parallel_Job'Class
      --         and then Element /= Null_Job

      entry Dequeue (Element : out Job_Ptr);
      --  TODO Post => not Element.Has_Dependencies
   private
      entry Enqueue_Job (Priority) (Element : Job_Ptr);

      No_Dependents, Has_Dependents : Buffers.Buffer (Capacity);
   end Queue;

   type Queue_Ptr is not null access all Queue;

end Orka.Jobs.Queues;
