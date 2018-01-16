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

package body Orka.Jobs.Queues is

   protected body Queue is

      function Can_Schedule_Job (P : Priority) return Boolean is
      begin
         case P is
            when Normal =>
               return not Has_Dependents.Full;
            when Low =>
               return not No_Dependents.Full;
         end case;
      end Can_Schedule_Job;

      function Has_Jobs return Boolean is
        (not Has_Dependents.Empty or else not No_Dependents.Empty);

      entry Enqueue (Element : Job_Ptr) when True is
      begin
         --  Prioritize jobs that have no dependencies themselves but
         --  are a dependency of some other job.
         if Element.Dependent /= Null_Job then
            requeue Enqueue_Job (Normal);
         else
            requeue Enqueue_Job (Low);
         end if;
      end Enqueue;

      entry Enqueue_Job (for P in Priority) (Element : Job_Ptr) when Can_Schedule_Job (P) is
      begin
         case P is
            when Normal =>
               Has_Dependents.Add_Last (Element);
            when Low =>
               No_Dependents.Add_Last (Element);
         end case;
      end Enqueue_Job;

      entry Dequeue (Element : out Job_Ptr; Stop : out Boolean) when Should_Stop or Has_Jobs is
      begin
         Stop := Should_Stop;
         if Should_Stop then
            return;
         end if;

         --  Prioritize jobs that have no dependencies themselves but
         --  are a dependency of some other job.
         if not Has_Dependents.Empty then
            Element := Has_Dependents.Remove_First;
         elsif not No_Dependents.Empty then
            Element := No_Dependents.Remove_First;
         end if;
      end Dequeue;

      procedure Shutdown is
      begin
         Should_Stop := True;
      end Shutdown;

   end Queue;

end Orka.Jobs.Queues;
