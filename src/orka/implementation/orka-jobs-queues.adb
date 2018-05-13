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

   procedure Release_Future (Value : in out Futures.Future_Access) is
   begin
      Slots.Manager.Release (Value);
   end Release_Future;

   protected body Queue is

      function Can_Schedule_Job (Kind : Executor_Kind) return Boolean is
        (not Buffers (Kind).Full);

      function Has_Jobs (Kind : Executor_Kind) return Boolean is
        (not Buffers (Kind).Empty);

      entry Enqueue
        (Element : Job_Ptr;
         Future  : in out Futures.Pointers.Mutable_Pointer) when True is
      begin
         if Element.all in GPU_Job'Class then
            requeue Enqueue_Job (GPU);
         else
            requeue Enqueue_Job (CPU);
         end if;
      end Enqueue;

      entry Enqueue_Job (for Kind in Executor_Kind)
        (Element : Job_Ptr;
         Future  : in out Futures.Pointers.Mutable_Pointer) when Can_Schedule_Job (Kind) is
      begin
         if Future.Is_Null then
            declare
               Slot : Futures.Future_Access;
            begin
               select
                  Slots.Manager.Acquire (Slot);
                  Future.Set (Slot, Release_Future'Unrestricted_Access);
               else
                  raise Program_Error with Executor_Kind'Image (Kind) & " queue full";
               end select;
            end;
         end if;

         Buffers (Kind).Add_Last ((Job => Element, Future => Future));
      end Enqueue_Job;

      entry Dequeue (for Kind in Executor_Kind)
        (Element : out Pair;
         Stop    : out Boolean) when Should_Stop or else Has_Jobs (Kind) is
      begin
         Stop := Should_Stop;
         if Should_Stop then
            return;
         end if;

         Element := Buffers (Kind).Remove_First;
      end Dequeue;

      procedure Shutdown is
      begin
         Slots.Manager.Shutdown;
         Should_Stop := True;
      end Shutdown;

      function Length (Kind : Executor_Kind) return Natural is
        (Buffers (Kind).Length);
   end Queue;

end Orka.Jobs.Queues;
