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

with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Text_IO;

with Orka.Futures;
with Orka.Jobs;
with Orka_Test.Package_9_Jobs;

procedure Orka_Test.Test_9_Jobs is
   Job_0 : constant Orka.Jobs.Job_Ptr := new Package_9_Jobs.Test_Sequential_Job'
     (Orka.Jobs.Abstract_Job with ID => 0);
   Job_1 : constant Orka.Jobs.Job_Ptr := new Package_9_Jobs.Test_Sequential_Job'
     (Orka.Jobs.Abstract_Job with ID => 1);
   Job_2 : constant Orka.Jobs.Job_Ptr := new Package_9_Jobs.Test_Sequential_Job'
     (Orka.Jobs.Abstract_Job with ID => 2);

   Job_3 : constant Orka.Jobs.Parallel_Job_Ptr := new Package_9_Jobs.Test_Parallel_Job;
   Job_4 : constant Orka.Jobs.Job_Ptr := Orka.Jobs.Parallelize (Job_3, 24, 6);

   Handle : Orka.Futures.Pointers.Mutable_Pointer;
   Status : Orka.Futures.Status;

   use Ada.Exceptions;
   use Ada.Real_Time;
   use Ada.Text_IO;

   T1, T2 : Time;

   package Job_System renames Package_9_Jobs.Job_System;
begin
   --  Graph: Job_0 --> Job_1 --> Job_4 (4 slices) --> Job_2
   Orka.Jobs.Chain ((Job_0, Job_1, Job_4, Job_2));

   Job_System.Queue.Enqueue (Job_0, Handle);
   Put_Line ("References (2): " & Handle.References'Image);

   T1 := Clock;
   declare
      Reference : constant Orka.Futures.Pointers.Reference := Handle.Get;
      Future    : constant Orka.Futures.Future_Access      := Reference.Value;
   begin
      select
         Future.Wait_Until_Done (Status);
         T2 := Clock;
         Put_Line ("   Status: " & Status'Image);
         Put_Line ("   Time:  " & Duration'Image (1e3 * To_Duration (T2 - T1)) & " ms");
      or
         delay until T1 + Milliseconds (10);
         Put_Line ("   Time out: " & Reference.Current_Status'Image);
      end select;
   exception
      when Error : others =>
         Put_Line ("Error: " & Exception_Information (Error));
   end;
   Put_Line ("References (1): " & Handle.References'Image);

   Job_System.Shutdown;

   Put_Line ("CPU Queue:   " & Job_System.Queue.Length (Job_System.Queues.CPU)'Image);
   Put_Line ("GPU Queue:   " & Job_System.Queue.Length (Job_System.Queues.GPU)'Image);
end Orka_Test.Test_9_Jobs;
