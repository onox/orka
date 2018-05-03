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

   Future : Orka.Futures.Pointers.Pointer;
   Status : Orka.Futures.Status;

   use Ada.Real_Time;
   use Ada.Text_IO;

   T1, T2 : Time;

   package Boss renames Package_9_Jobs.Boss;
begin
   --  Graph: Job_0 --> Job_1 --> Job_4 (4 slices) --> Job_2
   Job_1.Set_Dependencies ((1 => Job_0));
   Job_4.Set_Dependencies ((1 => Job_1));
   Job_2.Set_Dependencies ((1 => Job_4));

   Package_9_Jobs.Boss.Queue.Enqueue (Job_0, Future);
   Put_Line ("References (2): " & Future.References'Image);

   T1 := Clock;
   select
      Future.Get.all.Wait_Until_Done (Status);
      T2 := Clock;
      Put_Line ("   Status: " & Status'Image);
      Put_Line ("   Time:  " & Duration'Image (1e3 * To_Duration (T2 - T1)) & " ms");
   or
      delay until Clock + Milliseconds (10);
      Put_Line ("   Time out: " & Future.Get.all.Current_Status'Image);
   end select;
   Put_Line ("References (1): " & Future.References'Image);

   Boss.Shutdown;

   Put_Line ("Queue:   " & Natural'Image (Boss.Queue.Length));
end Orka_Test.Test_9_Jobs;
