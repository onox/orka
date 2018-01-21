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

with Orka.Jobs.Boss;
with Orka_Test.Package_9_Jobs;

procedure Orka_Test.Test_9_Jobs is
   package Boss is new Orka.Jobs.Boss (100);

   Job_0 : constant Orka.Jobs.Job_Ptr := new Package_9_Jobs.Test_Sequential_Job'
     (Orka.Jobs.Abstract_Job with ID => 0);
   Job_1 : constant Orka.Jobs.Job_Ptr := new Package_9_Jobs.Test_Sequential_Job'
     (Orka.Jobs.Abstract_Job with ID => 1);
   Job_2 : constant Orka.Jobs.Job_Ptr := new Package_9_Jobs.Test_Sequential_Job'
     (Orka.Jobs.Abstract_Job with ID => 2);

   Job_3 : constant Orka.Jobs.Parallel_Job_Ptr := new Package_9_Jobs.Test_Parallel_Job;
   Job_4 : constant Orka.Jobs.Job_Ptr := Orka.Jobs.Parallelize (Job_3, 24, 6);
begin
   --  Graph: Job_0 --> Job_1 --> Job_4 (4 slices) --> Job_2
   Job_1.Set_Dependencies ((1 => Job_0));
   Job_4.Set_Dependencies ((1 => Job_1));
   Job_2.Set_Dependencies ((1 => Job_4));

   Boss.Queue.Enqueue (Job_0);
end Orka_Test.Test_9_Jobs;
