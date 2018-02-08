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

package Orka_Test.Package_9_Jobs is

   package Boss is new Orka.Jobs.Boss (4, 1, 4);

   type Test_Parallel_Job is new Orka.Jobs.Slice_Job with null record;

   overriding
   procedure Execute (Object : Test_Parallel_Job; From, To : Positive);

   type Test_Sequential_Job is new Orka.Jobs.Abstract_Job with record
      ID : Natural;
   end record;

   overriding
   procedure Execute
     (Object  : Test_Sequential_Job;
      Enqueue : not null access procedure (Element : Orka.Jobs.Job_Ptr));

end Orka_Test.Package_9_Jobs;
