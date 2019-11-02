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

with Ada.Text_IO;

package body Orka_Test.Package_9_Jobs is

   overriding
   procedure Execute
     (Object  : Test_Sequential_Job;
      Context : Orka.Jobs.Execution_Context'Class) is
   begin
      Ada.Text_IO.Put_Line ("Sequential job " & Object.ID'Image);
   end Execute;

   overriding
   procedure Execute
     (Object   : Test_Parallel_Job;
      Context  : Orka.Jobs.Execution_Context'Class;
      From, To : Positive) is
   begin
      Ada.Text_IO.Put_Line ("Parallel job (" & From'Image & " .. " & To'Image & ")");
   end Execute;

end Orka_Test.Package_9_Jobs;
