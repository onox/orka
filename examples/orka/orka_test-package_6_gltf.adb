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

package body Orka_Test.Package_6_glTF is

   overriding
   procedure Execute
     (Object  : Create_Instance_Job;
      Enqueue : not null access procedure (Element : Orka.Jobs.Job_Ptr)) is
   begin
      Add_Resource (Object.Model.Create_Instance ((0.0, 0.0, 0.0, 1.0)));
   end Execute;

end Orka_Test.Package_6_glTF;
