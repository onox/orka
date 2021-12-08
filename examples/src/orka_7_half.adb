--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2016 onox <denkpadje@gmail.com>
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

with Orka.OS;
with Orka.Types;

procedure Orka_7_Half is
   use Orka;

   Numbers : constant Float_32_Array
     := (0.0, 0.5, -0.5, 1.0, -1.0, 0.1, -0.1, 0.0, 0.1234, -0.123456,
         10.1234, 20.1234, 50.1234, 100.1234, 1000.1234);

   Half_Numbers   : Float_16_Array (Numbers'Range);
   Single_Numbers : Float_32_Array (Half_Numbers'Range);
begin
   Orka.Types.Convert (Numbers, Half_Numbers);
   Orka.Types.Convert (Half_Numbers, Single_Numbers);

   for Number of Numbers loop
      Orka.OS.Put_Line (Float_32'Image (Number));
   end loop;
   Orka.OS.Put_Line ("------------");
   for Number of Single_Numbers loop
      Orka.OS.Put_Line (Float_32'Image (Number));
   end loop;
end Orka_7_Half;
