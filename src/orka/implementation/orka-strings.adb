--  SPDX-License-Identifier: Apache-2.0
--
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

with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;

package body Orka.Strings is

   package L renames Ada.Characters.Latin_1;
   package SF renames Ada.Strings.Fixed;

   function Trim (Value : String) return String is (SF.Trim (Value, Ada.Strings.Both));

   function Strip_Line_Term (Value : String) return String is
      Last_Index : Natural := Value'Last;
   begin
      for Index in reverse Value'Range loop
         exit when Value (Index) not in L.LF | L.CR;
         Last_Index := Last_Index - 1;
      end loop;
      return Value (Value'First .. Last_Index);
   end Strip_Line_Term;

   function Lines (Value : String) return Natural is
     (SF.Count (Strip_Line_Term (Value), "" & L.LF) + 1);

end Orka.Strings;
