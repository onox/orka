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
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body Orka.Strings is

   package L1 renames Ada.Characters.Latin_1;
   package SF renames Ada.Strings.Fixed;

   package UTF renames Ada.Strings.UTF_Encoding;

   function Unicode (Value : Wide_Wide_String) return UTF.UTF_8_String is
     (UTF.Wide_Wide_Strings.Encode (Value));

   function Trim (Value : String) return String is (SF.Trim (Value, Ada.Strings.Both));

   function Strip_Line_Term (Value : String) return String is
      Last_Index : Natural := Value'Last;
   begin
      for Index in reverse Value'Range loop
         exit when Value (Index) not in L1.LF | L1.CR;
         Last_Index := Last_Index - 1;
      end loop;
      return Value (Value'First .. Last_Index);
   end Strip_Line_Term;

   function Lines (Value : String) return Positive is
     (SF.Count (Strip_Line_Term (Value), "" & L1.LF) + 1);

   function Split
     (Value     : String;
      Separator : String  := " ";
      Maximum   : Natural := 0) return String_List
   is
      Lines : constant String := Strip_Line_Term (Value);

      Index : Positive := Lines'First;

      Auto_Count : constant Positive := SF.Count (Lines, Separator) + 1;
      Count : constant Positive :=
        (if Maximum > 0 then Positive'Min (Maximum, Auto_Count) else Auto_Count);
   begin
      return Result : String_List (1 .. Count) do
         for I in Result'First .. Result'Last - 1 loop
            declare
               Next_Index : constant Positive := SF.Index (Lines, Separator, Index);
            begin
               Result (I) := SU.To_Unbounded_String (Lines (Index .. Next_Index - 1));
               Index := Next_Index + Separator'Length;
            end;
         end loop;
         Result (Result'Last) := SU.To_Unbounded_String (Lines (Index .. Lines'Last));
      end return;
   end Split;

   function Join (List : String_List; Separator : String) return String is
      Result : SU.Unbounded_String;
   begin
      for Index in List'First .. List'Last - 1 loop
         SU.Append (Result, List (Index));
         SU.Append (Result, Separator);
      end loop;
      SU.Append (Result, List (List'Last));
      return +Result;
   end Join;

end Orka.Strings;
