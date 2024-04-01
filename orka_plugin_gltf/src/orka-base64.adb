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

with Ada.Unchecked_Conversion;

package body Orka.Base64 is

   type Input_Value is mod 2**6
     with Size => 6;

   type Output_Value is mod 2**8
     with Size => 8;

   type Input_Group is record
      V1, V2, V3, V4 : Input_Value;
   end record;

   type Output_Group is record
      B1, B2, B3 : Output_Value;
   end record;

   for Input_Group use record
      V4 at 0 range 0 .. 5;
      V3 at 0 range 6 .. 11;
      V2 at 0 range 12 .. 17;
      V1 at 0 range 18 .. 23;
   end record;

   for Output_Group use record
      B3 at 0 range 0 .. 7;
      B2 at 0 range 8 .. 15;
      B1 at 0 range 16 .. 23;
   end record;

   Alphabet : constant array (Character) of Input_Value :=
     ['A' =>  0, 'B' =>  1, 'C' =>  2, 'D' =>  3, 'E' =>  4, 'F' =>  5,
      'G' =>  6, 'H' =>  7, 'I' =>  8, 'J' =>  9, 'K' => 10, 'L' => 11,
      'M' => 12, 'N' => 13, 'O' => 14, 'P' => 15, 'Q' => 16, 'R' => 17,
      'S' => 18, 'T' => 19, 'U' => 20, 'V' => 21, 'W' => 22, 'X' => 23,
      'Y' => 24, 'Z' => 25, 'a' => 26, 'b' => 27, 'c' => 28, 'd' => 29,
      'e' => 30, 'f' => 31, 'g' => 32, 'h' => 33, 'i' => 34, 'j' => 35,
      'k' => 36, 'l' => 37, 'm' => 38, 'n' => 39, 'o' => 40, 'p' => 41,
      'q' => 42, 'r' => 43, 's' => 44, 't' => 45, 'u' => 46, 'v' => 47,
      'w' => 48, 'x' => 49, 'y' => 50, 'z' => 51, '0' => 52, '1' => 53,
      '2' => 54, '3' => 55, '4' => 56, '5' => 57, '6' => 58, '7' => 59,
      '8' => 60, '9' => 61, '+' => 62, '/' => 63, others => 0];

   function Convert_Group is new Ada.Unchecked_Conversion
     (Source => Input_Group, Target => Output_Group);

   function Decode (Input : String) return Stream_Element_Array is
      G1 : Input_Group;
      G2 : Output_Group;

      Groups : constant Positive := Input'Length / 4;
      Bytes  : Positive := Groups * 3;
   begin
      if Input'Length mod 4 /= 0 then
         raise Encoding_Error with "Length not a multiple of 4";
      end if;

      if (for some C of Input =>
        C not in 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '+' | '/' | '=')
      then
         raise Encoding_Error with "Illegal character";
      end if;

      declare
         Output : Stream_Element_Array (1 .. Stream_Element_Offset (Bytes));
      begin
         for Index in 0 .. Groups - 1 loop
            G1.V1 := Alphabet (Input (Input'First + Index * 4 + 0));
            G1.V2 := Alphabet (Input (Input'First + Index * 4 + 1));
            G1.V3 := Alphabet (Input (Input'First + Index * 4 + 2));
            G1.V4 := Alphabet (Input (Input'First + Index * 4 + 3));

            G2 := Convert_Group (G1);

            Output (Stream_Element_Offset (Index) * 3 + 1) := Stream_Element (G2.B1);
            Output (Stream_Element_Offset (Index) * 3 + 2) := Stream_Element (G2.B2);
            Output (Stream_Element_Offset (Index) * 3 + 3) := Stream_Element (G2.B3);
         end loop;

         if Input (Input'Last - 1 .. Input'Last) = "==" then
            Bytes := Bytes - 2;
         elsif Input (Input'Last) = '=' then
            Bytes := Bytes - 1;
         end if;

         return Output (1 .. Stream_Element_Offset (Bytes));
      end;
   end Decode;

end Orka.Base64;
