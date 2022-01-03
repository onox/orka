--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2022 onox <denkpadje@gmail.com>
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

with AUnit.Assertions;
with AUnit.Test_Caller;
with AUnit.Test_Fixtures;

with Orka.SIMD.SSE.Singles;
with Orka.SIMD.SSE2.Integers.Convert;

package body Test_SIMD_SSE2_Convert is

   use Orka;
   use Orka.SIMD.SSE.Singles;
   use Orka.SIMD.SSE2.Integers;

   use AUnit.Assertions;

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Convert_To_Integer (Object : in out Test) is
      Elements : constant m128 := (0.0, 0.5, -0.6, 1.2);

      Expected : constant m128i := (0, 0, -1, 1);
      Result   : constant m128i := Convert.Convert (Elements);
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Result (I), "Unexpected result at " & I'Image);
      end loop;
   end Test_Convert_To_Integer;

   procedure Test_Convert_To_Single (Object : in out Test) is
      Elements : constant m128i := (0, 1, -2**31, 2**31 - 1);

      Expected : constant m128 := (0.0, 1.0, -2.0**31, 2.0**31 - 1.0);
      Result   : constant m128 := Convert.Convert (Elements);
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Result (I), "Unexpected result at " & I'Image);
      end loop;
   end Test_Convert_To_Single;

   procedure Test_To_Unit_Floats (Object : in out Test) is
      function To_Integer_32 is new Ada.Unchecked_Conversion (Unsigned_32, Integer_32);

      Elements : constant m128i :=
        (To_Integer_32 (Unsigned_32'First),
         To_Integer_32 (Unsigned_32'Last / 4),
         To_Integer_32 (Unsigned_32'Last / 2),
         To_Integer_32 (Unsigned_32'Last));

      Expected : constant m128 := (0.0, 0.25, 0.5, 1.0);
      Result   : constant m128 := Convert.To_Unit_Floats (Elements);
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Result (I), "Unexpected result at " & I'Image &
           Expected (I)'Image & Result (I)'Image);
      end loop;
   end Test_To_Unit_Floats;

   ----------------------------------------------------------------------------

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(SIMD - SSE2 - Integers - Convert) ";
   begin
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Convert on singles", Test_Convert_To_Integer'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Convert on integers", Test_Convert_To_Single'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function To_Unit_Floats", Test_To_Unit_Floats'Access));

      return Test_Suite'Access;
   end Suite;

end Test_SIMD_SSE2_Convert;
