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

with AUnit.Assertions;
with AUnit.Test_Caller;
with AUnit.Test_Fixtures;

with Orka.SIMD.SSE2.Integers.Arithmetic;
with Orka.SIMD.SSE2.Integers.Compare;

package body Test_SIMD_SSE2_Compare is

   use Orka;
   use Orka.SIMD.SSE2.Integers;
   use Orka.SIMD.SSE2.Integers.Compare;

   type Is_True_Array is array (Index_4D) of Boolean;

   use AUnit.Assertions;

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   function Is_True (Elements : m128i; Position : Index_4D) return Boolean is
     (Elements (Position) /= 0);

   procedure Test_Equal (Object : in out Test) is
      Left  : constant m128i := (-0, 1, 1, 2**31 - 1);
      Right : constant m128i := (0, 0, 2, 2**31 - 1);

      Expected : constant Is_True_Array := (True, False, False, True);
      Result   : constant m128i := Left = Right;
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Is_True (Result, I), "Unexpected result at " & I'Image);
      end loop;
   end Test_Equal;

   procedure Test_Greater_Than (Object : in out Test) is
      Left  : constant m128i := (-0, 1, 2**31 - 1, 0);
      Right : constant m128i := (0, 0, -2**31, 2**31 - 1);

      Expected : constant Is_True_Array := (False, True, True, False);
      Result   : constant m128i := Left > Right;
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Is_True (Result, I), "Unexpected result at " & I'Image);
      end loop;
   end Test_Greater_Than;

   procedure Test_Less_Than (Object : in out Test) is
      Left  : constant m128i := (-0, 1, 2**31 - 1, 0);
      Right : constant m128i := (0, 0, -2**31, 2**31 - 1);

      Expected : constant Is_True_Array := (False, False, False, True);
      Result   : constant m128i := Left < Right;
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Is_True (Result, I), "Unexpected result at " & I'Image);
      end loop;
   end Test_Less_Than;

   ----------------------------------------------------------------------------

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(SIMD - SSE2 - Integers - Compare) ";
   begin
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '=' operator", Test_Equal'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '>' operator", Test_Greater_Than'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '<' operator", Test_Less_Than'Access));

      return Test_Suite'Access;
   end Suite;

end Test_SIMD_SSE2_Compare;
