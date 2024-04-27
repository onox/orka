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

with Orka.SIMD.SSE2.Integers.Logical;

package body Test_SIMD_SSE2_Logical is

   use Orka;
   use Orka.SIMD.SSE2.Integers;
   use Orka.SIMD.SSE2.Integers.Logical;

   use AUnit.Assertions;

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   function Convert is new Ada.Unchecked_Conversion (Integer_32, Unsigned_32);
   function Convert is new Ada.Unchecked_Conversion (Unsigned_32, Integer_32);

   function Get_And_Not (Left, Right : Integer_32) return Integer_32 is
     (Convert ((not Convert (Left)) and Convert (Right)));

   function Get_And (Left, Right : Integer_32) return Integer_32 is
     (Convert (Convert (Left) and Convert (Right)));

   function Get_Or (Left, Right : Integer_32) return Integer_32 is
     (Convert (Convert (Left) or Convert (Right)));

   function Get_Xor (Left, Right : Integer_32) return Integer_32 is
     (Convert (Convert (Left) xor Convert (Right)));

   procedure Test_And_Not (Object : in out Test) is
      Left  : constant m128i := [0, 1, 2**16, 2**30];
      Right : constant m128i := [0, 0, 2**16, 2**30 - 1];

      Expected : constant m128i :=
        [Get_And_Not (Left (X), Right (X)),
         Get_And_Not (Left (Y), Right (Y)),
         Get_And_Not (Left (Z), Right (Z)),
         Get_And_Not (Left (W), Right (W))];
      Result   : constant m128i := And_Not (Left, Right);
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Result (I), "Unexpected Integer at " & I'Image);
      end loop;
   end Test_And_Not;

   procedure Test_And (Object : in out Test) is
      Left  : constant m128i := [0, 1, 2**16, 2**30];
      Right : constant m128i := [0, 0, 2**16, 2**30 - 1];

      Expected : constant m128i :=
        [Get_And (Left (X), Right (X)),
         Get_And (Left (Y), Right (Y)),
         Get_And (Left (Z), Right (Z)),
         Get_And (Left (W), Right (W))];
      Result   : constant m128i := Left and Right;
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Result (I), "Unexpected Integer at " & I'Image);
      end loop;
   end Test_And;

   procedure Test_Or (Object : in out Test) is
      Left  : constant m128i := [0, 1, 2**16, 2**30];
      Right : constant m128i := [0, 0, 2**16, 2**30 - 1];

      Expected : constant m128i :=
        [Get_Or (Left (X), Right (X)),
         Get_Or (Left (Y), Right (Y)),
         Get_Or (Left (Z), Right (Z)),
         Get_Or (Left (W), Right (W))];
      Result   : constant m128i := Left or Right;
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Result (I), "Unexpected Integer at " & I'Image);
      end loop;
   end Test_Or;

   procedure Test_Xor (Object : in out Test) is
      Left  : constant m128i := [0, 1, 2**16, 2**30];
      Right : constant m128i := [0, 0, 2**16, 2**30 - 1];

      Expected : constant m128i :=
        [Get_Xor (Left (X), Right (X)),
         Get_Xor (Left (Y), Right (Y)),
         Get_Xor (Left (Z), Right (Z)),
         Get_Xor (Left (W), Right (W))];
      Result   : constant m128i := Left xor Right;
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Result (I), "Unexpected Integer at " & I'Image);
      end loop;
   end Test_Xor;

   ----------------------------------------------------------------------------

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(SIMD - SSE2 - Integers - Logical) ";
   begin
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function And_Not", Test_And_Not'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test 'and' operator", Test_And'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test 'or' operator", Test_Or'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test 'xor' operator", Test_Xor'Access));

      return Test_Suite'Access;
   end Suite;

end Test_SIMD_SSE2_Logical;
