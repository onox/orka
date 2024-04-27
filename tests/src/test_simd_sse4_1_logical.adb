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

with Orka.SIMD.SSE2.Integers;
with Orka.SIMD.SSE4_1.Integers.Logical;

package body Test_SIMD_SSE4_1_Logical is

   use Orka;
   use Orka.SIMD.SSE2.Integers;
   use Orka.SIMD.SSE4_1.Integers.Logical;

   use AUnit.Assertions;

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   function Convert is new Ada.Unchecked_Conversion (Unsigned_32, Integer_32);

   type Is_True_Array is array (Index_4D) of Boolean;

   function To_Mask (Mask : Is_True_Array) return m128i is
     [Convert (if Mask (X) then Unsigned_32'Last else 0),
      Convert (if Mask (Y) then Unsigned_32'Last else 0),
      Convert (if Mask (Z) then Unsigned_32'Last else 0),
      Convert (if Mask (W) then Unsigned_32'Last else 0)];

   procedure Test_Test_All_Zero (Object : in out Test) is
      Values : constant m128i :=
        [Convert (Unsigned_32'First),
         Convert (Unsigned_32'Last),
         Convert (0),
         Convert (0)];

      Mask_1 : constant m128i := To_Mask ([True, False, True, False]);
      Mask_2 : constant m128i := To_Mask ([True, True, True, True]);

      Result_1 : constant Boolean := Test_All_Zero (Values, Mask_1);
      Result_2 : constant Boolean := Test_All_Zero (Values, Mask_2);
   begin
      Assert (Result_1, "Vector not all zero");
      Assert (not Result_2, "Vector all zero");
   end Test_Test_All_Zero;

   procedure Test_Test_All_Ones (Object : in out Test) is
      Values : constant m128i :=
        [Convert (Unsigned_32'Last),
         Convert (0),
         Convert (Unsigned_32'Last),
         Convert (Unsigned_32'Last / 2)];

      Mask_1 : constant m128i := To_Mask ([True, False, True, False]);
      Mask_2 : constant m128i := To_Mask ([True, True, True, True]);

      Result_1 : constant Boolean := Test_All_Ones (Values, Mask_1);
      Result_2 : constant Boolean := Test_All_Ones (Values, Mask_2);
   begin
      Assert (Result_1, "Vector not all ones");
      Assert (not Result_2, "Vector all ones");
   end Test_Test_All_Ones;

   procedure Test_Test_Min_Ones_Zeros (Object : in out Test) is
      Values : constant m128i :=
        [Convert (Unsigned_32'Last),
         Convert (0),
         Convert (Unsigned_32'Last / 2),
         Convert (Unsigned_32'Last / 2)];

      Mask_1 : constant m128i := To_Mask ([False, False, True, False]);
      Mask_2 : constant m128i := To_Mask ([True, True, False, True]);
      Mask_3 : constant m128i := To_Mask ([False, True, False, False]);

      Result_1 : constant Boolean := Test_Mix_Ones_Zeros (Values, Mask_1);
      Result_2 : constant Boolean := Test_Mix_Ones_Zeros (Values, Mask_2);
      Result_3 : constant Boolean := Test_Mix_Ones_Zeros (Values, Mask_3);
   begin
      Assert (Result_1, "Vector not mix of ones and zeros");
      Assert (Result_2, "Vector not mix of ones and zeros");
      Assert (not Result_3, "Vector mix of ones and zeros");
   end Test_Test_Min_Ones_Zeros;

   ----------------------------------------------------------------------------

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(SIMD - SSE4.1 - Integers - Logical) ";
   begin
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Test_All_Zero function", Test_Test_All_Zero'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Test_All_Ones function", Test_Test_All_Ones'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Test_Mix_Ones_Zeros function", Test_Test_Min_Ones_Zeros'Access));

      return Test_Suite'Access;
   end Suite;

end Test_SIMD_SSE4_1_Logical;
