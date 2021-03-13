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

with Ahven; use Ahven;

with Orka.SIMD.AVX.Doubles;
with Orka.SIMD.AVX2.Doubles.Swizzle;

package body Test_SIMD_AVX2_Swizzle is

   use Orka;
   use Orka.SIMD;
   use Orka.SIMD.AVX.Doubles;
   use Orka.SIMD.AVX2.Doubles.Swizzle;

   overriding
   procedure Initialize (T : in out Test) is
   begin
      T.Set_Name ("Swizzle");

      T.Add_Test_Routine (Test_Permute'Access, "Test Permute function");
   end Initialize;
 
   procedure Test_Permute is
      Elements : constant m256d := (1.0, 2.0, 3.0, 4.0);

      Mask_0_0_0_0 : constant Unsigned_32 := 0 or 0 * 4 or 0 * 16 or 0 * 64;
      Mask_2_2_2_2 : constant Unsigned_32 := 2 or 2 * 4 or 2 * 16 or 2 * 64;
      Mask_1_0_3_2 : constant Unsigned_32 := 1 or 0 * 4 or 3 * 16 or 2 * 64;
      Mask_2_3_0_1 : constant Unsigned_32 := 2 or 3 * 4 or 0 * 16 or 1 * 64;

      Expected : constant array (Positive range <>) of m256d
        := ((1.0, 1.0, 1.0, 1.0),
            (3.0, 3.0, 3.0, 3.0),
            (2.0, 1.0, 4.0, 3.0),
            (3.0, 4.0, 1.0, 2.0));

      Results : array (Positive range Expected'Range) of m256d;
   begin
      Results (1) := Permute (Elements, Mask_0_0_0_0);
      Results (2) := Permute (Elements, Mask_2_2_2_2);
      Results (3) := Permute (Elements, Mask_1_0_3_2);
      Results (4) := Permute (Elements, Mask_2_3_0_1);

      for I in Expected'Range loop
         for J in Index_Homogeneous loop
            declare
               Message : constant String := "Unexpected Single at " & Index_Homogeneous'Image (J);
            begin
               Assert (Expected (I) (J) = Results (I) (J), Message);
            end;
         end loop;
      end loop;
   end Test_Permute;

end Test_SIMD_AVX2_Swizzle;
