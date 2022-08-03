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

with Orka.SIMD.AVX.Doubles.Compare;
with Orka.SIMD.AVX.Doubles.Logical;
with Orka.SIMD.AVX.Doubles.Swizzle;

package body Orka.SIMD.AVX.Doubles.Arithmetic is

   function "*" (Left, Right : m256d_Array) return m256d_Array is
      Result : m256d_Array;
   begin
      for I in Index_4D'Range loop
         Result (I) := Left * Right (I);
      end loop;
      return Result;
   end "*";

   function "*" (Left : m256d_Array; Right : m256d) return m256d is
      use SIMD.AVX.Doubles.Swizzle;

      Mask_0_0_0_0 : constant := 0 + 0 * 2 + 0 * 4 + 0 * 8;
      Mask_1_1_0_0 : constant := 1 + 1 * 2 + 0 * 4 + 0 * 8;
      Mask_0_0_1_1 : constant := 0 + 0 * 2 + 1 * 4 + 1 * 8;

      XXXX, YYYY, ZZZZ, WWWW, M0, M1, M2, M3 : m256d;
   begin
      --  Shuffling across 128-bit lanes requires AVX2. Thus we first
      --  fill either the lower half or upper half, and then permute
      --  lanes to duplicate one of the halves.
      XXXX := Shuffle_Within_Lanes (Right, Right, Mask_0_0_0_0);  --  X X _ _
      YYYY := Shuffle_Within_Lanes (Right, Right, Mask_1_1_0_0);  --  Y Y _ _
      ZZZZ := Shuffle_Within_Lanes (Right, Right, Mask_0_0_0_0);  --  _ _ Z Z
      WWWW := Shuffle_Within_Lanes (Right, Right, Mask_0_0_1_1);  --  _ _ W W

      XXXX := Duplicate_LH (XXXX);
      YYYY := Duplicate_LH (YYYY);
      ZZZZ := Duplicate_HL (ZZZZ);
      WWWW := Duplicate_HL (WWWW);

      M0 := XXXX * Left (X);
      M1 := YYYY * Left (Y);
      M2 := ZZZZ * Left (Z);
      M3 := WWWW * Left (W);

      M0 := M0 + M1;
      M2 := M2 + M3;
      return M0 + M2;
   end "*";

   function Divide_Or_Zero (Left, Right : m256d) return m256d is
      use SIMD.AVX.Doubles.Compare;
      use SIMD.AVX.Doubles.Logical;

      --  Create a mask with all 1's for each element that is non-zero
      Zero : constant m256d := (0.0, 0.0, 0.0, 0.0);
      Mask : constant m256d := Zero /= Right;

      Normalized : constant m256d := Left / Right;
   begin
      --  Any element in Right that is zero will result in a
      --  corresponding element consisting of all 0's in the Mask.
      --  This will avoid the divide-by-zero exception when dividing.
      return Mask and Normalized;
   end Divide_Or_Zero;

   function "abs" (Elements : m256d) return m256d is
      use SIMD.AVX.Doubles.Logical;
   begin
      return And_Not ((-0.0, -0.0, -0.0, -0.0), Elements);
   end "abs";

   function Sum (Elements : m256d) return Float_64 is
      use SIMD.AVX.Doubles.Swizzle;

      --  Based on SIMD.SSE.Singles.Arithmetic.Sum

      Mask_1_0_1_0 : constant := 1 + 0 * 2 + 1 * 4 + 0 * 8;

      --  Elements:  X   Y   Z   W
      --  Shuffled:  Y   X   W   Z
      --            --------------- +
      --  Sum:      X+Y X+Y Z+W Z+W
      Shuffled : constant m256d := Permute_Within_Lanes (Elements, Mask_1_0_1_0);
      Sum      : constant m256d := Elements + Shuffled;
   begin
      --  Sum:      X+Y X+Y Z+W Z+W
      --  Move:     Z+W Z+W Z+W Z+W
      --            --------------- +
      --  New sum:  X+Y+Z+W . . .
      return Cast (Duplicate_HL (Sum) + Sum) (X);
   end Sum;

end Orka.SIMD.AVX.Doubles.Arithmetic;
