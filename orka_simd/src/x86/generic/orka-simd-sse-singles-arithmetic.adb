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

with Orka.SIMD.SSE.Singles.Compare;
with Orka.SIMD.SSE.Singles.Logical;
with Orka.SIMD.SSE.Singles.Swizzle;

package body Orka.SIMD.SSE.Singles.Arithmetic is

   function "*" (Left, Right : m128_Array) return m128_Array is
      Result : m128_Array;
   begin
      for I in Index_4D'Range loop
         Result (I) := Left * Right (I);
      end loop;
      return Result;
   end "*";

   function "*" (Left : m128_Array; Right : m128) return m128 is
      use SIMD.SSE.Singles.Swizzle;

      Mask_0_0_0_0 : constant := 0 + 0 * 4 + 0 * 16 + 0 * 64;
      Mask_1_1_1_1 : constant := 1 + 1 * 4 + 1 * 16 + 1 * 64;
      Mask_2_2_2_2 : constant := 2 + 2 * 4 + 2 * 16 + 2 * 64;
      Mask_3_3_3_3 : constant := 3 + 3 * 4 + 3 * 16 + 3 * 64;

      XXXX, YYYY, ZZZZ, WWWW, M0, M1, M2, M3 : m128;
   begin
      XXXX := Shuffle (Right, Right, Mask_0_0_0_0);
      YYYY := Shuffle (Right, Right, Mask_1_1_1_1);
      ZZZZ := Shuffle (Right, Right, Mask_2_2_2_2);
      WWWW := Shuffle (Right, Right, Mask_3_3_3_3);

      M0 := XXXX * Left (X);
      M1 := YYYY * Left (Y);
      M2 := ZZZZ * Left (Z);
      M3 := WWWW * Left (W);

      M0 := M0 + M1;
      M2 := M2 + M3;
      return M0 + M2;
   end "*";

   function Divide_Or_Zero (Left, Right : m128) return m128 is
      use SIMD.SSE.Singles.Compare;
      use SIMD.SSE.Singles.Logical;

      --  Create a mask with all 1's for each element that is non-zero
      Zero : constant m128 := (0.0, 0.0, 0.0, 0.0);
      Mask : constant m128 := Zero /= Right;

      Normalized : constant m128 := Left / Right;
   begin
      --  Any element in Right that is zero will result in a
      --  corresponding element consisting of all 0's in the Mask.
      --  This will avoid the divide-by-zero exception when dividing.
      return Mask and Normalized;
   end Divide_Or_Zero;

   function "abs" (Elements : m128) return m128 is
      use SIMD.SSE.Singles.Logical;
   begin
      return And_Not ((-0.0, -0.0, -0.0, -0.0), Elements);
   end "abs";

   function Sum (Elements : m128) return Float_32 is
      use SIMD.SSE.Singles.Swizzle;

      --  From https://stackoverflow.com/a/35270026

      Mask_1_0_3_2 : constant := 1 + 0 * 4 + 3 * 16 + 2 * 64;

      --  Elements:  X   Y   Z   W
      --  Shuffled:  Y   X   W   Z
      --            --------------- +
      --  Sum:      X+Y X+Y Z+W Z+W
      Shuffled : constant m128 := Shuffle (Elements, Elements, Mask_1_0_3_2);
      Sum      : constant m128 := Elements + Shuffled;

      --  Sum:      X+Y X+Y Z+W Z+W
      --  Move:     Z+W Z+W  W   Z
      --            --------------- +
      --  New sum:  X+Y+Z+W . . .
      Result : constant m128 := Move_HL (Shuffled, Sum) + Sum;
   begin
      return Result (X);
   end Sum;

end Orka.SIMD.SSE.Singles.Arithmetic;
