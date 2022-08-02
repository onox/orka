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

with Orka.SIMD.SSE2.Doubles.Compare;
with Orka.SIMD.SSE2.Doubles.Logical;
with Orka.SIMD.SSE2.Doubles.Swizzle;

package body Orka.SIMD.SSE2.Doubles.Arithmetic is

   function Divide_Or_Zero (Left, Right : m128d) return m128d is
      use SIMD.SSE2.Doubles.Compare;
      use SIMD.SSE2.Doubles.Logical;

      --  Create a mask with all 1's for each element that is non-zero
      Zero : constant m128d := (0.0, 0.0);
      Mask : constant m128d := Zero /= Right;

      Normalized : constant m128d := Left / Right;
   begin
      --  Any element in Right that is zero will result in a
      --  corresponding element consisting of all 0's in the Mask.
      --  This will avoid the divide-by-zero exception when dividing.
      return Mask and Normalized;
   end Divide_Or_Zero;

   function "abs" (Elements : m128d) return m128d is
      use SIMD.SSE2.Doubles.Logical;
   begin
      return And_Not ((-0.0, -0.0), Elements);
   end "abs";

   function Sum (Elements : m128d) return Float_64 is
      use SIMD.SSE2.Doubles.Swizzle;

      --  Based on SIMD.SSE.Singles.Arithmetic.Sum

      Mask_1_0 : constant := 1 + 0 * 2;

      Shuffled : constant m128d := Shuffle (Elements, Elements, Mask_1_0);
      Sum      : constant m128d := Elements + Shuffled;
   begin
      return Sum (X);
   end Sum;

end Orka.SIMD.SSE2.Doubles.Arithmetic;
