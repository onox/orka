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

with Orka.SIMD.AVX.Singles.Compare;
with Orka.SIMD.AVX.Singles.Logical;
with Orka.SIMD.AVX.Singles.Swizzle;
with Orka.SIMD.SSE.Singles.Arithmetic;
with Orka.SIMD.SSE3.Singles.Arithmetic;

package body Orka.SIMD.AVX.Singles.Arithmetic is

   function Divide_Or_Zero (Left, Right : m256) return m256 is
      use SIMD.AVX.Singles.Compare;
      use SIMD.AVX.Singles.Logical;

      --  Create a mask with all 1's for each element that is non-zero
      Zero : constant m256 := (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
      Mask : constant m256 := Zero /= Right;

      Normalized : constant m256 := Left / Right;
   begin
      --  Any element in Right that is zero will result in a
      --  corresponding element consisting of all 0's in the Mask.
      --  This will avoid the divide-by-zero exception when dividing.
      return Mask and Normalized;
   end Divide_Or_Zero;

   function "abs" (Elements : m256) return m256 is
      use SIMD.AVX.Singles.Logical;
   begin
      return And_Not ((-0.0, -0.0, -0.0, -0.0, -0.0, -0.0, -0.0, -0.0), Elements);
   end "abs";

   function Sum (Elements : m256) return Float_32 is
      use SIMD.SSE.Singles;
      use SIMD.SSE.Singles.Arithmetic;
      use SIMD.AVX.Singles.Swizzle;

      --  From https://stackoverflow.com/a/35270026

      Low  : constant m128 := Cast (Elements);
      High : constant m128 := Extract (Elements, 1);
   begin
      return SIMD.SSE3.Singles.Arithmetic.Sum (Low + High);
   end Sum;

end Orka.SIMD.AVX.Singles.Arithmetic;
