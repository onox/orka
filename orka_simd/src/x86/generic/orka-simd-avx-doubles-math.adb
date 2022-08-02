--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

with Orka.SIMD.AVX.Doubles.Arithmetic;
with Orka.SIMD.AVX.Doubles.Swizzle;

package body Orka.SIMD.AVX.Doubles.Math is

   function Reciprocal_Sqrt (Elements : m256d) return m256d is
      use SIMD.AVX.Doubles.Arithmetic;
   begin
      return (others => 1.0) / Sqrt (Elements);
   end Reciprocal_Sqrt;

   function Cross_Product (Left, Right : m256d) return m256d is
      use SIMD.AVX.Doubles.Arithmetic;
      use SIMD.AVX.Doubles.Swizzle;

      function Shuffle (Elements : m256d) return m256d
        with Inline_Always;

      function Shuffle (Elements : m256d) return m256d is
         Mask_1_0_1_0 : constant := 1 + 0 * 2 + 1 * 4 + 0 * 8;
         Mask_1_2_0_0 : constant := 1 + 2 * 16 + 0 * 8 + 0 * 128;
         Mask_0_1_1_1 : constant := 0 + 1 * 2 + 1 * 4 + 1 * 8;

         YXWZ : constant m256d := Permute_Within_Lanes (Elements, Mask_1_0_1_0);
         WZXY : constant m256d := Permute_Lanes (YXWZ, Elements, Mask_1_2_0_0);
         YZXY : constant m256d := Blend (YXWZ, WZXY, Mask_0_1_1_1);
      begin
         return YZXY;
      end Shuffle;

      Left_YZX  : constant m256d := Shuffle (Left);
      Right_YZX : constant m256d := Shuffle (Right);

      --  Z := Left (X) * Right (Y) - Left (Y) * Right (X)
      --  X := Left (Y) * Right (Z) - Left (Z) * Right (Y)
      --  Y := Left (Z) * Right (X) - Left (X) * Right (Z)
      Result_ZXY : constant m256d := Left * Right_YZX - Left_YZX * Right;
   begin
      return Shuffle (Result_ZXY);
   end Cross_Product;

end Orka.SIMD.AVX.Doubles.Math;
