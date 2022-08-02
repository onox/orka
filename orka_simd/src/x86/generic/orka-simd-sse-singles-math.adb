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

with Orka.SIMD.SSE.Singles.Arithmetic;
with Orka.SIMD.SSE.Singles.Swizzle;

package body Orka.SIMD.SSE.Singles.Math is

   function Cross_Product (Left, Right : m128) return m128 is
      use SIMD.SSE.Singles.Arithmetic;
      use SIMD.SSE.Singles.Swizzle;

      Mask_1_2_0_3 : constant := 1 + 2 * 4 + 0 * 16 + 3 * 64;

      Left_YZX  : constant m128 := Shuffle (Left, Left, Mask_1_2_0_3);
      Right_YZX : constant m128 := Shuffle (Right, Right, Mask_1_2_0_3);

      --  Z := Left (X) * Right (Y) - Left (Y) * Right (X)
      --  X := Left (Y) * Right (Z) - Left (Z) * Right (Y)
      --  Y := Left (Z) * Right (X) - Left (X) * Right (Z)
      Result_ZXY : constant m128 := Left * Right_YZX - Left_YZX * Right;
   begin
      return Shuffle (Result_ZXY, Result_ZXY, Mask_1_2_0_3);
   end Cross_Product;

end Orka.SIMD.SSE.Singles.Math;
