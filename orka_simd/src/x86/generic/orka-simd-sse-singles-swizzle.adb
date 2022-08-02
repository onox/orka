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

package body Orka.SIMD.SSE.Singles.Swizzle is

   Mask_1_0_1_0 : constant := 1 * 64 + 0 * 16 + 1 * 4 + 0;
   Mask_3_2_3_2 : constant := 3 * 64 + 2 * 16 + 3 * 4 + 2;
   Mask_2_0_2_0 : constant := 2 * 64 + 0 * 16 + 2 * 4 + 0;
   Mask_3_1_3_1 : constant := 3 * 64 + 1 * 16 + 3 * 4 + 1;

   procedure Transpose (Matrix : in out m128_Array) is
      M0 : constant m128 := Unpack_Low  (Matrix (X), Matrix (Y));
      M1 : constant m128 := Unpack_Low  (Matrix (Z), Matrix (W));
      M2 : constant m128 := Unpack_High (Matrix (X), Matrix (Y));
      M3 : constant m128 := Unpack_High (Matrix (Z), Matrix (W));
   begin
      Matrix (X) := Move_LH (M0, M1);
      Matrix (Y) := Move_HL (M1, M0);
      Matrix (Z) := Move_LH (M2, M3);
      Matrix (W) := Move_HL (M3, M2);
   end Transpose;

   function Transpose (Matrix : m128_Array) return m128_Array is
      Result : m128_Array;

      M0 : constant m128 := Shuffle (Matrix (X), Matrix (Y), Mask_1_0_1_0);
      M1 : constant m128 := Shuffle (Matrix (Z), Matrix (W), Mask_1_0_1_0);
      M2 : constant m128 := Shuffle (Matrix (X), Matrix (Y), Mask_3_2_3_2);
      M3 : constant m128 := Shuffle (Matrix (Z), Matrix (W), Mask_3_2_3_2);
   begin
      Result (X) := Shuffle (M0, M1, Mask_2_0_2_0);
      Result (Y) := Shuffle (M0, M1, Mask_3_1_3_1);
      Result (Z) := Shuffle (M2, M3, Mask_2_0_2_0);
      Result (W) := Shuffle (M2, M3, Mask_3_1_3_1);
      return Result;
   end Transpose;

end Orka.SIMD.SSE.Singles.Swizzle;
