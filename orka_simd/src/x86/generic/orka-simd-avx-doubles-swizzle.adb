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

package body Orka.SIMD.AVX.Doubles.Swizzle is

   Mask_LH : constant := 0 + 0 * 16 + 0 * 8 + 0 * 128;
   Mask_HL : constant := 1 + 1 * 16 + 0 * 8 + 0 * 128;

   function Duplicate_LH (Elements : m256d) return m256d is
     (Permute_Lanes (Elements, Elements, Mask_LH));

   function Duplicate_HL (Elements : m256d) return m256d is
     (Permute_Lanes (Elements, Elements, Mask_HL));

   Mask_0_2_0_0 : constant := 0 + 2 * 16 + 0 * 8 + 0 * 128;
   Mask_1_3_0_0 : constant := 1 + 3 * 16 + 0 * 8 + 0 * 128;

   procedure Transpose (Matrix : in out m256d_Array) is
      M0 : constant m256d := Unpack_Low  (Matrix (X), Matrix (Y));
      M1 : constant m256d := Unpack_High (Matrix (X), Matrix (Y));
      M2 : constant m256d := Unpack_Low  (Matrix (Z), Matrix (W));
      M3 : constant m256d := Unpack_High (Matrix (Z), Matrix (W));
   begin
      Matrix (X) := Permute_Lanes (M0, M2, Mask_0_2_0_0);
      Matrix (Y) := Permute_Lanes (M1, M3, Mask_0_2_0_0);
      Matrix (Z) := Permute_Lanes (M0, M2, Mask_1_3_0_0);
      Matrix (W) := Permute_Lanes (M1, M3, Mask_1_3_0_0);
   end Transpose;

   function Transpose (Matrix : m256d_Array) return m256d_Array is
      Result : m256d_Array;

      M0 : constant m256d := Unpack_Low  (Matrix (X), Matrix (Y));
      M1 : constant m256d := Unpack_High (Matrix (X), Matrix (Y));
      M2 : constant m256d := Unpack_Low  (Matrix (Z), Matrix (W));
      M3 : constant m256d := Unpack_High (Matrix (Z), Matrix (W));
   begin
      Result (X) := Permute_Lanes (M0, M2, Mask_0_2_0_0);
      Result (Y) := Permute_Lanes (M1, M3, Mask_0_2_0_0);
      Result (Z) := Permute_Lanes (M0, M2, Mask_1_3_0_0);
      Result (W) := Permute_Lanes (M1, M3, Mask_1_3_0_0);
      return Result;
   end Transpose;

end Orka.SIMD.AVX.Doubles.Swizzle;
