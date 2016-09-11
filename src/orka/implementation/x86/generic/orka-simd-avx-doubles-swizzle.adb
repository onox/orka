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

   Mask_1_0_1_0 : constant Unsigned_32 := 1 * 64 or 0 * 16 or 1 * 4 or 0;
   Mask_3_2_3_2 : constant Unsigned_32 := 3 * 64 or 2 * 16 or 3 * 4 or 2;
   Mask_2_0_2_0 : constant Unsigned_32 := 2 * 64 or 0 * 16 or 2 * 4 or 0;
   Mask_3_1_3_1 : constant Unsigned_32 := 3 * 64 or 1 * 16 or 3 * 4 or 1;

   Mask_2_0 : constant Unsigned_32 := 2 * 16 or 0;
   Mask_1_3 : constant Unsigned_32 := 1 * 16 or 3;

   procedure Transpose (Matrix : in out m256d_Array) is
      M0 : constant m256d := Unpack_Low  (Matrix (X), Matrix (Y));
      M1 : constant m256d := Unpack_Low  (Matrix (Z), Matrix (W));
      M2 : constant m256d := Unpack_High (Matrix (X), Matrix (Y));
      M3 : constant m256d := Unpack_High (Matrix (Z), Matrix (W));
   begin
      --  Mask_2_0 => M0 (1), M0 (2), M1 (1), M1 (2)
      Matrix (X) := Permute_Lanes (M0, M1, Mask_2_0);
      --  Mask_1_3 => M0 (3), M0 (4), M1 (3), M1 (4)
      Matrix (Y) := Permute_Lanes (M1, M0, Mask_1_3);
      --  Mask_2_0 => M2 (1), M2 (2), M3 (1), M3 (2)
      Matrix (Z) := Permute_Lanes (M2, M3, Mask_2_0);
      --  Mask_1_3 => M2 (3), M2 (4), M3 (3), M3 (4)
      Matrix (W) := Permute_Lanes (M3, M2, Mask_1_3);
   end Transpose;

   function Transpose (Matrix : m256d_Array) return m256d_Array is
      Result : m256d_Array;

      M0 : constant m256d := Shuffle (Matrix (X), Matrix (Y), Mask_1_0_1_0);
      M1 : constant m256d := Shuffle (Matrix (Z), Matrix (W), Mask_1_0_1_0);
      M2 : constant m256d := Shuffle (Matrix (X), Matrix (Y), Mask_3_2_3_2);
      M3 : constant m256d := Shuffle (Matrix (Z), Matrix (W), Mask_3_2_3_2);
   begin
      Result (X) := Shuffle (M0, M1, Mask_2_0_2_0);
      Result (Y) := Shuffle (M0, M1, Mask_3_1_3_1);
      Result (Z) := Shuffle (M2, M3, Mask_2_0_2_0);
      Result (W) := Shuffle (M2, M3, Mask_3_1_3_1);
      return Result;
   end Transpose;

end Orka.SIMD.AVX.Doubles.Swizzle;
