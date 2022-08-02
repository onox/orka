--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2020 onox <denkpadje@gmail.com>
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
with Orka.SIMD.AVX2.Doubles.Swizzle;

package body Orka.SIMD.FMA.Doubles.Arithmetic is

   function "*" (Left, Right : m256d_Array) return m256d_Array is
      Result : m256d_Array;
   begin
      for I in Index_4D'Range loop
         Result (I) := Left * Right (I);
      end loop;
      return Result;
   end "*";

   function "*" (Left : m256d_Array; Right : m256d) return m256d is
      use SIMD.AVX.Doubles.Arithmetic;
      use SIMD.AVX2.Doubles.Swizzle;

      Mask_0_0_0_0 : constant := 0 + 0 * 4 + 0 * 16 + 0 * 64;
      Mask_1_1_1_1 : constant := 1 + 1 * 4 + 1 * 16 + 1 * 64;
      Mask_2_2_2_2 : constant := 2 + 2 * 4 + 2 * 16 + 2 * 64;
      Mask_3_3_3_3 : constant := 3 + 3 * 4 + 3 * 16 + 3 * 64;

      XXXX, YYYY, ZZZZ, WWWW, Result : m256d;
   begin
      XXXX := Permute (Right, Mask_0_0_0_0);
      YYYY := Permute (Right, Mask_1_1_1_1);
      ZZZZ := Permute (Right, Mask_2_2_2_2);
      WWWW := Permute (Right, Mask_3_3_3_3);

      Result := XXXX * Left (X);
      Result := Multiply_Add (YYYY, Left (Y), Result);
      Result := Multiply_Add (ZZZZ, Left (Z), Result);
      Result := Multiply_Add (WWWW, Left (W), Result);

      return Result;
   end "*";

end Orka.SIMD.FMA.Doubles.Arithmetic;
