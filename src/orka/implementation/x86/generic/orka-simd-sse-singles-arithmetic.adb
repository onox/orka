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

   Mask_0_0_0_0 : constant Unsigned_32 := 0 or 0 * 4 or 0 * 16 or 0 * 64;
   Mask_1_1_1_1 : constant Unsigned_32 := 1 or 1 * 4 or 1 * 16 or 1 * 64;
   Mask_2_2_2_2 : constant Unsigned_32 := 2 or 2 * 4 or 2 * 16 or 2 * 64;
   Mask_3_3_3_3 : constant Unsigned_32 := 3 or 3 * 4 or 3 * 16 or 3 * 64;

   function "*" (Left, Right : m128_Array) return m128_Array is
      use SIMD.SSE.Singles.Swizzle;

      Result : m128_Array;

      XXXX, YYYY, ZZZZ, WWWW, M0, M1, M2, M3 : m128;
   begin
      for I in Index_Homogeneous'Range loop
         XXXX := Shuffle (Right (I), Right (I), Mask_0_0_0_0);
         YYYY := Shuffle (Right (I), Right (I), Mask_1_1_1_1);
         ZZZZ := Shuffle (Right (I), Right (I), Mask_2_2_2_2);
         WWWW := Shuffle (Right (I), Right (I), Mask_3_3_3_3);

         M0 := XXXX * Left (X);
         M1 := YYYY * Left (Y);
         M2 := ZZZZ * Left (Z);
         M3 := WWWW * Left (W);

         M0 := M0 + M1;
         M2 := M2 + M3;
         Result (I) := M0 + M2;
      end loop;
      return Result;
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

end Orka.SIMD.SSE.Singles.Arithmetic;
