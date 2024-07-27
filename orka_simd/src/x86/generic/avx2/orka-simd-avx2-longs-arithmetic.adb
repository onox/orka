--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2024 onox <denkpadje@gmail.com>
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

with Orka.SIMD.AVX2.Longs.Swizzle;
with Orka.SIMD.SSE2.Longs.Arithmetic;
with Orka.SIMD.SSE4_1.Longs.Arithmetic;

package body Orka.SIMD.AVX2.Longs.Arithmetic is

   use SIMD.SSE2.Longs;
   use SIMD.AVX2.Longs.Swizzle;

   function "*" (Left, Right : m256l) return m256l is
      use SIMD.SSE4_1.Longs.Arithmetic;

      Left_Low  : constant m128l := Extract (Left, 0);
      Left_High : constant m128l := Extract (Left, 1);

      Right_Low  : constant m128l := Extract (Right, 0);
      Right_High : constant m128l := Extract (Right, 1);
   begin
      return Pack
        (High => Left_High * Right_High,
         Low  => Left_Low * Right_Low);
   end "*";

   function Sum (Elements : m256l) return Integer_64 is
      use SIMD.SSE2.Longs.Arithmetic;

      --  From https://stackoverflow.com/a/35270026

      Low  : constant m128l := Cast (Elements);
      High : constant m128l := Extract (Elements, 1);

      Result : constant m128l := Low + High;
   begin
      return Result (X) + Result (Y);
   end Sum;

end Orka.SIMD.AVX2.Longs.Arithmetic;
