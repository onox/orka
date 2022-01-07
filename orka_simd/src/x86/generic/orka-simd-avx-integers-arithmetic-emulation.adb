--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2022 onox <denkpadje@gmail.com>
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

with Orka.SIMD.AVX.Integers.Swizzle;
with Orka.SIMD.SSE2.Integers.Arithmetic;

package body Orka.SIMD.AVX.Integers.Arithmetic.Emulation is

   use SIMD.AVX.Integers.Swizzle;
   use SIMD.SSE2.Integers;
   use SIMD.SSE2.Integers.Arithmetic;

   function "+" (Left, Right : m256i) return m256i is
      Left_Low  : constant m128i := Extract (Left, 0);
      Left_High : constant m128i := Extract (Left, 1);

      Right_Low  : constant m128i := Extract (Right, 0);
      Right_High : constant m128i := Extract (Right, 1);
   begin
      return Pack (High => Left_High + Right_High, Low => Left_Low + Right_Low);
   end "+";

   function "-" (Left, Right : m256i) return m256i is
      Left_Low  : constant m128i := Extract (Left, 0);
      Left_High : constant m128i := Extract (Left, 1);

      Right_Low  : constant m128i := Extract (Right, 0);
      Right_High : constant m128i := Extract (Right, 1);
   begin
      return Pack (High => Left_High - Right_High, Low => Left_Low - Right_Low);
   end "-";

end Orka.SIMD.AVX.Integers.Arithmetic.Emulation;
