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

with Orka.SIMD.AVX.Integers.Swizzle;
with Orka.SIMD.SSE2.Integers.Arithmetic;

package body Orka.SIMD.AVX2.Integers.Arithmetic is

   function Sum (Elements : m256i) return Integer_32 is
      use SIMD.SSE2.Integers;
      use SIMD.SSE2.Integers.Arithmetic;
      use SIMD.AVX.Integers.Swizzle;

      --  From https://stackoverflow.com/a/35270026

      Low  : constant m128i := Cast (Elements);
      High : constant m128i := Extract (Elements, 1);
   begin
      return SIMD.SSE2.Integers.Arithmetic.Sum (Low + High);
   end Sum;

end Orka.SIMD.AVX2.Integers.Arithmetic;
