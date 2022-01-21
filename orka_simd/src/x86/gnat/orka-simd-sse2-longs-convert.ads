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

with Orka.SIMD.SSE2.Doubles.Arithmetic;
with Orka.SIMD.SSE2.Integers.Shift;

package Orka.SIMD.SSE2.Longs.Convert is
   pragma Pure;

   use SIMD.SSE2.Doubles;
   use SIMD.SSE2.Doubles.Arithmetic;
   use SIMD.SSE2.Integers;
   use SIMD.SSE2.Integers.Shift;

   function Convert (Elements : m128d) return m128i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_cvtpd2dq";

   function Convert (Elements : m128i) return m128d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_cvtdq2pd";

   Smallest_Elements_F32 : constant m128d := (others => 2.0**(-Float_32'Machine_Mantissa));

   function To_Unit_Floats (Elements : m128i) return m128d is
     (Convert (Shift_Bits_Right_Zeros (Elements, Float_32'Size - Float_32'Machine_Mantissa))
        * Smallest_Elements_F32)
   with Inline;
   --  Return first two floating-point numbers in the 0 .. 1 interval

end Orka.SIMD.SSE2.Longs.Convert;
