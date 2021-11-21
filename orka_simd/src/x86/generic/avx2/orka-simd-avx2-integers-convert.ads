--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2021 onox <denkpadje@gmail.com>
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

with Orka.SIMD.AVX.Integers.Convert;
with Orka.SIMD.AVX.Singles.Arithmetic;
with Orka.SIMD.AVX2.Integers.Shift;

package Orka.SIMD.AVX2.Integers.Convert is
   pragma Pure;

   use SIMD.AVX.Singles;
   use SIMD.AVX.Singles.Arithmetic;
   use SIMD.AVX2.Integers.Shift;

   function Convert (Elements : m256i) return m256 renames SIMD.AVX.Integers.Convert.Convert;

   Smallest_Elements : constant m256 := (others => 2.0**(-Float_32'Machine_Mantissa));

   function To_Unit_Floats (Elements : m256i) return m256 is
     (Convert (Shift_Bits_Right_Zeros (Elements, Float_32'Size - Float_32'Machine_Mantissa))
        * Smallest_Elements)
   with Inline;
   --  Return floating-point numbers in the 0 .. 1 interval

end Orka.SIMD.AVX2.Integers.Convert;
