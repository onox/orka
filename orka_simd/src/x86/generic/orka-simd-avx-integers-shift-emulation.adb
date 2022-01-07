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
with Orka.SIMD.SSE2.Integers.Shift;
with Orka.SIMD.SSSE3.Integers.Shift;

package body Orka.SIMD.AVX.Integers.Shift.Emulation is

   use SIMD.AVX.Integers.Swizzle;
   use SIMD.SSE2.Integers;
   use SIMD.SSE2.Integers.Shift;
   use SIMD.SSSE3.Integers.Shift;

   function Shift_Elements_Left_Zeros (Elements : m256i) return m256i is
      Low  : constant m128i := Extract (Elements, 0);
      High : constant m128i := Extract (Elements, 1);
   begin
      return Pack
        (High => Align_Right_Bytes (High, Low, (m128i'Length - 1) * m128i'Component_Size),
         Low  => Shift_Elements_Left_Zeros (Low));
   end Shift_Elements_Left_Zeros;

   function Shift_Elements_Right_Zeros (Elements : m256i) return m256i is
      Low  : constant m128i := Extract (Elements, 0);
      High : constant m128i := Extract (Elements, 1);
   begin
      return Pack
        (High => Shift_Elements_Right_Zeros (High),
         Low  => Align_Right_Bytes (High, Low, m128i'Component_Size));
   end Shift_Elements_Right_Zeros;

end Orka.SIMD.AVX.Integers.Shift.Emulation;
