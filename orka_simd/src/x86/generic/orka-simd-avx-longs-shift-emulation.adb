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

with Ada.Unchecked_Conversion;

with Orka.SIMD.AVX.Integers.Swizzle;
with Orka.SIMD.SSE2.Integers;
with Orka.SIMD.SSE2.Longs.Shift;
with Orka.SIMD.SSSE3.Longs.Shift;

package body Orka.SIMD.AVX.Longs.Shift.Emulation is

   use SIMD.AVX.Integers;
   use SIMD.AVX.Integers.Swizzle;
   use SIMD.SSE2.Integers;
   use SIMD.SSE2.Longs;
   use SIMD.SSE2.Longs.Shift;
   use SIMD.SSSE3.Longs.Shift;

   function Convert is new Ada.Unchecked_Conversion (m256i, m256l);
   function Convert is new Ada.Unchecked_Conversion (m256l, m256i);

   function Convert is new Ada.Unchecked_Conversion (m128i, m128l);
   function Convert is new Ada.Unchecked_Conversion (m128l, m128i);

   function Shift_Elements_Left_Zeros (Elements : m256l) return m256l is
      Low  : constant m128l := Convert (Extract (Convert (Elements), 0));
      High : constant m128l := Convert (Extract (Convert (Elements), 1));
   begin
      return Convert (Pack
        (High => Convert (Align_Right_Bytes
                            (High, Low, (m128l'Length - 1) * m128l'Component_Size)),
         Low  => Convert (Shift_Elements_Left_Zeros (Low))));
   end Shift_Elements_Left_Zeros;

   function Shift_Elements_Right_Zeros (Elements : m256l) return m256l is
      Low  : constant m128l := Convert (Extract (Convert (Elements), 0));
      High : constant m128l := Convert (Extract (Convert (Elements), 1));
   begin
      return Convert (Pack
        (High => Convert (Shift_Elements_Right_Zeros (High)),
         Low  => Convert (Align_Right_Bytes (High, Low, m128l'Component_Size))));
   end Shift_Elements_Right_Zeros;

end Orka.SIMD.AVX.Longs.Shift.Emulation;
