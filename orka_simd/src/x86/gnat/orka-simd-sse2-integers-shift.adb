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

with Ada.Unchecked_Conversion;

with Orka.SIMD.SSE2.Longs;

package body Orka.SIMD.SSE2.Integers.Shift is

   type All_Bits_Count is new Integer range 0 .. 128;

   use SIMD.SSE2.Longs;

   function Shift_All_Bits_Left_Zeros
     (Elements : m128l; Bits : All_Bits_Count) return m128l
   with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pslldqi128";
   --  Shift each bit to the left by the given amount of bits, shifting in zeros

   function Shift_All_Bits_Right_Zeros
     (Elements : m128l; Bits : All_Bits_Count) return m128l
   with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psrldqi128";
   --  Shift each bit to the right by the given amount of bits, shifting in zeros

   ----------------------------------------------------------------------------

   function Convert is new Ada.Unchecked_Conversion (m128i, m128l);
   function Convert is new Ada.Unchecked_Conversion (m128l, m128i);

   function Shift_Elements_Left_Zeros (Elements : m128i) return m128i is
     (Convert (Shift_All_Bits_Left_Zeros (Convert (Elements), Integer_32'Size)));

   function Shift_Elements_Right_Zeros (Elements : m128i) return m128i is
     (Convert (Shift_All_Bits_Right_Zeros (Convert (Elements), Integer_32'Size)));

end Orka.SIMD.SSE2.Integers.Shift;
