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

with Orka.SIMD.SSE2.Longs;

package body Orka.SIMD.SSSE3.Integers.Shift is

   use SIMD.SSE2.Longs;

   function Align_Right_Bytes (Left, Right : m128l; Mask : Unsigned_32) return m128l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_palignr128";

   ----------------------------------------------------------------------------

   function Convert is new Ada.Unchecked_Conversion (m128i, m128l);
   function Convert is new Ada.Unchecked_Conversion (m128l, m128i);

   function Align_Right_Bytes (Left, Right : m128i; Mask : Unsigned_32) return m128i is
     (Convert (Align_Right_Bytes (Convert (Left), Convert (Right), Mask)));

end Orka.SIMD.SSSE3.Integers.Shift;
