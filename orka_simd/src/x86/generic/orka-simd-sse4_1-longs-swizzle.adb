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

with Ada.Unchecked_Conversion;

with Orka.SIMD.SSE2.Integers;
with Orka.SIMD.SSE4_1.Integers.Swizzle;

package body Orka.SIMD.SSE4_1.Longs.Swizzle is

   use SIMD.SSE2.Integers;
   use SIMD.SSE4_1.Integers.Swizzle;

   function Convert is new Ada.Unchecked_Conversion (m128l, m128i);
   function Convert is new Ada.Unchecked_Conversion (m128i, m128l);

   function Blend (Left, Right : m128l; Mask : Blend_8_Mask) return m128l is
     (Convert (Blend (Convert (Left), Convert (Right), Mask)));

   function Blend (Left, Right, Mask : m128l) return m128l is
     (Convert (Blend (Convert (Left), Convert (Right), Convert (Mask))));

end Orka.SIMD.SSE4_1.Longs.Swizzle;
