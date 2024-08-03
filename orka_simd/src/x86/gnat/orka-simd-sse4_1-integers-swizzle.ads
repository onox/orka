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

with Orka.SIMD.SSE2.Integers;

package Orka.SIMD.SSE4_1.Integers.Swizzle is
   pragma Pure;

   use Orka.SIMD.SSE2.Integers;

   function Blend (Left, Right : m128i; Mask : Blend_8_Mask) return m128i
     with Inline_Always;
   --  Select elements from two sources (Left and Right) using a constant mask
   --
   --  The compiler needs access to the Mask at compile-time, thus construct it
   --  using the function Mask with four parameters A, B, C and D, where each
   --  parameter is 0 to choose an element from Left and 1 to choose from Right.

   --  Select 16-bit elements from two sources (Left and Right) using a constant 8-bit mask

   function Blend (Left, Right, Mask : m128i) return m128i
     with Inline_Always;
   --  Select 8-bit elements from two sources (Left and Right) using a variable mask

end Orka.SIMD.SSE4_1.Integers.Swizzle;
