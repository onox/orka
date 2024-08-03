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

package Orka.SIMD.SSE2.Integers.Swizzle is
   pragma Pure;

   function Permute (Elements : m128i; Mask : Swizzle_4_Mask) return m128i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pshufd";
   --  Shuffle the 32-bit integers using a constant mask
   --
   --  The compiler needs access to the Mask at compile-time, thus construct it
   --  using the function Mask where parameters A, B, C, and D each select one
   --  of the four elements from Elements.

   function Duplicate_H (Elements : m128i) return m128i
     with Inline_Always;
   --  Duplicate second and fourth element as follows:
   --  Element (2), Element (2), Element (4), Element (4)

   function Duplicate_L (Elements : m128i) return m128i
     with Inline_Always;
   --  Duplicate first and third element as follows:
   --  Element (1), Element (1), Element (3), Element (3)

end Orka.SIMD.SSE2.Integers.Swizzle;
