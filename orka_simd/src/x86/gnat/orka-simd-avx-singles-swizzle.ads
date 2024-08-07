--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2016 onox <denkpadje@gmail.com>
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

with Orka.SIMD.SSE.Singles;

package Orka.SIMD.AVX.Singles.Swizzle is
   pragma Pure;

   use SIMD.SSE.Singles;

   function Shuffle (Left, Right : m256; Mask : Swizzle_4_Mask) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_shufps256";
   --  Shuffle the 32-bit floats in Left and Right using the given Mask
   --
   --  The first 4 elements are retrieved from the first lane of Left and Right,
   --  and the last 4 elements from the second lane of Left and Right.
   --
   --  The compiler needs access to the Mask at compile-time, thus construct it
   --  using the function Mask where parameters A and B select the floats to use
   --  from Left and C and D from Right, repeated for each 128-bit lane.

   function Unpack_High (Left, Right : m256) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_unpckhps256";
   --  Unpack and interleave the 32-bit floats from the upper halves of
   --  the 128-bit lanes of Left and Right as follows:
   --  Left (3), Right (3), Left (4), Right (4)
   --  Left (7), Right (7), Left (8), Right (8)

   function Unpack_Low (Left, Right : m256) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_unpcklps256";
   --  Unpack and interleave the 32-bit floats from the lower halves of
   --  the 128-bit lanes of Left and Right as follows:
   --  Left (1), Right (1), Left (2), Right (2)
   --  Left (5), Right (5), Left (6), Right (6)

   function Duplicate_H (Elements : m256) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_movshdup256";
   --  Duplicate second, fourth, sixth and eight element as follows:
   --  Elements (2), Elements (2), Elements (4), Elements (4)
   --  Elements (6), Elements (6), Elements (8), Elements (8)

   function Duplicate_L (Elements : m256) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_movsldup256";
   --  Duplicate first, third, fifth and seventh element as follows:
   --  Elements (1), Elements (1), Elements (3), Elements (3)
   --  Elements (5), Elements (5), Elements (7), Elements (7)

   function Blend (Left, Right : m256; Mask : Blend_8_Mask) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_blendps256";
   --  Select elements from two sources (Left and Right) using a constant mask
   --
   --  The compiler needs access to the Mask at compile-time, thus construct it
   --  using the function Mask where each of the parameters A, B, C, D, E, F, G, and H is
   --  either a 0 to select an element from Left or a 1 to select an element from Right.

   function Blend (Left, Right, Mask : m256) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_blendvps256";
   --  Select elements from two sources (Left and Right) using a variable mask

   function Cast (Elements : m256) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_ps_ps256";

   function Cast (Elements : m128) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_ps256_ps";

   function Extract (Elements : m256; Mask : Lane_Mask) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vextractf128_ps256";
   --  Extract 128-bit from either the lower half (Mask = 0) or upper
   --  half (Mask = 1)

   function Insert (Left : m256; Right : m128; Mask : Lane_Mask) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vinsertf128_ps256";
   --  Insert Right into the lower half (Mask = 0) or upper half (Mask = 1)

   function Pack (High, Low : m128) return m256 is (Insert (Cast (Low), High, Mask (Upper)))
     with Inline_Always;

   function Permute (Elements : m128; Mask : Swizzle_4_Mask) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vpermilps";
   --  Shuffle elements using just Elements
   --
   --  Similar to Shuffle (Elements, Elements, Mask)

   function Permute (Elements : m256; Mask : Swizzle_4_Mask) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vpermilps256";
   --  Shuffle elements using just Elements within each 128-bit lane
   --
   --  Similar to Shuffle (Elements, Elements, Mask)

   function Permute_Lanes (Left, Right : m256; Mask : Swizzle_Lanes_4_Mask) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vperm2f128_ps256";
   --  Shuffle and/or zero 128-bit lanes using a constant mask
   --
   --  The compiler needs access to the Mask at compile-time, thus construct it
   --  using the function Mask to select one of the four lanes for the lower and
   --  upper lanes of the result and/or to zero the lanes.

end Orka.SIMD.AVX.Singles.Swizzle;
