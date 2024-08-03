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

with Orka.SIMD.SSE2.Integers;

package Orka.SIMD.AVX2.Integers.Swizzle is
   pragma Pure;

   use SIMD.SSE2.Integers;

   function Blend (Left, Right : m128i; Mask : Blend_4_Mask) return m128i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pblendd128";

   function Blend (Left, Right : m256i; Mask : Blend_8_Mask) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pblendd256";
   --  Select elements from two sources (Left and Right) using a constant mask

   function Blend (Left, Right, Mask : m256i) return m256i
     with Inline_Always;
   --  Select 8-bit elements from two sources (Left and Right) using a variable mask

   function Extract (Elements : m256i; Mask : Lane_Mask) return m128i
     with Inline_Always;
   --  Extract 128-bit from either the lower half (Mask = 0) or upper
   --  half (Mask = 1)

   function Insert (Left : m256i; Right : m128i; Mask : Lane_Mask) return m256i
     with Inline_Always;
   --  Insert Right into the lower half (Mask = 0) or upper half (Mask = 1)

   function Permute (Elements, Index : m256i) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_permvarsi256";
   --  Shuffle elements across lanes using the first 3 bits of each corresponding element in Index

   function Permute_Lanes (Elements : m256i; Mask : Swizzle_4_Mask) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pshufd256";
   --  Shuffle the 32-bit floats within the two 128-bit lanes using the constant mask
   --
   --  Each lane of the result uses elements retrieved from the corresponding
   --  lane of Elements, both selecting elements using the same mask.
   --
   --  The compiler needs access to the Mask at compile-time, thus construct it
   --  using the function Mask where parameters A, B, C, and D select the
   --  elements to use from a lane of Elements, repeated for each 128-bit lane.

   function Permute_Lanes (Left, Right : m256i; Mask : Swizzle_Lanes_4_Mask) return m256i
     with Inline_Always;
   --  Shuffle and/or zero 128-bit lanes using a constant mask
   --
   --  The compiler needs access to the Mask at compile-time, thus construct it
   --  using the function Mask to select one of the four lanes for the lower and
   --  upper lanes of the result and/or to zero the lanes.

   function Unpack_High (Left, Right : m256i) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_punpckhdq256";
   --  Unpack and interleave the 32-bit integers from the upper halves of
   --  the 128-bit lanes of Left and Right as follows:
   --
   --  Left (3), Right (3), Left (4), Right (4)
   --  Left (7), Right (7), Left (8), Right (8)

   function Unpack_Low (Left, Right : m256i) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_punpckldq256";
   --  Unpack and interleave the 32-bit integers from the lower halves of
   --  the 128-bit lanes of Left and Right as follows:
   --
   --  Left (1), Right (1), Left (2), Right (2)
   --  Left (5), Right (5), Left (6), Right (6)

   function Duplicate_H (Elements : m256i) return m256i
     with Inline_Always;
   --  Duplicate even elements as follows:
   --  Element (2), Element (2), Element (4), Element (4),
   --  Element (6), Element (6), Element (8), Element (8)

   function Duplicate_L (Elements : m256i) return m256i
     with Inline_Always;
   --  Duplicate odd elements as follows:
   --  Element (1), Element (1), Element (3), Element (3),
   --  Element (5), Element (5), Element (7), Element (7)

end Orka.SIMD.AVX2.Integers.Swizzle;
