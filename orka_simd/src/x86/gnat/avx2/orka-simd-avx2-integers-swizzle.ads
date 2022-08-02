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

   function Blend (Left, Right : m256i; Mask : Integer_32) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pblendd256";
   --  Select elements from two sources (Left and Right) using a constant mask

   function Extract (Elements : m256i; Mask : Integer_32) return m128i
     with Inline_Always;
   --  Extract 128-bit from either the lower half (Mask = 0) or upper
   --  half (Mask = 1)

   function Insert (Left : m256i; Right : m128i; Mask : Integer_32) return m256i
     with Inline_Always;
   --  Insert Right into the lower half (Mask = 0) or upper half (Mask = 1)

   function Permute (Elements, Index : m256i) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_permvarsi256";
   --  Shuffle elements across lanes using the first 3 bits of each corresponding element in Index

   function Permute_Lanes (Elements : m256i; Mask : Integer_32) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pshufd256";
   --  Shuffle elements within the two 128-bit lanes
   --
   --  For each element in each lane, 2 bits in the Mask are used to control
   --  which element of the lane to use for the result.
   --
   --  Bits 1-2 for element 1
   --  Bits 3-4 for element 2
   --  Bits 5-6 for element 3
   --  Bits 7-8 for element 4
   --
   --  The same 8 bits of Mask are used to repeat the process for the second lane.

   function Permute_Lanes (Left, Right : m256i; Mask : Integer_32) return m256i
     with Inline_Always;
   --  Shuffle 128-bit lanes
   --
   --  Bits 1-2 of Mask are used to control which of the four 128-bit lanes
   --  to use for the lower half (128-bit) of the result. Bits 5-6 to select
   --  a lane for the upper half of the result:
   --
   --  0 =>  Left (1 .. 4)
   --  1 =>  Left (5 .. 8)
   --  2 => Right (1 .. 4)
   --  3 => Right (5 .. 8)
   --
   --  Bits 4 and 8 are used to zero the corresponding half (lower or upper).
   --
   --  The compiler needs access to the Mask at compile-time, thus construct it
   --  as follows:
   --
   --  Mask_zu_zl_u_l : constant Unsigned_32 := zu * 128 or zl * 8 or u * 16 or l;
   --
   --  u and l are numbers between 0 and 3 (see above). zu and zl are either 0 or 1
   --  to zero a lane.

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

end Orka.SIMD.AVX2.Integers.Swizzle;
