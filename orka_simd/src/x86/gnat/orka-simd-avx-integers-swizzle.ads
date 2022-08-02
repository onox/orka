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

with Orka.SIMD.SSE2.Integers;

package Orka.SIMD.AVX.Integers.Swizzle is
   pragma Pure;

   use SIMD.SSE2.Integers;

   function Cast (Elements : m256i) return m128i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_si_si256";

   function Cast (Elements : m128i) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_si256_si";

   function Extract (Elements : m256i; Mask : Integer_32) return m128i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vextractf128_si256";
   --  Extract 128-bit from either the lower half (Mask = 0) or upper
   --  half (Mask = 1)

   function Insert (Left : m256i; Right : m128i; Mask : Integer_32) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vinsertf128_si256";
   --  Insert Right into the lower half (Mask = 0) or upper half (Mask = 1)

   function Pack (High, Low : m128i) return m256i is (Insert (Cast (Low), High, 1))
     with Inline_Always;

   function Permute_Lanes (Left, Right : m256i; Mask : Integer_32) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vperm2f128_si256";
   --  Shuffle 128-bit lanes.
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

end Orka.SIMD.AVX.Integers.Swizzle;
