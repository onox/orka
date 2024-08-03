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

   function Extract (Elements : m256i; Mask : Lane_Mask) return m128i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vextractf128_si256";
   --  Extract 128-bit from either the lower half (Mask = 0) or upper
   --  half (Mask = 1)

   function Insert (Left : m256i; Right : m128i; Mask : Lane_Mask) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vinsertf128_si256";
   --  Insert Right into the lower half (Mask = 0) or upper half (Mask = 1)

   function Pack (High, Low : m128i) return m256i is (Insert (Cast (Low), High, Mask (Upper)))
     with Inline_Always;

   function Permute_Lanes (Left, Right : m256i; Mask : Swizzle_Lanes_4_Mask) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vperm2f128_si256";
   --  Shuffle and/or zero 128-bit lanes using a constant mask
   --
   --  The compiler needs access to the Mask at compile-time, thus construct it
   --  using the function Mask to select one of the four lanes for the lower and
   --  upper lanes of the result and/or to zero the lanes.

end Orka.SIMD.AVX.Integers.Swizzle;
