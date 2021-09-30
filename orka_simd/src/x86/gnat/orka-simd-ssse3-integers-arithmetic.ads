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

package Orka.SIMD.SSSE3.Integers.Arithmetic is
   pragma Pure;

   use SIMD.SSE2.Integers;

   function "abs" (Elements : m128i) return m128i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pabsd128";

   function Horizontal_Add (Left, Right : m128i) return m128i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_phaddd128";
   --  Compute the sums of adjacent 32-bit integers in Left and Right
   --
   --  The two sums (four elements gives two pairs) of elements from Left
   --  are stored in the two integers in the lower half, sums from Right in
   --  the upper half.

   function Horizontal_Subtract (Left, Right : m128i) return m128i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_phsubd128";
   --  Compute the differences of adjacent 32-bit integers in Left and Right
   --
   --  The two differences (four elements gives two pairs) of elements
   --  from Left are stored in the two integers in the lower half, differences
   --  from Right in the upper half.

   function Negate_Sign (Elements, Signs : m128i) return m128i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psignd128";

end Orka.SIMD.SSSE3.Integers.Arithmetic;
