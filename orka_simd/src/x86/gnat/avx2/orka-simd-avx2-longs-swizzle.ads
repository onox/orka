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

package Orka.SIMD.AVX2.Longs.Swizzle is
   pragma Pure;

   function Blend (Left, Right, Mask : m256l) return m256l
     with Inline_Always;
   --  Select 8-bit elements from two sources (Left and Right) using a variable mask

   function Cast (Elements : m256l) return m128l
     with Inline_Always;

   function Cast (Elements : m128l) return m256l
     with Inline_Always;

   function Extract (Elements : m256l; Mask : Lane_Mask) return m128l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_extract128i256";
   --  Extract 128-bit from either the lower half (Mask = 0) or upper
   --  half (Mask = 1)

   function Insert (Left : m256l; Right : m128l; Mask : Lane_Mask) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_insert128i256";
   --  Insert Right into the lower half (Mask = 0) or upper half (Mask = 1)

   function Pack (High, Low : m128l) return m256l is (Insert (Cast (Low), High, Mask (Upper)))
     with Inline_Always;

   function Permute (Elements : m256l; Mask : Swizzle_4_Mask) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_permdi256";
   --  Shuffle elements across lanes using ta constant mask

   function Permute_Lanes (Left, Right : m256l; Mask : Swizzle_Lanes_4_Mask) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_permti256";
   --  Shuffle and/or zero 128-bit lanes using a constant mask
   --
   --  The compiler needs access to the Mask at compile-time, thus construct it
   --  using the function Mask to select one of the four lanes for the lower and
   --  upper lanes of the result and/or to zero the lanes.

   function Unpack_High (Left, Right : m256l) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_punpckhqdq256";
   --  Unpack and interleave the 64-bit longs from the upper halves of
   --  the 128-bit lanes of Left and Right as follows:
   --
   --  Left (Y), Right (Y)
   --  Left (W), Right (W)

   function Unpack_Low (Left, Right : m256l) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_punpcklqdq256";
   --  Unpack and interleave the 64-bit longs from the lower halves of
   --  the 128-bit lanes of Left and Right as follows:
   --
   --  Left (X), Right (X)
   --  Left (Z), Right (Z)

end Orka.SIMD.AVX2.Longs.Swizzle;
