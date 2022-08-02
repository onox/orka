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

   function Extract (Elements : m256l; Mask : Integer_32) return m128l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_extract128i256";
   --  Extract 128-bit from either the lower half (Mask = 0) or upper
   --  half (Mask = 1)

   function Insert (Left : m256l; Right : m128l; Mask : Integer_32) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_insert128i256";
   --  Insert Right into the lower half (Mask = 0) or upper half (Mask = 1)

   function Permute_Lanes (Elements : m256l; Mask : Integer_32) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_permdi256";
   --  Shuffle elements across lanes using the first 2 bits of each corresponding element in Index

   function Permute_Lanes (Left, Right : m256l; Mask : Integer_32) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_permti256";
   --  Shuffle 128-bit lanes
   --
   --  Bits 1-2 of Mask are used to control which of the four 128-bit lanes
   --  to use for the lower half (128-bit) of the result. Bits 5-6 to select
   --  a lane for the upper half of the result:
   --
   --  0 =>  Left (X .. Y)
   --  1 =>  Left (Z .. W)
   --  2 => Right (X .. Y)
   --  3 => Right (Z .. W)
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
