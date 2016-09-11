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

with Orka.SIMD.SSE2.Doubles;

package Orka.SIMD.AVX.Doubles.Swizzle is
   pragma Preelaborate;

   use SIMD.SSE2.Doubles;

   function Shuffle (Left, Right : m256d; Mask : Unsigned_32) return m256d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_shufpd256";
   --  Shuffle the 64-bit doubles in Left and Right using the given Mask.
   --  The first and third doubles are retrieved from Left.
   --  The second and fourth doubles are retrieved from Right:
   --
   --  Result (1) := if Mask (a) = 0 then  Left (1) else  Left (2)
   --  Result (2) := if Mask (b) = 0 then Right (1) else Right (2)
   --  Result (3) := if Mask (c) = 0 then  Left (3) else  Left (4)
   --  Result (4) := if Mask (d) = 0 then Right (3) else Right (4)
   --
   --  The compiler needs access to the Mask at compile-time, thus construct it
   --  as follows:
   --
   --  Mask_d_c_b_a : constant Unsigned_32 := d * 8 or c * 4 or b * 2 or a;
   --
   --  a and c select the doubles to use from Left, b and d from Right.

   function Unpack_High (Left, Right : m256d) return m256d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_unpckhpd256";
   --  Unpack and interleave the 64-bit doubles from the upper halves of
   --  the 128-bit lanes of Left and Right as follows:
   --  Left (2), Right (2), Left (4), Right (4)

   function Unpack_Low (Left, Right : m256d) return m256d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_unpcklpd256";
   --  Unpack and interleave the 64-bit doubles from the lower halves of
   --  the 128-bit lanes of Left and Right as follows:
   --  Left (1), Right (1), Left (3), Right (3)

   function Duplicate (Elements : m256d) return m256d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_movddup256";
   --  Duplicate first and third element as follows:
   --  Elements (1), Elements (1), Elements (3), Elements (3)

   procedure Transpose (Matrix : in out m256d_Array);

   function Transpose (Matrix : m256d_Array) return m256d_Array;

   function Blend (Left, Right : m256d; Mask : Unsigned_32) return m256d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_blendpd256";
   --  Select elements from two sources (Left and Right) using a constant mask

   function Blend (Left, Right, Mask : m256d) return m256d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_blendvpd256";
   --  Select elements from two sources (Left and Right) using a variable mask

   function Extract (Elements : m256d; Mask : Unsigned_32) return m128d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vextractf128_pd256";
   --  Extract 128-bit from either the lower half (Mask = 0) or upper
   --  half (Mask = 1)

   function Insert (Left : m256d; Right : m128d; Mask : Unsigned_32) return m256d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vinsertf128_pd256";
   --  Insert Right into the lower half (Mask = 0) or upper half (Mask = 1)

   function Permute (Elements : m128d; Mask : Unsigned_32) return m128d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vpermilpd";
   --  Shuffle the 64-bit doubles in Elements. Similar to Shuffle (Elements, Elements, Mask):
   --
   --  Result (1) := if Mask (a) = 0 then Elements (1) else Elements (2)
   --  Result (2) := if Mask (b) = 0 then Elements (1) else Elements (2)
   --
   --  The compiler needs access to the Mask at compile-time, thus construct it
   --  as follows:
   --
   --  Mask_b_a : constant Unsigned_32 := b * 2 or a;

   function Permute (Elements : m256d; Mask : Unsigned_32) return m256d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vpermilpd256";
   --  Shuffle elements using just Elements. Similar to Shuffle (Elements, Elements, Mask)

   function Permute_Lanes (Left, Right : m256d; Mask : Unsigned_32) return m256d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vperm2f128_pd256";
   --  Shuffle 128-bit lanes.
   --
   --  Bits 1-2 of Mask are used to control which of the four 128-bit lanes
   --  to use for the lower half (128-bit) of the result. Bits 5-6 to select
   --  a lane for the upper half of the result:
   --
   --  0 =>  Left (1 .. 2)
   --  1 =>  Left (3 .. 4)
   --  2 => Right (1 .. 2)
   --  3 => Right (3 .. 4)
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

end Orka.SIMD.AVX.Doubles.Swizzle;