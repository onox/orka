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

package Orka.SIMD.SSE2.Doubles.Swizzle is
   pragma Pure;

   function Shuffle (Left, Right : m128d; Mask : Integer_32) return m128d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_shufpd";
   --  Shuffle the 64-bit doubles in Left and Right using the given Mask. The first
   --  double (lower half) is retrieved from Left, the second double (upper half)
   --  from Right.
   --
   --  The compiler needs access to the Mask at compile-time, thus construct it
   --  as follows:
   --
   --  Mask_a_b : constant Unsigned_32 := a or b * 2;
   --
   --  a selects the double to use from Left, b from Right.

   function Unpack_High (Left, Right : m128d) return m128d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_unpckhpd";
   --  Unpack and interleave the 64-bit doubles from the upper halves of
   --  Left and Right as follows: Left (2), Right (2)

   function Unpack_Low (Left, Right : m128d) return m128d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_unpcklpd";
   --  Unpack and interleave the 64-bit doubles from the lower halves of
   --  Left and Right as follows: Left (1), Right (1)

end Orka.SIMD.SSE2.Doubles.Swizzle;
