--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2020 onox <denkpadje@gmail.com>
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

with Orka.SIMD.AVX.Doubles;

package Orka.SIMD.AVX2.Doubles.Swizzle is
   pragma Pure;

   use SIMD.AVX.Doubles;

   function Permute (Elements : m256d; Mask : Swizzle_4_Mask) return m256d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_permdf256";
   --  Shuffle the 64-bit doubles in Elements using a constant mask
   --
   --  The compiler needs access to the Mask at compile-time, thus construct it
   --  using the function Mask where parameters A, B, C, and D each select one
   --  of the four elements from Elements.

end Orka.SIMD.AVX2.Doubles.Swizzle;
