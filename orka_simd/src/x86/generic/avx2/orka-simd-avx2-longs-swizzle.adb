--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2024 onox <denkpadje@gmail.com>
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

with Ada.Unchecked_Conversion;

with Orka.SIMD.AVX.Longs.Swizzle;
with Orka.SIMD.AVX2.Integers.Swizzle;

package body Orka.SIMD.AVX2.Longs.Swizzle is

   use SIMD.AVX2.Integers;
   use SIMD.AVX2.Integers.Swizzle;

   function Convert is new Ada.Unchecked_Conversion (m256l, m256i);
   function Convert is new Ada.Unchecked_Conversion (m256i, m256l);

   function Blend (Left, Right, Mask : m256l) return m256l is
     (Convert (Blend (Convert (Left), Convert (Right), Convert (Mask))));

   function Cast (Elements : m256l) return m128l renames SIMD.AVX.Longs.Swizzle.Cast;

   function Cast (Elements : m128l) return m256l renames SIMD.AVX.Longs.Swizzle.Cast;

end Orka.SIMD.AVX2.Longs.Swizzle;
