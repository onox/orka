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

with Ada.Unchecked_Conversion;

with Orka.SIMD.AVX.Longs;
with Orka.SIMD.AVX2.Longs.Swizzle;
with Orka.SIMD.SSE2.Longs;

package body Orka.SIMD.AVX2.Integers.Swizzle is

   use SIMD.AVX.Longs;
   use SIMD.SSE2.Longs;

   function Convert is new Ada.Unchecked_Conversion (m256i, m256l);
   function Convert is new Ada.Unchecked_Conversion (m256l, m256i);

   function Convert is new Ada.Unchecked_Conversion (m128i, m128l);
   function Convert is new Ada.Unchecked_Conversion (m128l, m128i);

   function Extract (Elements : m256i; Mask : Integer_32) return m128i is
     (Convert (AVX2.Longs.Swizzle.Extract (Convert (Elements), Mask)));

   function Insert (Left : m256i; Right : m128i; Mask : Integer_32) return m256i is
     (Convert (AVX2.Longs.Swizzle.Insert (Convert (Left), Convert (Right), Mask)));

   function Permute_Lanes (Left, Right : m256i; Mask : Integer_32) return m256i is
     (Convert (AVX2.Longs.Swizzle.Permute_Lanes (Convert (Left), Convert (Right), Mask)));

end Orka.SIMD.AVX2.Integers.Swizzle;
