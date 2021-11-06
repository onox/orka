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

package body Orka.SIMD.AVX2.Integers.Swizzle is

   use SIMD.AVX.Longs;

   type m128l is array (Index_2D) of Integer_64
     with Alignment => 16;
   pragma Machine_Attribute (m128l, "vector_type");

   function Extract (Elements : m256l; Mask : Unsigned_32) return m128l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_extract128i256";

   function Insert (Left : m256l; Right : m128l; Mask : Unsigned_32) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_insert128i256";

   function Permute_Lanes (Left, Right : m256l; Mask : Unsigned_32) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_permti256";

   ----------------------------------------------------------------------------

   function Convert is new Ada.Unchecked_Conversion (m256i, m256l);
   function Convert is new Ada.Unchecked_Conversion (m256l, m256i);

   function Convert is new Ada.Unchecked_Conversion (m128i, m128l);
   function Convert is new Ada.Unchecked_Conversion (m128l, m128i);

   function Extract (Elements : m256i; Mask : Lane) return m128i is
     (Convert
        ((case Mask is
            when Lower => Extract (Convert (Elements), 0),
            when Upper => Extract (Convert (Elements), 1))));

   function Insert (Left : m256i; Right : m128i; Mask : Lane) return m256i is
     (Convert
        ((case Mask is
            when Lower => Insert (Convert (Left), Convert (Right), 0),
            when Upper => Insert (Convert (Left), Convert (Right), 1))));

   function Permute_Lanes (Left, Right : m256i; Mask : Unsigned_8) return m256i is
     (Convert
        (case Mask is
           when 0 => Permute_Lanes (Convert (Left), Convert (Right), 0),
           when 1 => Permute_Lanes (Convert (Left), Convert (Right), 1),
           when 2 => Permute_Lanes (Convert (Left), Convert (Right), 2),
           when 3 => Permute_Lanes (Convert (Left), Convert (Right), 3),
           when 4 => Permute_Lanes (Convert (Left), Convert (Right), 4),
           when 5 => Permute_Lanes (Convert (Left), Convert (Right), 5),
           when 6 => Permute_Lanes (Convert (Left), Convert (Right), 6),
           when 7 => Permute_Lanes (Convert (Left), Convert (Right), 7)));

end Orka.SIMD.AVX2.Integers.Swizzle;
