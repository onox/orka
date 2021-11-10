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

package body Orka.SIMD.SSE2.Integers.Logical is

   type Unsigned_128 is array (Index_2D) of Unsigned_64
     with Alignment => 16;
   pragma Machine_Attribute (Unsigned_128, "vector_type");

   function And_Not (Left, Right : Unsigned_128) return Unsigned_128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pandn128";

   function "and" (Left, Right : Unsigned_128) return Unsigned_128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pand128";

   function "or" (Left, Right : Unsigned_128) return Unsigned_128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pand128";

   function "xor" (Left, Right : Unsigned_128) return Unsigned_128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pand128";

   ----------------------------------------------------------------------------

   function Convert is new Ada.Unchecked_Conversion (m128i, Unsigned_128);
   function Convert is new Ada.Unchecked_Conversion (Unsigned_128, m128i);

   function And_Not (Left, Right : m128i) return m128i is
     (Convert (And_Not (Convert (Left), Convert (Right))));

   function "and" (Left, Right : m128i) return m128i is
     (Convert (Convert (Left) and Convert (Right)));

   function "or" (Left, Right : m128i) return m128i is
     (Convert (Convert (Left) or Convert (Right)));

   function "xor" (Left, Right : m128i) return m128i is
     (Convert (Convert (Left) xor Convert (Right)));

end Orka.SIMD.SSE2.Integers.Logical;