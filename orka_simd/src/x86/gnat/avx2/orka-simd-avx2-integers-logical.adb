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

package body Orka.SIMD.AVX2.Integers.Logical is

   use SIMD.AVX.Longs;

   function And_Not (Left, Right : m256l) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_andnotsi256";

   function "and" (Left, Right : m256l) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_andsi256";

   function "or" (Left, Right : m256l) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_por256";

   function "xor" (Left, Right : m256l) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pxor256";

   ----------------------------------------------------------------------------

   function Convert is new Ada.Unchecked_Conversion (m256i, m256l);
   function Convert is new Ada.Unchecked_Conversion (m256l, m256i);

   function And_Not (Left, Right : m256i) return m256i is
     (Convert (And_Not (Convert (Left), Convert (Right))));

   function "and" (Left, Right : m256i) return m256i is
     (Convert (Convert (Left) and Convert (Right)));

   function "or" (Left, Right : m256i) return m256i is
     (Convert (Convert (Left) or Convert (Right)));

   function "xor" (Left, Right : m256i) return m256i is
     (Convert (Convert (Left) xor Convert (Right)));

end Orka.SIMD.AVX2.Integers.Logical;
