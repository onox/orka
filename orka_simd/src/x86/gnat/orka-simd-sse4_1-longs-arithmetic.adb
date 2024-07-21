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

with Orka.SIMD.SSE2.Integers;

package body Orka.SIMD.SSE4_1.Longs.Arithmetic is

   use SIMD.SSE2.Integers;

   function Convert is new Ada.Unchecked_Conversion (m128l, m128i);

   function "*" (Left, Right : m128i) return m128l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pmuldq128";

   ----------------------------------------------------------------------------

   function "*" (Left, Right : m128l) return m128l is (Convert (Left) * Convert (Right));

end Orka.SIMD.SSE4_1.Longs.Arithmetic;
