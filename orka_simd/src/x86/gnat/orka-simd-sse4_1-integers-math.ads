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

with Orka.SIMD.SSE2.Integers;

package Orka.SIMD.SSE4_1.Integers.Math is
   pragma Pure;

   use SIMD.SSE2.Integers;

   function Min (Left, Right : m128i) return m128i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pminsd128";
   --  Compare each 32-bit integer in Left and Right and take the minimum values.
   --
   --  Result (I) := Integer_32'Min (Left (I), Right (I)) for I in 1 ..4

   function Max (Left, Right : m128i) return m128i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pmaxsd128";
   --  Compare each 32-bit integer in Left and Right and take the maximum values.
   --
   --  Result (I) := Integer_32'Max (Left (I), Right (I)) for I in 1 ..4

end Orka.SIMD.SSE4_1.Integers.Math;
