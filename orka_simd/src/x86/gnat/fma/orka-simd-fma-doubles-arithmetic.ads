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

with Orka.SIMD.SSE2.Doubles;
with Orka.SIMD.AVX.Doubles;

package Orka.SIMD.FMA.Doubles.Arithmetic is
   pragma Pure;

   use SSE2.Doubles;
   use AVX.Doubles;

   function Multiply_Add (A, B, C : m128d) return m128d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vfmaddpd";

   function Multiply_Add_Sub (A, B, C : m128d) return m128d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vfmaddsubpd";

   function Multiply_Add (A, B, C : m256d) return m256d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vfmaddpd256";

   function Multiply_Add_Sub (A, B, C : m256d) return m256d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vfmaddsubpd256";

   function "*" (Left, Right : m256d_Array) return m256d_Array
     with Inline_Always;
   --  Multiplies the left matrix with the right matrix. Matrix multiplication
   --  is associative, but not commutative.

   function "*" (Left : m256d_Array; Right : m256d) return m256d
     with Inline_Always;
   --  Multiplies the left matrix with the right vector. Matrix multiplication
   --  is associative, but not commutative.

end Orka.SIMD.FMA.Doubles.Arithmetic;
