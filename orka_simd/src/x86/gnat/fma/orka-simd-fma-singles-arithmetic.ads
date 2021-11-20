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

with Orka.SIMD.SSE.Singles;
with Orka.SIMD.AVX.Singles;

package Orka.SIMD.FMA.Singles.Arithmetic is
   pragma Pure;

   use SSE.Singles;
   use AVX.Singles;

   function Multiply_Add (A, B, C : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vfmaddps";

   function Multiply_Add_Sub (A, B, C : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vfmaddsubps";

   function Multiply_Add (A, B, C : m256) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vfmaddps256";

   function Multiply_Add_Sub (A, B, C : m256) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vfmaddsubps256";

   function "*" (Left, Right : m128_Array) return m128_Array
     with Inline_Always;
   --  Multiplies the left matrix with the right matrix. Matrix multiplication
   --  is associative, but not commutative.

   function "*" (Left : m128_Array; Right : m128) return m128
     with Inline_Always;
   --  Multiplies the left matrix with the right vector. Matrix multiplication
   --  is associative, but not commutative.

end Orka.SIMD.FMA.Singles.Arithmetic;
