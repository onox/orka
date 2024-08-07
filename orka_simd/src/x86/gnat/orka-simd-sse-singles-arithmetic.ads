--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2016 onox <denkpadje@gmail.com>
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

package Orka.SIMD.SSE.Singles.Arithmetic is
   pragma Pure;

   function "*" (Left, Right : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_mulps";

   function "*" (Left, Right : m128_Array) return m128_Array
     with Inline_Always;
   --  Multiplies the left matrix with the right matrix. Matrix multiplication
   --  is associative, but not commutative.

   function "*" (Left : m128_Array; Right : m128) return m128
     with Inline_Always;
   --  Multiplies the left matrix with the right vector. Matrix multiplication
   --  is associative, but not commutative.

   function "/" (Left, Right : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_divps";

   function Divide_Or_Zero (Left, Right : m128) return m128
     with Inline_Always;

   function "+" (Left, Right : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_addps";

   function "-" (Left, Right : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_subps";

   function "-" (Elements : m128) return m128 is
     ([others => 0.0] - Elements)
   with Inline_Always;

   function "abs" (Elements : m128) return m128
     with Inline_Always;

   function Sum (Elements : m128) return Float_32
     with Inline_Always;

end Orka.SIMD.SSE.Singles.Arithmetic;
