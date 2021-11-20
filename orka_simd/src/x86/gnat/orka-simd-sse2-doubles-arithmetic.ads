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

package Orka.SIMD.SSE2.Doubles.Arithmetic is
   pragma Pure;

   function "*" (Left, Right : m128d) return m128d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_mulpd";

   function "/" (Left, Right : m128d) return m128d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_divpd";

   function Divide_Or_Zero (Left, Right : m128d) return m128d
     with Inline_Always;

   function "+" (Left, Right : m128d) return m128d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_addpd";

   function "-" (Left, Right : m128d) return m128d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_subpd";

   function "-" (Elements : m128d) return m128d is
     ((0.0, 0.0) - Elements)
   with Inline;

   function "abs" (Elements : m128d) return m128d
     with Inline_Always;

   function Sum (Elements : m128d) return Float_64
     with Inline_Always;

end Orka.SIMD.SSE2.Doubles.Arithmetic;
