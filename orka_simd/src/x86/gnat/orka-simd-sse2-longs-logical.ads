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

package Orka.SIMD.SSE2.Longs.Logical is
   pragma Pure;

   function And_Not (Left, Right : m128l) return m128l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pandn128";

   function "and" (Left, Right : m128l) return m128l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pand128";

   function "or" (Left, Right : m128l) return m128l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_por128";

   function "xor" (Left, Right : m128l) return m128l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pxor128";

end Orka.SIMD.SSE2.Longs.Logical;
