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

package Orka.SIMD.AVX2.Integers.Arithmetic is
   pragma Pure;

   function "abs" (Elements : m256i) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pabsd256";

   function "+" (Left, Right : m256i) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_paddd256";

   function "-" (Left, Right : m256i) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psubd256";

   function "-" (Elements : m256i) return m256i is
     ((others => 0) - Elements)
   with Inline;

   function "*" (Left, Right : m256i) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pmulld256";

   function Horizontal_Add (Left, Right : m256i) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_phaddd256";

   function Horizontal_Subtract (Left, Right : m256i) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_phsubd256";

   function Negate_Signs (Elements, Signs : m256i) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psignd256";

end Orka.SIMD.AVX2.Integers.Arithmetic;
