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

with Orka.SIMD.AVX2.Integers.Logical;

package Orka.SIMD.AVX2.Integers.Compare is
   pragma Pure;

   use SIMD.AVX2.Integers.Logical;

   function "=" (Left, Right : m256i) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pcmpeqd256";

   function "not" (Elements : m256i) return m256i is (Elements xor (Elements = Elements))
     with Inline_Always;

   function "/=" (Left, Right : m256i) return m256i is (not (Left = Right))
     with Inline_Always;

   function ">" (Left, Right : m256i) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pcmpgtd256";

   function "<" (Left, Right : m256i) return m256i is (Right > Left)
     with Inline_Always;

   function ">=" (Left, Right : m256i) return m256i is (not (Right > Left))
     with Inline_Always;

   function "<=" (Left, Right : m256i) return m256i is (not (Left > Right))
     with Inline_Always;

end Orka.SIMD.AVX2.Integers.Compare;
