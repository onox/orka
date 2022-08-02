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

package Orka.SIMD.AVX.Singles.Arithmetic is
   pragma Pure;

   function "*" (Left, Right : m256) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_mulps256";

   function "/" (Left, Right : m256) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_divps256";

   function Divide_Or_Zero (Left, Right : m256) return m256
     with Inline_Always;

   function "+" (Left, Right : m256) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_addps256";

   function "-" (Left, Right : m256) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_subps256";

   function "-" (Elements : m256) return m256 is
     ((0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0) - Elements);

   function "abs" (Elements : m256) return m256
     with Inline_Always;

   function Sum (Elements : m256) return Float_32
     with Inline_Always;

   function Add_Subtract (Left, Right : m256) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_addsubps256";
   --  Subtract and add 32-bit floats from Left and Right

   function Horizontal_Add (Left, Right : m256) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_haddps256";
   --  Compute the sums of adjacent 32-bit floats in Left and Right.
   --  The four sums (eight elements gives four pairs) of elements
   --  from Left are stored in the four floats in the first, second, fifth,
   --  and sixth position, sums from Right in the third, fourth,
   --  seventh, and eight.

   function Horizontal_Subtract (Left, Right : m256) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_hsubps256";
   --  Compute the differences of adjacent 32-bit floats in Left and Right.
   --  The four differences (eight elements gives four pairs) of elements
   --  from Left are stored in the four floats in the first, second, fifth,
   --  and sixth position, differences from Right in the third, fourth,
   --  seventh, and eight.

   function Dot (Left, Right : m256; Mask : Integer_32) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_dpps256";

end Orka.SIMD.AVX.Singles.Arithmetic;
