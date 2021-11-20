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

package Orka.SIMD.AVX.Doubles.Arithmetic is
   pragma Pure;

   function "*" (Left, Right : m256d) return m256d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_mulpd256";

   function "*" (Left, Right : m256d_Array) return m256d_Array
     with Inline_Always;
   --  Multiplies the left matrix with the right matrix. Matrix multiplication
   --  is associative, but not commutative.

   function "*" (Left : m256d_Array; Right : m256d) return m256d
     with Inline_Always;
   --  Multiplies the left matrix with the right vector. Matrix multiplication
   --  is associative, but not commutative.

   function "/" (Left, Right : m256d) return m256d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_divpd256";

   function Divide_Or_Zero (Left, Right : m256d) return m256d
     with Inline_Always;

   function "+" (Left, Right : m256d) return m256d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_addpd256";

   function "-" (Left, Right : m256d) return m256d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_subpd256";

   function "-" (Elements : m256d) return m256d is
     ((0.0, 0.0, 0.0, 0.0) - Elements)
   with Inline;

   function "abs" (Elements : m256d) return m256d
     with Inline_Always;

   function Sum (Elements : m256d) return Float_64
     with Inline_Always;

   function Add_Subtract (Left, Right : m256d) return m256d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_addsubpd256";
   --  Subtract and add 64-bit doubles from Left and Right

   function Horizontal_Add (Left, Right : m256d) return m256d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_haddpd256";
   --  Compute the sums of adjacent 64-bit doubles in Left and Right.
   --  The two sums (four elements gives two pairs) of elements
   --  from Left are stored in the two doubles in the first and third
   --  position, sums from Right in the second and fourth.

   function Horizontal_Subtract (Left, Right : m256d) return m256d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_hsubpd256";
   --  Compute the differences of adjacent 64-bit doubles in Left and Right.
   --  The two differences (four elements gives two pairs) of elements
   --  from Left are stored in the two doubles in the first and third
   --  position, differences from Right in the second and fourth.

end Orka.SIMD.AVX.Doubles.Arithmetic;
