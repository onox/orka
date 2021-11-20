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

with Orka.SIMD.SSE.Singles;

package Orka.SIMD.SSE3.Singles.Arithmetic is
   pragma Pure;

   use Orka.SIMD.SSE.Singles;

   function Add_Subtract (Left, Right : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_addsubps";
   --  Subtract and add 32-bit floats from Left and Right

   function Horizontal_Add (Left, Right : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_haddps";
   --  Compute the sums of adjacent 32-bit floats in Left and Right
   --
   --  The two sums (four elements gives two pairs) of elements from Left
   --  are stored in the two floats in the lower half, sums from Right in
   --  the upper half.

   function Horizontal_Subtract (Left, Right : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_hsubps";
   --  Compute the differences of adjacent 32-bit floats in Left and Right
   --
   --  The two differences (four elements gives two pairs) of elements
   --  from Left are stored in the two floats in the lower half, differences
   --  from Right in the upper half.

   function Sum (Elements : m128) return Float_32
     with Inline_Always;

end Orka.SIMD.SSE3.Singles.Arithmetic;
