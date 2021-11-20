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

with Orka.SIMD.SSE2.Integers;

package Orka.SIMD.AVX2.Integers.Shift is
   pragma Pure;

   use SIMD.SSE2.Integers;

   function Shift_Bits_Left_Zeros (Elements : m256i; Bits : m128i) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pslld256";
   --  Shift each element to the left by the given amount of bits from the
   --  first Double of the Bits register

   function Shift_Bits_Right_Zeros (Elements : m256i; Bits : m128i) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psrld256";
   --  Shift each element to the right by the given amount of bits from the
   --  first Double of the Bits register, shifting in zeros

   function Shift_Bits_Right_Signs (Elements : m256i; Bits : m128i) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psrad256";
   --  Shift each element to the right by the given amount of bits from the
   --  first Double of the Bits register, shifting in sign bits
   --
   --  If the value of the Double is > 31, then the elements will be either 16#FFFF_FFFF# or 0.

   ----------------------------------------------------------------------------

   function Shift_Bits_Left_Zeros (Elements, Bits : m256i) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psllv8si";
   --  Shift each element to the right by the given amount of bits from the
   --  corresponding element in Bits, shifting in zero bits
   --
   --  If the element in Bits is > 31, then the elements will be 0.

   function Shift_Bits_Right_Zeros (Elements, Bits : m256i) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psrlv8si";
   --  Shift each element to the right by the given amount of bits from the
   --  corresponding element in Bits, shifting in zero bits
   --
   --  If the element in Bits is > 31, then the elements will be 0.

   function Shift_Bits_Right_Signs (Elements, Bits : m256i) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psrav8si";
   --  Shift each element to the right by the given amount of bits from the
   --  corresponding element in Bits, shifting in sign bits
   --
   --  If the element in Bits is > 31, then the elements will be either 16#FFFF_FFFF# or 0.

   ----------------------------------------------------------------------------

   type Bits_Count is new Integer range 1 .. 32;

   function Shift_Bits_Left_Zeros (Elements : m256i; Bits : Bits_Count) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pslldi256";
   --  Shift each element to the left by the given amount of bits, shifting in zeros

   function Shift_Bits_Right_Zeros (Elements : m256i; Bits : Bits_Count) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psrldi256";
   --  Shift each element to the right by the given amount of bits, shifting in zeros

   function Shift_Bits_Right_Signs (Elements : m256i; Bits : Bits_Count) return m256i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psradi256";
   --  Shift each element to the right by the given amount of bits, shifting in
   --  sign bits
   --
   --  If Bits is > 31, then the elements will be either 16#FFFF_FFFF# or 0.

   ----------------------------------------------------------------------------

   function Shift_Elements_Left_Zeros (Elements : m256i) return m256i
     with Inline_Always;
   --  Shift each element to the left by one element, shifting in zeros

   function Shift_Elements_Right_Zeros (Elements : m256i) return m256i
     with Inline_Always;
   --  Shift each element to the right by one element, shifting in zeros

end Orka.SIMD.AVX2.Integers.Shift;
