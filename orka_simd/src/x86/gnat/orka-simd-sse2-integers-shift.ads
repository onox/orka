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

package Orka.SIMD.SSE2.Integers.Shift is
   pragma Pure;

   function Shift_Bits_Left_Zeros (Elements, Bits : m128i) return m128i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pslld128";
   --  Shift each element to the left by the given amount of bits from the
   --  first Double of the Bits register

   function Shift_Bits_Right_Zeros (Elements, Bits : m128i) return m128i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psrld128";
   --  Shift each element to the right by the given amount of bits from the
   --  first Double of the Bits register, shifting in zeros

   function Shift_Bits_Right_Signs (Elements, Bits : m128i) return m128i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psrad128";
   --  Shift each element to the right by the given amount of bits from the
   --  first Double of the Bits register, shifting in sign bits
   --
   --  If the value of the Double is > 31, then the elements will be either 16#FFFF_FFFF# or 0.

   ----------------------------------------------------------------------------

   type Bits_Count is new Integer range 1 .. 32;

   function Shift_Bits_Left_Zeros (Elements : m128i; Bits : Bits_Count) return m128i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pslldi128";
   --  Shift each element to the left by the given amount of bits, shifting in zeros

   function Shift_Bits_Right_Zeros (Elements : m128i; Bits : Bits_Count) return m128i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psrldi128";
   --  Shift each element to the right by the given amount of bits, shifting in zeros

   function Shift_Bits_Right_Signs (Elements : m128i; Bits : Bits_Count) return m128i
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psradi128";
   --  Shift each element to the right by the given amount of bits, shifting in
   --  sign bits
   --
   --  If Bits is > 31, then the elements will be either 16#FFFF_FFFF# or 0.

   ----------------------------------------------------------------------------

   function Shift_Elements_Left_Zeros (Elements : m128i) return m128i
     with Inline_Always;
   --  Shift each element to the left by one element, shifting in zeros

   function Shift_Elements_Right_Zeros (Elements : m128i) return m128i
     with Inline_Always;
   --  Shift each element to the right by one element, shifting in zeros

end Orka.SIMD.SSE2.Integers.Shift;
