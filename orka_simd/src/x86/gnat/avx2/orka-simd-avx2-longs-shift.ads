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

package Orka.SIMD.AVX2.Longs.Shift is
   pragma Pure;

   function Shift_Bits_Left_Zeros (Elements : m256l; Bits : m128l) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psllq256";
   --  Shift each element to the left by the given amount of bits from the
   --  first Double of the Bits register

   function Shift_Bits_Right_Zeros (Elements : m256l; Bits : m128l) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psrlq256";
   --  Shift each element to the right by the given amount of bits from the
   --  first Double of the Bits register, shifting in zeros

   ----------------------------------------------------------------------------

   function Shift_Bits_Left_Zeros (Elements, Bits : m256l) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psllv4di";
   --  Shift each element to the right by the given amount of bits from the
   --  corresponding element in Bits, shifting in zero bits
   --
   --  If the element in Bits is > 31, then the elements will be 0.

   function Shift_Bits_Right_Zeros (Elements, Bits : m256l) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psrlv4di";
   --  Shift each element to the right by the given amount of bits from the
   --  corresponding element in Bits, shifting in zero bits
   --
   --  If the element in Bits is > 31, then the elements will be 0.

   ----------------------------------------------------------------------------

   type Bits_Count is new Integer range 1 .. 64;

   function Shift_Bits_Left_Zeros (Elements : m256l; Bits : Bits_Count) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psllqi256";
   --  Shift each element to the left by the given amount of bits, shifting in zeros

   function Shift_Bits_Right_Zeros (Elements : m256l; Bits : Bits_Count) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psrlqi256";
   --  Shift each element to the right by the given amount of bits, shifting in zeros

   ----------------------------------------------------------------------------

   function Shift_Elements_Left_Zeros (Elements : m256l) return m256l
     with Inline_Always;
   --  Shift each element to the left by one element, shifting in zeros

   function Shift_Elements_Right_Zeros (Elements : m256l) return m256l
     with Inline_Always;
   --  Shift each element to the right by one element, shifting in zeros

end Orka.SIMD.AVX2.Longs.Shift;
