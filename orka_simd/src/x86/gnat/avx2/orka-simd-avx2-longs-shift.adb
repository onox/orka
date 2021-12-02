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

package body Orka.SIMD.AVX2.Longs.Shift is

   type All_Bits_Count is new Integer range 0 .. 128;

   function Shift_All_Bits_Left_Zeros
     (Elements : m256l; Bits : All_Bits_Count) return m256l
   with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pslldqi256";
   --  Shift each bit to the left by the given amount of bits, shifting in zeros

   function Shift_All_Bits_Right_Zeros
     (Elements : m256l; Bits : All_Bits_Count) return m256l
   with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_psrldqi256";
   --  Shift each bit to the right by the given amount of bits, shifting in zeros

   ----------------------------------------------------------------------------

   function Shift_Elements_Left_Zeros (Elements : m256l) return m256l is
      --  Shifts are done per 128-bit lane, so save Y and store it afterwards in Z
      Last_Element_First_Lane : constant Integer_64 := Elements (Y);
   begin
      return Result : m256l :=
        Shift_All_Bits_Left_Zeros (Elements, Integer_64'Size)
      do
         Result (Z) := Last_Element_First_Lane;
      end return;
   end Shift_Elements_Left_Zeros;

   function Shift_Elements_Right_Zeros (Elements : m256l) return m256l is
      --  Shifts are done per 128-bit lane, so save Z and store it afterwards in Y
      First_Element_Last_Lane : constant Integer_64 := Elements (Z);
   begin
      return Result : m256l :=
        Shift_All_Bits_Right_Zeros (Elements, Integer_64'Size)
      do
         Result (Y) := First_Element_Last_Lane;
      end return;
   end Shift_Elements_Right_Zeros;

end Orka.SIMD.AVX2.Longs.Shift;
