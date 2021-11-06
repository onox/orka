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

with Ada.Unchecked_Conversion;

with Orka.SIMD.AVX.Longs;

package body Orka.SIMD.AVX2.Integers.Shift is

   use SIMD.AVX.Longs;

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

   function Convert is new Ada.Unchecked_Conversion (m256i, m256l);
   function Convert is new Ada.Unchecked_Conversion (m256l, m256i);

   function Shift_Elements_Left_Zeros (Elements : m256i) return m256i is
      --  Shifts are done per 128-bit lane, so save 4 and store it afterwards in 5
      Last_Element_First_Lane : constant Integer_32 := Elements (4);
   begin
      return Result : m256i :=
        Convert (Shift_All_Bits_Left_Zeros (Convert (Elements), Integer_32'Size))
      do
         Result (5) := Last_Element_First_Lane;
      end return;
   end Shift_Elements_Left_Zeros;

   function Shift_Elements_Right_Zeros (Elements : m256i) return m256i is
      --  Shifts are done per 128-bit lane, so save 5 and store it afterwards in 4
      First_Element_Last_Lane : constant Integer_32 := Elements (5);
   begin
      return Result : m256i :=
        Convert (Shift_All_Bits_Right_Zeros (Convert (Elements), Integer_32'Size))
      do
         Result (4) := First_Element_Last_Lane;
      end return;
   end Shift_Elements_Right_Zeros;

end Orka.SIMD.AVX2.Integers.Shift;
