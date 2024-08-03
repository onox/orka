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

package Orka.SIMD is
   pragma Pure;

   type Element_Index_Vector_2 is range 0 .. 1;
   type Element_Index_Vector_4 is range 0 .. 3;

   type Swizzle_2_Mask is new Integer_32 range 0 .. 2 ** 2 - 1;
   --  2 bits mask, one bit for each element of the result

   type Swizzle_4_Mask is new Integer_32 range 0 .. 2 ** 8 - 1;
   --  8 bits mask, two bits for each element of the result

   function Mask (A, B : Element_Index_Vector_2) return Swizzle_2_Mask is
     (Swizzle_2_Mask (A + B * 2))
   with Static;

   function Mask (A, B, C, D : Element_Index_Vector_4) return Swizzle_4_Mask is
     (Swizzle_4_Mask (A + B * 4 + C * 16 + D * 64))
   with Static;

   type Lane_Index_4 is new Integer_32 range 0 .. 3;
   type Zero_Lane    is new Integer_32 range 0 .. 1;

   type Swizzle_Lanes_4_Mask is new Integer_32 range 0 .. 2 ** 8 - 1;
   --  8 bits mask, bits 1-2 and 5-6 are used to select a lower and upper lane,
   --  and bits 4 and 8 to zero the corresponding lane

   function Mask (Lower, Upper : Lane_Index_4; Zero_Lower, Zero_Upper : Zero_Lane) return Swizzle_Lanes_4_Mask is
     (Swizzle_Lanes_4_Mask (Lower + Upper * 16
        + Lane_Index_4 (Zero_Lower) * 8 + Lane_Index_4 (Zero_Upper) * 128))
   with Static;

   -----------------------------------------------------------------------------

   type Left_Or_Right is new Integer_32 range 0 .. 1;

   type Blend_2_Mask is new Integer_32 range 0 .. 2 ** 2 - 1;
   --  2 bits mask where each bit is 0 (left source) or 1 (right source)

   type Blend_4_Mask is new Integer_32 range 0 .. 2 ** 4 - 1;
   --  4 bits mask where each bit is 0 (left source) or 1 (right source)

   type Blend_8_Mask is new Integer_32 range 0 .. 2 ** 8 - 1;
   --  8 bits mask where each bit is 0 (left source) or 1 (right source)

   function Mask (A, B : Left_Or_Right) return Blend_2_Mask is
     (Blend_2_Mask (A + B * 2))
   with Static;

   function Mask (A, B, C, D : Left_Or_Right) return Blend_4_Mask is
     (Blend_4_Mask (A + B * 2 + C * 4 + D * 8))
   with Static;

   function Mask (A, B, C, D, E, F, G, H : Left_Or_Right) return Blend_8_Mask is
     (Blend_8_Mask (A + B * 2 + C * 4 + D * 8 + E * 16 + F * 32 + G * 64 + H * 128))
   with Static;

   function Mask (A, B : Left_Or_Right) return Blend_8_Mask is (Mask (A, A, A, A, B, B, B, B))
     with Static;

   function Mask (A, B, C, D : Left_Or_Right) return Blend_8_Mask is (Mask (A, A, B, B, C, C, D, D))
     with Static;

   -----------------------------------------------------------------------------

   type Lane is (Lower, Upper);

   type Lane_Mask is new Integer_32 range 0 .. 1;
   --  1 bit mask where bit is 0 (lower half) or 1 (upper half)

   function Mask (Selected_Lane : Lane) return Lane_Mask is
     (case Selected_Lane is
        when Lower => 0,
        when Upper => 1)
   with Static;

end Orka.SIMD;
