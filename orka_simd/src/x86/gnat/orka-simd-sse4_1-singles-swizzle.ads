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

package Orka.SIMD.SSE4_1.Singles.Swizzle is
   pragma Pure;

   use Orka.SIMD.SSE.Singles;

   function Blend (Left, Right : m128; Mask : Integer_32) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_blendps";
   --  Select elements from two sources (Left and Right) using a constant mask

   function Blend (Left, Right, Mask : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_blendvps";
   --  Select elements from two sources (Left and Right) using a variable mask

   type Index_Type is new Integer_32 range 0 .. 3;

   function Extract (Elements : m128; Index : Index_Type) return Float_32
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vec_ext_v4sf";

   function Insert (Left, Right : m128; Mask : Integer_32) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_insertps128";
   --  Insert an element from Right into Left. Bits 6-7 of Mask
   --  define the index of the source (Right), bits 4-5 define
   --  the index of the destination (Left). Bits 0-3 define the
   --  zero mask.

end Orka.SIMD.SSE4_1.Singles.Swizzle;
