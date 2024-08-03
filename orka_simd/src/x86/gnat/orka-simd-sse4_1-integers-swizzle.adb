--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2024 onox <denkpadje@gmail.com>
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

package body Orka.SIMD.SSE4_1.Integers.Swizzle is

   type Index_8D is range 1 .. 8;

   type m128hi is array (Index_8D) of Integer_16
     with Alignment => 16;
   pragma Machine_Attribute (m128hi, "vector_type");

   type Index_16D is range 1 .. 16;

   type m128qi is array (Index_16D) of Integer_8
     with Alignment => 16;
   pragma Machine_Attribute (m128qi, "vector_type");

   function Convert is new Ada.Unchecked_Conversion (m128hi, m128i);
   function Convert is new Ada.Unchecked_Conversion (m128i, m128hi);

   function Convert is new Ada.Unchecked_Conversion (m128qi, m128i);
   function Convert is new Ada.Unchecked_Conversion (m128i, m128qi);

   function Blend (Left, Right : m128hi; Mask : Blend_8_Mask) return m128hi
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pblendw128";

   function Blend (Left, Right, Mask : m128qi) return m128qi
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pblendvb128";

   ----------------------------------------------------------------------------

   function Blend (Left, Right : m128i; Mask : Blend_8_Mask) return m128i is
     (Convert (Blend (Convert (Left), Convert (Right), Mask)));

   function Blend (Left, Right, Mask : m128i) return m128i is
     (Convert (Blend (Convert (Left), Convert (Right), Convert (Mask))));

end Orka.SIMD.SSE4_1.Integers.Swizzle;
