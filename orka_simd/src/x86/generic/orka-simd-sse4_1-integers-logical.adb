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

package body Orka.SIMD.SSE4_1.Integers.Logical is

   type m128l is array (Index_2D) of Integer_64
     with Alignment => 16;
   pragma Machine_Attribute (m128l, "vector_type");

   function Test_All_Zero (Elements, Mask : m128l) return Integer
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_ptestz128";

   function Test_All_Ones (Elements, Mask : m128l) return Integer
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_ptestc128";

   function Test_Mix_Ones_Zeros (Elements, Mask : m128l) return Integer
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_ptestnzc128";

   ----------------------------------------------------------------------------

   function Convert is new Ada.Unchecked_Conversion (m128i, m128l);

   function Test_All_Zero (Elements, Mask : m128i) return Boolean is
     (Test_All_Zero (Convert (Elements), Convert (Mask)) = 1);

   function Test_All_Ones (Elements, Mask : m128i) return Boolean is
     (Test_All_Ones (Convert (Elements), Convert (Mask)) = 1);

   function Test_Mix_Ones_Zeros (Elements, Mask : m128i) return Boolean is
     (Test_Mix_Ones_Zeros (Convert (Elements), Convert (Mask)) = 1);

end Orka.SIMD.SSE4_1.Integers.Logical;
