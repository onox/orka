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

package Orka.SIMD.SSE2.Integers is
   pragma Pure;

   type m128i is array (Index_4D) of Integer_32
     with Alignment => 16;
   pragma Machine_Attribute (m128i, "vector_type");

   function Identity (Elements : m128i) return m128i is (Elements)
     with Inline_Always;

   --  The functions Unsupported_Operation below are meant to be used as actual for generic formal
   --  parameters when instantiating generic packages using SIMD intrinsics

   function Unsupported_Operation return Integer_32 is (raise Program_Error)
     with Inline_Always;

   function Unsupported_Operation (Elements : m128i) return m128i is (raise Program_Error)
     with Inline_Always;

   function Unsupported_Operation (Elements : m128i) return Integer_32 is (raise Program_Error)
     with Inline_Always;

   function Unsupported_Operation (Left, Right : m128i) return m128i is (raise Program_Error)
     with Inline_Always;

   function Unsupported_Operation (Value : Integer_32) return Integer_32 is (raise Program_Error)
     with Inline_Always;

   function Is_Valid (Value : Integer_32) return Boolean is (Value'Valid)
     with Inline_Always;

end Orka.SIMD.SSE2.Integers;
