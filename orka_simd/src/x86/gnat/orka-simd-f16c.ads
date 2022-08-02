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

with Orka.SIMD.AVX.Singles;
with Orka.SIMD.SSE.Singles;

package Orka.SIMD.F16C is
   pragma Pure;

   use SIMD.AVX;
   use SIMD.AVX.Singles;
   use SIMD.SSE.Singles;

   type m128s is array (Index_8D) of Integer_16
     with Alignment => 16;
   pragma Machine_Attribute (m128s, "vector_type");

   function Convert (Elements : m128s) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vcvtph2ps";
   --  Convert 4x 16-bit floats (in lower half) to 4x 32-bit floats

   function Convert (Elements : m128s) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vcvtph2ps256";
   --  Convert 8x 16-bit floats to 8x 32-bit

   -----------------------------------------------------------------------------

   function Convert (Elements : m128; Rounding : Integer_32) return m128s
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vcvtps2ph";
   --  Convert 4x 32-bit floats to 4x 16-bit floats (in lower half)

   function Convert_Nearest_Integer (Elements : m128) return m128s is
     (Convert (Elements, 0))
   with Inline;

   function Convert_Down (Elements : m128) return m128s is
     (Convert (Elements, 1))
   with Inline;

   function Convert_Up (Elements : m128) return m128s is
     (Convert (Elements, 2))
   with Inline;

   function Convert_Truncate (Elements : m128) return m128s is
     (Convert (Elements, 3))
   with Inline;

   -----------------------------------------------------------------------------

   function Convert (Elements : m256; Rounding : Integer_32) return m128s
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_vcvtps2ph256";
   --  Convert 8x 32-bit floats to 8x 16-bit

   function Convert_Nearest_Integer (Elements : m256) return m128s is
     (Convert (Elements, 0))
   with Inline;

   function Convert_Down (Elements : m256) return m128s is
     (Convert (Elements, 1))
   with Inline;

   function Convert_Up (Elements : m256) return m128s is
     (Convert (Elements, 2))
   with Inline;

   function Convert_Truncate (Elements : m256) return m128s is
     (Convert (Elements, 3))
   with Inline;

end Orka.SIMD.F16C;
