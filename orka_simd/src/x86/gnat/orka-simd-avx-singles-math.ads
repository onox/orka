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

package Orka.SIMD.AVX.Singles.Math is
   pragma Pure;

   function Min (Left, Right : m256) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_minps256";
   --  Compare each 32-bit float in Left and Right and take the minimum values.
   --
   --  Result (I) := Float'Min (Left (I), Right (I)) for I in 1 ..4

   function Max (Left, Right : m256) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_maxps256";
   --  Compare each 32-bit float in Left and Right and take the maximum values.
   --
   --  Result (I) := Float'Max (Left (I), Right (I)) for I in 1 ..4

   function Reciprocal (Elements : m256) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_rcpps256";
   --  Return the reciprocal (1/X) of each element

   function Reciprocal_Sqrt (Elements : m256) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_rsqrtps256";
   --  Return the reciprocal of the square root (1/Sqrt(X)) of each element

   function Sqrt (Elements : m256) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_sqrtps256";
   --  Return the square root (Sqrt(X)) of each element

   function Round (Elements : m256; Rounding : Integer_32) return m256
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_roundps256";

   function Round_Nearest_Integer (Elements : m256) return m256 is
     (Round (Elements, 0));
   --  Round each element to the nearest integer

   function Floor (Elements : m256) return m256 is
     (Round (Elements, 1));
   --  Round each element down to an integer value

   function Ceil (Elements : m256) return m256 is
     (Round (Elements, 2));
   --  Round each element up to an integer value

   function Round_Truncate (Elements : m256) return m256 is
     (Round (Elements, 3));
   --  Round each element to zero

end Orka.SIMD.AVX.Singles.Math;
