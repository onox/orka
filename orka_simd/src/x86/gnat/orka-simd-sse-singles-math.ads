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

package Orka.SIMD.SSE.Singles.Math is
   pragma Pure;

   function Min (Left, Right : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_minps";
   --  Compare each 32-bit float in Left and Right and take the minimum values.
   --
   --  Result (I) := Float'Min (Left (I), Right (I)) for I in 1 ..4

   function Max (Left, Right : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_maxps";
   --  Compare each 32-bit float in Left and Right and take the maximum values.
   --
   --  Result (I) := Float'Max (Left (I), Right (I)) for I in 1 ..4

   function Reciprocal (Elements : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_rcpps";
   --  Return the reciprocal (1/X) of each element

   function Reciprocal_Sqrt (Elements : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_rsqrtps";
   --  Return the reciprocal of the square root (1/Sqrt(X)) of each element

   function Sqrt (Elements : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_sqrtps";
   --  Return the square root (Sqrt(X)) of each element

   function Cross_Product (Left, Right : m128) return m128
     with Inline_Always;

end Orka.SIMD.SSE.Singles.Math;
