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

package Orka.SIMD.SSE2.Doubles.Math is
   pragma Pure;

   function Min (Left, Right : m128d) return m128d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_minpd";
   --  Compare each 64-bit double in Left and Right and take the minimum values.
   --
   --  Result (I) := Double'Min (Left (I), Right (I)) for I in 1 ..4

   function Max (Left, Right : m128d) return m128d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_maxpd";
   --  Compare each 64-bit double in Left and Right and take the maximum values.
   --
   --  Result (I) := Double'Max (Left (I), Right (I)) for I in 1 ..4

   function Sqrt (Elements : m128d) return m128d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_sqrtpd";
   --  Return the square root (Sqrt(X)) of each element

end Orka.SIMD.SSE2.Doubles.Math;
