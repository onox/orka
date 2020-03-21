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

with Orka.SIMD.SSE2.Doubles;

package Orka.SIMD.SSE3.Doubles.Arithmetic is
   pragma Pure;

   use Orka.SIMD.SSE2.Doubles;

   function Add_Subtract (Left, Right : m128d) return m128d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_addsubpd";
   --  Subtract the doubles in the lower half and add the doubles in
   --  the upper half

   function Horizontal_Add (Left, Right : m128d) return m128d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_haddpd";
   --  Compute the sum of the two doubles in Left and store it in the
   --  lower half of the result. Store the sum of Right in the upper half.

   function Horizontal_Subtract (Left, Right : m128d) return m128d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_hsubpd";
   --  Compute the difference of the two doubles in Left and store the
   --  result in the lower half of the result. Store the difference of Right
   --  in the upper half.

end Orka.SIMD.SSE3.Doubles.Arithmetic;
