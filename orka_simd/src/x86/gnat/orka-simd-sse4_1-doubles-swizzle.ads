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

package Orka.SIMD.SSE4_1.Doubles.Swizzle is
   pragma Pure;

   use Orka.SIMD.SSE2.Doubles;

   function Blend (Left, Right : m128d; Mask : Blend_2_Mask) return m128d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_blendpd";
   --  Select elements from two sources (Left and Right) using a constant mask
   --
   --  The compiler needs access to the Mask at compile-time, thus construct it
   --  using the function Mask where each of the parameters A and B is
   --  either a 0 to select an element from Left or a 1 to select an element from Right.

   function Blend (Left, Right, Mask : m128d) return m128d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_blendvpd";
   --  Select elements from two sources (Left and Right) using a variable mask

end Orka.SIMD.SSE4_1.Doubles.Swizzle;
