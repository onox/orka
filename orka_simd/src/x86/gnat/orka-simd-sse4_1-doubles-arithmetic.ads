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

package Orka.SIMD.SSE4_1.Doubles.Arithmetic is
   pragma Pure;

   use Orka.SIMD.SSE2.Doubles;

   function Dot (Left, Right : m128d; Mask : Integer_32) return m128d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_dppd";

end Orka.SIMD.SSE4_1.Doubles.Arithmetic;
