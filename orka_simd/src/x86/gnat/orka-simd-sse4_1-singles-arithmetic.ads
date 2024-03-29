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

with Orka.SIMD.SSE.Singles;

package Orka.SIMD.SSE4_1.Singles.Arithmetic is
   pragma Pure;

   use Orka.SIMD.SSE.Singles;

   function Dot (Left, Right : m128; Mask : Integer_32) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_dpps";

end Orka.SIMD.SSE4_1.Singles.Arithmetic;
