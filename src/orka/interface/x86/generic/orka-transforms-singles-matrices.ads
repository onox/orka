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

with GL.Types;

with Orka.Transforms.SIMD_Matrices;

with Orka.SIMD.SSE.Singles.Arithmetic;
with Orka.SIMD.SSE.Singles.Swizzle;

package Orka.Transforms.Singles.Matrices is new Orka.Transforms.SIMD_Matrices
  (GL.Types.Single, SIMD.SSE.Singles.m128, SIMD.SSE.Singles.m128_Array,
   SIMD.SSE.Singles.Arithmetic."*", SIMD.SSE.Singles.Arithmetic."-",
   SIMD.SSE.Singles.Swizzle.Transpose);
pragma Preelaborate (Orka.Transforms.Singles.Matrices);
