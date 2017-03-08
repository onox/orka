--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

with Orka.Transforms.SIMD_Vectors;

with Orka.SIMD.AVX.Doubles.Arithmetic;

package Orka.Transforms.Doubles.Vectors is new Orka.Transforms.SIMD_Vectors
  (GL.Types.Double, SIMD.AVX.Doubles.m256d,
   SIMD.AVX.Doubles.Arithmetic."*", SIMD.AVX.Doubles.Arithmetic."+",
   SIMD.AVX.Doubles.Arithmetic."-", SIMD.AVX.Doubles.Arithmetic."-",
   SIMD.AVX.Doubles.Arithmetic."abs", SIMD.AVX.Doubles.Arithmetic.Sum,
   SIMD.AVX.Doubles.Arithmetic.Divide_Or_Zero);
pragma Preelaborate (Orka.Transforms.Doubles.Vectors);
