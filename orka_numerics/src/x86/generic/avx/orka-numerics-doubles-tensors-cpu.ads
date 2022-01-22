--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2022 onox <denkpadje@gmail.com>
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

with Orka.Numerics.Tensors.SIMD_CPU;

with Orka.SIMD.AVX.Doubles.Arithmetic;
with Orka.SIMD.AVX.Doubles.Compare;
with Orka.SIMD.AVX.Doubles.Logical;
with Orka.SIMD.AVX.Doubles.Math;
with Orka.SIMD.AVX.Longs.Logical;
with Orka.SIMD.AVX.Longs.Random.Emulation;
with Orka.SIMD.AVX.Longs.Arithmetic.Emulation;
with Orka.SIMD.AVX.Longs.Logical.Emulation;
with Orka.SIMD.AVX.Longs.Shift.Emulation;

package Orka.Numerics.Doubles.Tensors.CPU is new Orka.Numerics.Doubles.Tensors.SIMD_CPU
  (Index_4D,
   Integer_64,
   SIMD.AVX.Longs.m256l,
   SIMD.AVX.Longs.Arithmetic.Emulation."+",
   SIMD.AVX.Longs.Logical.Emulation."and",
   SIMD.AVX.Longs.Shift.Emulation.Shift_Elements_Left_Zeros,
   SIMD.AVX.Longs.Shift.Emulation.Shift_Elements_Right_Zeros,
   SIMD.AVX.Longs.Logical.Test_All_Ones,
   SIMD.AVX.Longs.Logical.Test_All_Zero,
   SIMD.AVX.Doubles.m256d,
   SIMD.AVX.Doubles.Arithmetic."*",
   SIMD.AVX.Doubles.Arithmetic."/",
   SIMD.AVX.Doubles.Arithmetic."+",
   SIMD.AVX.Doubles.Arithmetic."-",
   SIMD.AVX.Doubles.Arithmetic."-",
   SIMD.AVX.Doubles.Arithmetic."abs",
   SIMD.AVX.Doubles.Arithmetic.Sum,
   SIMD.AVX.Doubles.Arithmetic.Divide_Or_Zero,
   SIMD.AVX.Doubles.Math.Sqrt,
   SIMD.AVX.Doubles.Math.Min,
   SIMD.AVX.Doubles.Math.Max,
   SIMD.AVX.Doubles.Math.Ceil,
   SIMD.AVX.Doubles.Math.Floor,
   SIMD.AVX.Doubles.Math.Round_Nearest_Integer,
   SIMD.AVX.Doubles.Math.Round_Truncate,
   SIMD.AVX.Doubles.Logical.And_Not,
   SIMD.AVX.Doubles.Logical."and",
   SIMD.AVX.Doubles.Logical."or",
   SIMD.AVX.Doubles.Logical."xor",
   SIMD.AVX.Doubles.Compare."=",
   SIMD.AVX.Doubles.Compare."/=",
   SIMD.AVX.Doubles.Compare.">",
   SIMD.AVX.Doubles.Compare."<",
   SIMD.AVX.Doubles.Compare.">=",
   SIMD.AVX.Doubles.Compare."<=",
   SIMD.AVX.Longs.Random.Emulation.State,
   SIMD.AVX.Longs.Random.Emulation.Next,
   SIMD.AVX.Longs.Random.Emulation.Reset);
pragma Preelaborate (Orka.Numerics.Doubles.Tensors.CPU);
