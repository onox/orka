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
with Orka.Numerics.Doubles.Elementary_Functions;

with Orka.SIMD.SSE2.Doubles.Arithmetic;
with Orka.SIMD.SSE2.Doubles.Compare;
with Orka.SIMD.SSE2.Doubles.Logical;
with Orka.SIMD.SSE2.Doubles.Math;
with Orka.SIMD.SSE2.Longs.Arithmetic;
with Orka.SIMD.SSE2.Longs.Logical;
with Orka.SIMD.SSE2.Longs.Random;
with Orka.SIMD.SSE2.Longs.Shift;
with Orka.SIMD.SSE4_1.Longs.Logical;
with Orka.SIMD.SSE4_1.Doubles.Math;

package Orka.Numerics.Doubles.Tensors.CPU is new Orka.Numerics.Doubles.Tensors.SIMD_CPU
  (Index_2D,
   Integer_64,
   SIMD.SSE2.Longs.m128l,
   SIMD.SSE2.Longs.Arithmetic."+",
   SIMD.SSE2.Longs.Logical."and",
   SIMD.SSE2.Longs.Shift.Shift_Elements_Left_Zeros,
   SIMD.SSE2.Longs.Shift.Shift_Elements_Right_Zeros,
   SIMD.SSE4_1.Longs.Logical.Test_All_Ones,
   SIMD.SSE4_1.Longs.Logical.Test_All_Zero,
   SIMD.SSE2.Doubles.m128d,
   SIMD.SSE2.Doubles.Arithmetic."*",
   SIMD.SSE2.Doubles.Arithmetic."/",
   SIMD.SSE2.Doubles.Arithmetic."+",
   SIMD.SSE2.Doubles.Arithmetic."-",
   SIMD.SSE2.Doubles.Arithmetic."-",
   SIMD.SSE2.Doubles.Arithmetic."abs",
   SIMD.SSE2.Doubles.Arithmetic.Sum,
   SIMD.SSE2.Doubles.Arithmetic.Divide_Or_Zero,
   SIMD.SSE2.Doubles.Math.Sqrt,
   SIMD.SSE2.Doubles.Math.Min,
   SIMD.SSE2.Doubles.Math.Max,
   Numerics.Doubles.Elementary_Functions.Exp,
   Numerics.Doubles.Elementary_Functions.Log,
   Numerics.Doubles.Elementary_Functions.Log10,
   Numerics.Doubles.Elementary_Functions.Log2,
   Numerics.Doubles.Elementary_Functions.Sin,
   Numerics.Doubles.Elementary_Functions.Cos,
   Numerics.Doubles.Elementary_Functions.Tan,
   Numerics.Doubles.Elementary_Functions.Arcsin,
   Numerics.Doubles.Elementary_Functions.Arccos,
   Numerics.Doubles.Elementary_Functions.Arctan,
   Numerics.Doubles.Elementary_Functions.Radians_To_Degrees,
   Numerics.Doubles.Elementary_Functions.Degrees_To_Radians,
   Numerics.Doubles.Elementary_Functions."**",
   SIMD.SSE4_1.Doubles.Math.Ceil,
   SIMD.SSE4_1.Doubles.Math.Floor,
   SIMD.SSE4_1.Doubles.Math.Round_Nearest_Integer,
   SIMD.SSE4_1.Doubles.Math.Round_Truncate,
   SIMD.SSE2.Doubles.Logical.And_Not,
   SIMD.SSE2.Doubles.Logical."and",
   SIMD.SSE2.Doubles.Logical."or",
   SIMD.SSE2.Doubles.Logical."xor",
   SIMD.SSE2.Doubles.Compare."=",
   SIMD.SSE2.Doubles.Compare."/=",
   SIMD.SSE2.Doubles.Compare.">",
   SIMD.SSE2.Doubles.Compare."<",
   SIMD.SSE2.Doubles.Compare.">=",
   SIMD.SSE2.Doubles.Compare."<=",
   SIMD.SSE2.Longs.Random.State,
   SIMD.SSE2.Longs.Random.Next,
   SIMD.SSE2.Longs.Random.Reset,
   Float_64'Min,
   Float_64'Max,
   Numerics.Doubles.Elementary_Functions.Is_Valid,
   Numerics.Doubles.Elementary_Functions.Sqrt);
pragma Preelaborate (Orka.Numerics.Doubles.Tensors.CPU);
