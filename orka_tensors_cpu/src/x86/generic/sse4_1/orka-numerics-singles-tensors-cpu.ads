--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2021 onox <denkpadje@gmail.com>
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
with Orka.Numerics.Singles.Elementary_Functions;

with Orka.SIMD.SSE.Singles.Arithmetic;
with Orka.SIMD.SSE.Singles.Compare;
with Orka.SIMD.SSE.Singles.Logical;
with Orka.SIMD.SSE.Singles.Math;
with Orka.SIMD.SSE2.Integers.Arithmetic;
with Orka.SIMD.SSE2.Integers.Logical;
with Orka.SIMD.SSE2.Integers.Random;
with Orka.SIMD.SSE2.Integers.Shift;
with Orka.SIMD.SSE3.Singles.Arithmetic;
with Orka.SIMD.SSE4_1.Integers.Logical;
with Orka.SIMD.SSE4_1.Singles.Math;

package Orka.Numerics.Singles.Tensors.CPU is new Orka.Numerics.Singles.Tensors.SIMD_CPU
  (Index_4D,
   Integer_32,
   SIMD.SSE2.Integers.m128i,
   SIMD.SSE2.Integers.Arithmetic."+",
   SIMD.SSE2.Integers.Logical."and",
   SIMD.SSE2.Integers.Shift.Shift_Elements_Left_Zeros,
   SIMD.SSE2.Integers.Shift.Shift_Elements_Right_Zeros,
   SIMD.SSE4_1.Integers.Logical.Test_All_Ones,
   SIMD.SSE4_1.Integers.Logical.Test_All_Zero,
   SIMD.SSE.Singles.m128,
   SIMD.SSE.Singles.Arithmetic."*",
   SIMD.SSE.Singles.Arithmetic."/",
   SIMD.SSE.Singles.Arithmetic."+",
   SIMD.SSE.Singles.Arithmetic."-",
   SIMD.SSE.Singles.Arithmetic."-",
   SIMD.SSE.Singles.Arithmetic."abs",
   SIMD.SSE3.Singles.Arithmetic.Sum,
   SIMD.SSE.Singles.Arithmetic.Divide_Or_Zero,
   SIMD.SSE.Singles.Math.Sqrt,
   SIMD.SSE.Singles.Math.Min,
   SIMD.SSE.Singles.Math.Max,
   Numerics.Singles.Elementary_Functions.Exp,
   Numerics.Singles.Elementary_Functions.Log,
   Numerics.Singles.Elementary_Functions.Log10,
   Numerics.Singles.Elementary_Functions.Log2,
   Numerics.Singles.Elementary_Functions.Sin,
   Numerics.Singles.Elementary_Functions.Cos,
   Numerics.Singles.Elementary_Functions.Tan,
   Numerics.Singles.Elementary_Functions.Arcsin,
   Numerics.Singles.Elementary_Functions.Arccos,
   Numerics.Singles.Elementary_Functions.Arctan,
   Numerics.Singles.Elementary_Functions.Radians_To_Degrees,
   Numerics.Singles.Elementary_Functions.Degrees_To_Radians,
   Numerics.Singles.Elementary_Functions."**",
   SIMD.SSE4_1.Singles.Math.Ceil,
   SIMD.SSE4_1.Singles.Math.Floor,
   SIMD.SSE4_1.Singles.Math.Round_Nearest_Integer,
   SIMD.SSE4_1.Singles.Math.Round_Truncate,
   SIMD.SSE.Singles.Logical.And_Not,
   SIMD.SSE.Singles.Logical."and",
   SIMD.SSE.Singles.Logical."or",
   SIMD.SSE.Singles.Logical."xor",
   SIMD.SSE.Singles.Compare."=",
   SIMD.SSE.Singles.Compare."/=",
   SIMD.SSE.Singles.Compare.">",
   SIMD.SSE.Singles.Compare."<",
   SIMD.SSE.Singles.Compare.">=",
   SIMD.SSE.Singles.Compare."<=",
   SIMD.SSE2.Integers.Random.State,
   SIMD.SSE2.Integers.Random.Next,
   SIMD.SSE2.Integers.Random.Reset,
   Float_32'Min,
   Float_32'Max,
   Numerics.Singles.Elementary_Functions.Is_Valid,
   Numerics.Singles.Elementary_Functions.Sqrt);
pragma Preelaborate (Orka.Numerics.Singles.Tensors.CPU);
