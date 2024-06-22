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
with Orka.Numerics.Singles.Elementary_Functions;

with Orka.SIMD.AVX.Singles.Arithmetic;
with Orka.SIMD.AVX.Singles.Compare;
with Orka.SIMD.AVX.Singles.Logical;
with Orka.SIMD.AVX.Singles.Math;
with Orka.SIMD.AVX.Integers.Arithmetic.Emulation;
with Orka.SIMD.AVX.Integers.Logical.Emulation;
with Orka.SIMD.AVX.Integers.Random.Emulation;
with Orka.SIMD.AVX.Integers.Shift.Emulation;
with Orka.SIMD.AVX.Integers.Logical;

package Orka.Numerics.Singles.Tensors.CPU is new Orka.Numerics.Singles.Tensors.SIMD_CPU
  (SIMD.AVX.Index_8D,
   Integer_32,
   SIMD.AVX.Integers.m256i,
   SIMD.AVX.Integers.Arithmetic.Emulation."+",
   SIMD.AVX.Integers.Logical.Emulation."and",
   SIMD.AVX.Integers.Shift.Emulation.Shift_Elements_Left_Zeros,
   SIMD.AVX.Integers.Shift.Emulation.Shift_Elements_Right_Zeros,
   SIMD.AVX.Integers.Logical.Test_All_Ones,
   SIMD.AVX.Integers.Logical.Test_All_Zero,
   SIMD.AVX.Singles.m256,
   SIMD.AVX.Singles.Arithmetic."*",
   SIMD.AVX.Singles.Arithmetic."/",
   SIMD.AVX.Singles.Arithmetic."+",
   SIMD.AVX.Singles.Arithmetic."-",
   SIMD.AVX.Singles.Arithmetic."-",
   SIMD.AVX.Singles.Arithmetic."abs",
   SIMD.AVX.Singles.Arithmetic.Sum,
   SIMD.AVX.Singles.Arithmetic.Divide_Or_Zero,
   SIMD.AVX.Singles.Math.Sqrt,
   SIMD.AVX.Singles.Math.Min,
   SIMD.AVX.Singles.Math.Max,
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
   SIMD.AVX.Singles.Math.Ceil,
   SIMD.AVX.Singles.Math.Floor,
   SIMD.AVX.Singles.Math.Round_Nearest_Integer,
   SIMD.AVX.Singles.Math.Round_Truncate,
   SIMD.AVX.Singles.Logical.And_Not,
   SIMD.AVX.Singles.Logical."and",
   SIMD.AVX.Singles.Logical."or",
   SIMD.AVX.Singles.Logical."xor",
   SIMD.AVX.Singles.Compare."=",
   SIMD.AVX.Singles.Compare."/=",
   SIMD.AVX.Singles.Compare.">",
   SIMD.AVX.Singles.Compare."<",
   SIMD.AVX.Singles.Compare.">=",
   SIMD.AVX.Singles.Compare."<=",
   SIMD.AVX.Integers.Random.Emulation.State,
   SIMD.AVX.Integers.Random.Emulation.Next,
   SIMD.AVX.Integers.Random.Emulation.Reset,
   Float_32'Min,
   Float_32'Max,
   Numerics.Singles.Elementary_Functions.Is_Valid,
   Numerics.Singles.Elementary_Functions.Sqrt);
pragma Preelaborate (Orka.Numerics.Singles.Tensors.CPU);
