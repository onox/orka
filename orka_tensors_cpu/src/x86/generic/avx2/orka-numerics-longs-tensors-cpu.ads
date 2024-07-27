--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2024 onox <denkpadje@gmail.com>
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

with Orka.SIMD.AVX2.Longs.Arithmetic.Emulation;
with Orka.SIMD.AVX2.Longs.Compare;
with Orka.SIMD.AVX2.Longs.Logical;
with Orka.SIMD.AVX2.Longs.Math.Emulation;
with Orka.SIMD.AVX2.Longs.Random;
with Orka.SIMD.AVX2.Longs.Shift;
with Orka.SIMD.AVX.Longs.Logical;

package Orka.Numerics.Longs.Tensors.CPU is new Orka.Numerics.Longs.Tensors.SIMD_CPU
  (Index_4D,
   Integer_64,
   SIMD.AVX.Longs.m256l,
   SIMD.AVX2.Longs.Arithmetic."+",
   SIMD.AVX2.Longs.Logical."and",
   SIMD.AVX2.Longs.Shift.Shift_Elements_Left_Zeros,
   SIMD.AVX2.Longs.Shift.Shift_Elements_Right_Zeros,
   SIMD.AVX.Longs.Logical.Test_All_Ones,
   SIMD.AVX.Longs.Logical.Test_All_Zero,
   SIMD.AVX.Longs.m256l,
   SIMD.AVX2.Longs.Arithmetic."*",
   SIMD.AVX.Longs.Unsupported_Operation,  --  "/"
   SIMD.AVX2.Longs.Arithmetic."+",
   SIMD.AVX2.Longs.Arithmetic."-",
   SIMD.AVX2.Longs.Arithmetic."-",
   SIMD.AVX2.Longs.Arithmetic.Emulation."abs",
   SIMD.AVX2.Longs.Arithmetic.Sum,
   SIMD.AVX.Longs.Unsupported_Operation,  --  Divide_Or_Zero
   SIMD.AVX.Longs.Unsupported_Operation,  --  Sqrt
   SIMD.AVX2.Longs.Math.Emulation.Min,
   SIMD.AVX2.Longs.Math.Emulation.Max,
   SIMD.AVX.Longs.Unsupported_Operation,  --  Exp
   SIMD.AVX.Longs.Unsupported_Operation,  --  Log
   SIMD.AVX.Longs.Unsupported_Operation,  --  Log10
   SIMD.AVX.Longs.Unsupported_Operation,  --  Log2
   SIMD.AVX.Longs.Unsupported_Operation,  --  Sin
   SIMD.AVX.Longs.Unsupported_Operation,  --  Cos
   SIMD.AVX.Longs.Unsupported_Operation,  --  Tan
   SIMD.AVX.Longs.Unsupported_Operation,  --  Arcsin
   SIMD.AVX.Longs.Unsupported_Operation,  --  Arccos
   SIMD.AVX.Longs.Unsupported_Operation,  --  Arctan
   SIMD.AVX.Longs.Unsupported_Operation,  --  Radians_To_Degrees
   SIMD.AVX.Longs.Unsupported_Operation,  --  Degrees_To_Radians
   SIMD.AVX.Longs.Unsupported_Operation,  --  "**"
   SIMD.AVX.Longs.Identity,
   SIMD.AVX.Longs.Identity,
   SIMD.AVX.Longs.Identity,
   SIMD.AVX.Longs.Identity,
   SIMD.AVX2.Longs.Logical.And_Not,
   SIMD.AVX2.Longs.Logical."and",
   SIMD.AVX2.Longs.Logical."or",
   SIMD.AVX2.Longs.Logical."xor",
   SIMD.AVX2.Longs.Compare."=",
   SIMD.AVX2.Longs.Compare."/=",
   SIMD.AVX2.Longs.Compare.">",
   SIMD.AVX2.Longs.Compare."<",
   SIMD.AVX2.Longs.Compare.">=",
   SIMD.AVX2.Longs.Compare."<=",
   SIMD.AVX2.Longs.Random.State,
   SIMD.AVX2.Longs.Random.Next,
   SIMD.AVX2.Longs.Random.Reset,
   Integer_64'Min,
   Integer_64'Max,
   SIMD.AVX.Longs.Is_Valid,
   SIMD.AVX.Longs.Unsupported_Operation);  --  Sqrt
pragma Preelaborate (Orka.Numerics.Longs.Tensors.CPU);
