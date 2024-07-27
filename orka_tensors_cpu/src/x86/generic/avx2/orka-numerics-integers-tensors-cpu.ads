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

with Orka.SIMD.AVX2.Integers.Arithmetic;
with Orka.SIMD.AVX2.Integers.Compare;
with Orka.SIMD.AVX2.Integers.Logical;
with Orka.SIMD.AVX2.Integers.Math;
with Orka.SIMD.AVX2.Integers.Random;
with Orka.SIMD.AVX2.Integers.Shift;
with Orka.SIMD.AVX.Integers.Logical;

package Orka.Numerics.Integers.Tensors.CPU is new Orka.Numerics.Integers.Tensors.SIMD_CPU
  (SIMD.AVX.Index_8D,
   Integer_32,
   SIMD.AVX.Integers.m256i,
   SIMD.AVX2.Integers.Arithmetic."+",
   SIMD.AVX2.Integers.Logical."and",
   SIMD.AVX2.Integers.Shift.Shift_Elements_Left_Zeros,
   SIMD.AVX2.Integers.Shift.Shift_Elements_Right_Zeros,
   SIMD.AVX.Integers.Logical.Test_All_Ones,
   SIMD.AVX.Integers.Logical.Test_All_Zero,
   SIMD.AVX.Integers.m256i,
   SIMD.AVX2.Integers.Arithmetic."*",
   SIMD.AVX.Integers.Unsupported_Operation,  --  "/"
   SIMD.AVX2.Integers.Arithmetic."+",
   SIMD.AVX2.Integers.Arithmetic."-",
   SIMD.AVX2.Integers.Arithmetic."-",
   SIMD.AVX2.Integers.Arithmetic."abs",
   SIMD.AVX2.Integers.Arithmetic.Sum,
   SIMD.AVX.Integers.Unsupported_Operation,  --  Divide_Or_Zero
   SIMD.AVX.Integers.Unsupported_Operation,  --  Sqrt
   SIMD.AVX2.Integers.Math.Min,
   SIMD.AVX2.Integers.Math.Max,
   SIMD.AVX.Integers.Unsupported_Operation,  --  Exp
   SIMD.AVX.Integers.Unsupported_Operation,  --  Log
   SIMD.AVX.Integers.Unsupported_Operation,  --  Log10
   SIMD.AVX.Integers.Unsupported_Operation,  --  Log2
   SIMD.AVX.Integers.Unsupported_Operation,  --  Sin
   SIMD.AVX.Integers.Unsupported_Operation,  --  Cos
   SIMD.AVX.Integers.Unsupported_Operation,  --  Tan
   SIMD.AVX.Integers.Unsupported_Operation,  --  Arcsin
   SIMD.AVX.Integers.Unsupported_Operation,  --  Arccos
   SIMD.AVX.Integers.Unsupported_Operation,  --  Arctan
   SIMD.AVX.Integers.Unsupported_Operation,  --  Radians_To_Degrees
   SIMD.AVX.Integers.Unsupported_Operation,  --  Degrees_To_Radians
   SIMD.AVX.Integers.Unsupported_Operation,  --  "**"
   SIMD.AVX.Integers.Identity,
   SIMD.AVX.Integers.Identity,
   SIMD.AVX.Integers.Identity,
   SIMD.AVX.Integers.Identity,
   SIMD.AVX2.Integers.Logical.And_Not,
   SIMD.AVX2.Integers.Logical."and",
   SIMD.AVX2.Integers.Logical."or",
   SIMD.AVX2.Integers.Logical."xor",
   SIMD.AVX2.Integers.Compare."=",
   SIMD.AVX2.Integers.Compare."/=",
   SIMD.AVX2.Integers.Compare.">",
   SIMD.AVX2.Integers.Compare."<",
   SIMD.AVX2.Integers.Compare.">=",
   SIMD.AVX2.Integers.Compare."<=",
   SIMD.AVX2.Integers.Random.State,
   SIMD.AVX2.Integers.Random.Next,
   SIMD.AVX2.Integers.Random.Reset,
   Integer_32'Min,
   Integer_32'Max,
   SIMD.AVX.Integers.Is_Valid,
   SIMD.AVX.Integers.Unsupported_Operation);  --  Sqrt
pragma Preelaborate (Orka.Numerics.Integers.Tensors.CPU);
