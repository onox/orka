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

with Orka.SIMD.SSE2.Integers.Arithmetic;
with Orka.SIMD.SSE2.Integers.Compare;
with Orka.SIMD.SSE2.Integers.Logical;
with Orka.SIMD.SSE2.Integers.Random;
with Orka.SIMD.SSE2.Integers.Shift;
with Orka.SIMD.SSE4_1.Integers.Arithmetic;
with Orka.SIMD.SSE4_1.Integers.Logical;
with Orka.SIMD.SSE4_1.Integers.Math;
with Orka.SIMD.SSSE3.Integers.Arithmetic;

package Orka.Numerics.Integers.Tensors.CPU is new Orka.Numerics.Integers.Tensors.SIMD_CPU
  (Index_4D,
   Integer_32,
   SIMD.SSE2.Integers.m128i,
   SIMD.SSE2.Integers.Arithmetic."+",
   SIMD.SSE2.Integers.Logical."and",
   SIMD.SSE2.Integers.Shift.Shift_Elements_Left_Zeros,
   SIMD.SSE2.Integers.Shift.Shift_Elements_Right_Zeros,
   SIMD.SSE4_1.Integers.Logical.Test_All_Ones,
   SIMD.SSE4_1.Integers.Logical.Test_All_Zero,
   SIMD.SSE2.Integers.m128i,
   SIMD.SSE4_1.Integers.Arithmetic."*",
   SIMD.SSE2.Integers.Unsupported_Operation,  --  "/"
   SIMD.SSE2.Integers.Arithmetic."+",
   SIMD.SSE2.Integers.Arithmetic."-",
   SIMD.SSE2.Integers.Arithmetic."-",
   SIMD.SSSE3.Integers.Arithmetic."abs",
   SIMD.SSE2.Integers.Arithmetic.Sum,
   SIMD.SSE2.Integers.Unsupported_Operation,  --  Divide_Or_Zero
   SIMD.SSE2.Integers.Unsupported_Operation,  --  Sqrt
   SIMD.SSE4_1.Integers.Math.Min,
   SIMD.SSE4_1.Integers.Math.Max,
   SIMD.SSE2.Integers.Unsupported_Operation,  --  Exp
   SIMD.SSE2.Integers.Unsupported_Operation,  --  Log
   SIMD.SSE2.Integers.Unsupported_Operation,  --  Log10
   SIMD.SSE2.Integers.Unsupported_Operation,  --  Log2
   SIMD.SSE2.Integers.Unsupported_Operation,  --  Sin
   SIMD.SSE2.Integers.Unsupported_Operation,  --  Cos
   SIMD.SSE2.Integers.Unsupported_Operation,  --  Tan
   SIMD.SSE2.Integers.Unsupported_Operation,  --  Arcsin
   SIMD.SSE2.Integers.Unsupported_Operation,  --  Arccos
   SIMD.SSE2.Integers.Unsupported_Operation,  --  Arctan
   SIMD.SSE2.Integers.Unsupported_Operation,  --  Radians_To_Degrees
   SIMD.SSE2.Integers.Unsupported_Operation,  --  Degrees_To_Radians
   SIMD.SSE2.Integers.Unsupported_Operation,  --  "**"
   SIMD.SSE2.Integers.Identity,
   SIMD.SSE2.Integers.Identity,
   SIMD.SSE2.Integers.Identity,
   SIMD.SSE2.Integers.Identity,
   SIMD.SSE2.Integers.Logical.And_Not,
   SIMD.SSE2.Integers.Logical."and",
   SIMD.SSE2.Integers.Logical."or",
   SIMD.SSE2.Integers.Logical."xor",
   SIMD.SSE2.Integers.Compare."=",
   SIMD.SSE2.Integers.Compare."/=",
   SIMD.SSE2.Integers.Compare.">",
   SIMD.SSE2.Integers.Compare."<",
   SIMD.SSE2.Integers.Compare.">=",
   SIMD.SSE2.Integers.Compare."<=",
   SIMD.SSE2.Integers.Random.State,
   SIMD.SSE2.Integers.Random.Next,
   SIMD.SSE2.Integers.Random.Reset,
   Integer_32'Min,
   Integer_32'Max,
   SIMD.SSE2.Integers.Is_Valid,
   SIMD.SSE2.Integers.Unsupported_Operation);  --  Sqrt
pragma Preelaborate (Orka.Numerics.Integers.Tensors.CPU);
