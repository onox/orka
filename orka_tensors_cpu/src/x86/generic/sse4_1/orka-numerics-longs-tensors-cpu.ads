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

with Orka.SIMD.SSE2.Longs.Arithmetic;
with Orka.SIMD.SSE2.Longs.Logical;
with Orka.SIMD.SSE2.Longs.Random;
with Orka.SIMD.SSE2.Longs.Shift;
with Orka.SIMD.SSE4_1.Longs.Arithmetic;
with Orka.SIMD.SSE4_1.Longs.Compare;
with Orka.SIMD.SSE4_1.Longs.Logical;
with Orka.SIMD.SSE4_2.Longs.Compare;
with Orka.SIMD.SSE4_2.Longs.Arithmetic.Emulation;
with Orka.SIMD.SSE4_2.Longs.Math.Emulation;

package Orka.Numerics.Longs.Tensors.CPU is new Orka.Numerics.Longs.Tensors.SIMD_CPU
  (Index_2D,
   Integer_64,
   SIMD.SSE2.Longs.m128l,
   SIMD.SSE2.Longs.Arithmetic."+",
   SIMD.SSE2.Longs.Logical."and",
   SIMD.SSE2.Longs.Shift.Shift_Elements_Left_Zeros,
   SIMD.SSE2.Longs.Shift.Shift_Elements_Right_Zeros,
   SIMD.SSE4_1.Longs.Logical.Test_All_Ones,
   SIMD.SSE4_1.Longs.Logical.Test_All_Zero,
   SIMD.SSE2.Longs.m128l,
   SIMD.SSE4_1.Longs.Arithmetic."*",
   SIMD.SSE2.Longs.Unsupported_Operation,  --  "/"
   SIMD.SSE2.Longs.Arithmetic."+",
   SIMD.SSE2.Longs.Arithmetic."-",
   SIMD.SSE2.Longs.Arithmetic."-",
   SIMD.SSE4_2.Longs.Arithmetic.Emulation."abs",
   SIMD.SSE2.Longs.Arithmetic.Sum,
   SIMD.SSE2.Longs.Unsupported_Operation,  --  Divide_Or_Zero
   SIMD.SSE2.Longs.Unsupported_Operation,  --  Sqrt
   SIMD.SSE4_2.Longs.Math.Emulation.Min,
   SIMD.SSE4_2.Longs.Math.Emulation.Max,
   SIMD.SSE2.Longs.Unsupported_Operation,  --  Exp
   SIMD.SSE2.Longs.Unsupported_Operation,  --  Log
   SIMD.SSE2.Longs.Unsupported_Operation,  --  Log10
   SIMD.SSE2.Longs.Unsupported_Operation,  --  Log2
   SIMD.SSE2.Longs.Unsupported_Operation,  --  Sin
   SIMD.SSE2.Longs.Unsupported_Operation,  --  Cos
   SIMD.SSE2.Longs.Unsupported_Operation,  --  Tan
   SIMD.SSE2.Longs.Unsupported_Operation,  --  Arcsin
   SIMD.SSE2.Longs.Unsupported_Operation,  --  Arccos
   SIMD.SSE2.Longs.Unsupported_Operation,  --  Arctan
   SIMD.SSE2.Longs.Unsupported_Operation,  --  Radians_To_Degrees
   SIMD.SSE2.Longs.Unsupported_Operation,  --  Degrees_To_Radians
   SIMD.SSE2.Longs.Unsupported_Operation,  --  "**"
   SIMD.SSE2.Longs.Identity,
   SIMD.SSE2.Longs.Identity,
   SIMD.SSE2.Longs.Identity,
   SIMD.SSE2.Longs.Identity,
   SIMD.SSE2.Longs.Logical.And_Not,
   SIMD.SSE2.Longs.Logical."and",
   SIMD.SSE2.Longs.Logical."or",
   SIMD.SSE2.Longs.Logical."xor",
   SIMD.SSE4_1.Longs.Compare."=",
   SIMD.SSE4_1.Longs.Compare."/=",
   SIMD.SSE4_2.Longs.Compare.">",
   SIMD.SSE4_2.Longs.Compare."<",
   SIMD.SSE4_2.Longs.Compare.">=",
   SIMD.SSE4_2.Longs.Compare."<=",
   SIMD.SSE2.Longs.Random.State,
   SIMD.SSE2.Longs.Random.Next,
   SIMD.SSE2.Longs.Random.Reset,
   Integer_64'Min,
   Integer_64'Max,
   SIMD.SSE2.Longs.Is_Valid,
   SIMD.SSE2.Longs.Unsupported_Operation);  --  Sqrt
pragma Preelaborate (Orka.Numerics.Longs.Tensors.CPU);
