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

with Ahven.Framework; use Ahven.Framework;
with Ahven.Text_Runner;

with Test_SIMD_AVX_Arithmetic;
with Test_SIMD_AVX_Compare;
with Test_SIMD_AVX_Math;
with Test_SIMD_AVX_Swizzle;
with Test_SIMD_SSE_Arithmetic;
with Test_SIMD_SSE_Compare;
with Test_SIMD_SSE_Logical;
with Test_SIMD_SSE_Math;
with Test_SIMD_SSE_Swizzle;
with Test_SIMD_SSE4_1_Math;

with Test_Transforms_Singles_Matrices;
with Test_Transforms_Doubles_Matrices;

with Test_Transforms_Singles_Vectors;
with Test_Transforms_Doubles_Vectors;

with Test_Transforms_Singles_Quaternions;

with Test_Scene_Trees;

procedure Run_Unit_Tests is
   Suite_All : Test_Suite := Create_Suite ("all");

   Suite_SIMD       : constant Test_Suite_Access := Create_Suite ("SIMD");
   Suite_Transforms : constant Test_Suite_Access := Create_Suite ("Transforms");

   Suite_SSE    : constant Test_Suite_Access := Create_Suite ("SSE (Singles)");
   Suite_SSE4_1 : constant Test_Suite_Access := Create_Suite ("SSE4.1 (Singles)");
   Suite_AVX    : constant Test_Suite_Access := Create_Suite ("AVX (Doubles)");

   Suite_Transforms_Singles : constant Test_Suite_Access := Create_Suite ("Singles");
   Suite_Transforms_Doubles : constant Test_Suite_Access := Create_Suite ("Doubles");
begin
   --  SIMD
   Suite_SSE.Add_Test (new Test_SIMD_SSE_Arithmetic.Test);
   Suite_SSE.Add_Test (new Test_SIMD_SSE_Compare.Test);
   Suite_SSE.Add_Test (new Test_SIMD_SSE_Logical.Test);
   Suite_SSE.Add_Test (new Test_SIMD_SSE_Math.Test);
   Suite_SSE.Add_Test (new Test_SIMD_SSE_Swizzle.Test);
   Suite_SIMD.Add_Test (Suite_SSE);

   Suite_SSE4_1.Add_Test (new Test_SIMD_SSE4_1_Math.Test);
   Suite_SIMD.Add_Test (Suite_SSE4_1);

   Suite_AVX.Add_Test (new Test_SIMD_AVX_Arithmetic.Test);
   Suite_AVX.Add_Test (new Test_SIMD_AVX_Compare.Test);
   Suite_AVX.Add_Test (new Test_SIMD_AVX_Math.Test);
   Suite_AVX.Add_Test (new Test_SIMD_AVX_Swizzle.Test);
   Suite_SIMD.Add_Test (Suite_AVX);

   Suite_All.Add_Test (Suite_SIMD);

   --  Transforms
   Suite_Transforms_Singles.Add_Test (new Test_Transforms_Singles_Matrices.Test);
   Suite_Transforms_Singles.Add_Test (new Test_Transforms_Singles_Vectors.Test);
   Suite_Transforms_Singles.Add_Test (new Test_Transforms_Singles_Quaternions.Test);
   Suite_Transforms.Add_Test (Suite_Transforms_Singles);

   Suite_Transforms_Doubles.Add_Test (new Test_Transforms_Doubles_Matrices.Test);
   Suite_Transforms_Doubles.Add_Test (new Test_Transforms_Doubles_Vectors.Test);
   Suite_Transforms.Add_Test (Suite_Transforms_Doubles);

   Suite_All.Add_Test (Suite_Transforms);

   --  Scene tree
   Suite_All.Add_Test (new Test_Scene_Trees.Test);

   Ahven.Text_Runner.Run (Suite_All);
end Run_Unit_Tests;
