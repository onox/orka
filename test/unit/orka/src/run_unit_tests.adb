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

with Test_SIMD_SSE_Arithmetic;
with Test_SIMD_SSE_Compare;
with Test_SIMD_SSE_Logical;
with Test_SIMD_SSE_Math;
with Test_SIMD_SSE_Swizzle;
with Test_SIMD_SSE4_1_Math;

procedure Run_Unit_Tests is
   Suite_SIMD : Test_Suite := Create_Suite ("SIMD");

   Suite_SSE    : constant Test_Suite_Access := Create_Suite ("SSE");
   Suite_SSE4_1 : constant Test_Suite_Access := Create_Suite ("SSE4.1");
begin
   Suite_SSE.Add_Test (new Test_SIMD_SSE_Arithmetic.Test);
   Suite_SSE.Add_Test (new Test_SIMD_SSE_Compare.Test);
   Suite_SSE.Add_Test (new Test_SIMD_SSE_Logical.Test);
   Suite_SSE.Add_Test (new Test_SIMD_SSE_Math.Test);
   Suite_SSE.Add_Test (new Test_SIMD_SSE_Swizzle.Test);
   Suite_SIMD.Add_Test (Suite_SSE);

   Suite_SSE4_1.Add_Test (new Test_SIMD_SSE4_1_Math.Test);
   Suite_SIMD.Add_Test (Suite_SSE4_1);

   Ahven.Text_Runner.Run (Suite_SIMD);
end Run_Unit_Tests;
