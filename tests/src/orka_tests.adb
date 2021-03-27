with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites;

with Test_SIMD_SSE_Arithmetic;
with Test_SIMD_SSE_Compare;
with Test_SIMD_SSE_Logical;
with Test_SIMD_SSE_Math;
with Test_SIMD_SSE_Swizzle;
with Test_SIMD_SSE4_1_Math;
with Test_SIMD_AVX_Arithmetic;
with Test_SIMD_AVX_Compare;
with Test_SIMD_AVX_Math;
with Test_SIMD_AVX_Swizzle;
with Test_SIMD_AVX2_Swizzle;
with Test_SIMD_FMA_Singles_Arithmetic;
with Test_SIMD_FMA_Doubles_Arithmetic;

with Test_Transforms_Singles_Matrices;
with Test_Transforms_Singles_Quaternions;
with Test_Transforms_Singles_Vectors;
with Test_Transforms_Doubles_Matrices;
with Test_Transforms_Doubles_Vectors;

with Test_Scene_Trees;

procedure Orka_Tests is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite :=
        AUnit.Test_Suites.New_Suite;
   begin
      --  SIMD > SSE (Single)
      Result.Add_Test (Test_SIMD_SSE_Arithmetic.Suite);
      Result.Add_Test (Test_SIMD_SSE_Compare.Suite);
      Result.Add_Test (Test_SIMD_SSE_Logical.Suite);
      Result.Add_Test (Test_SIMD_SSE_Math.Suite);
      Result.Add_Test (Test_SIMD_SSE_Swizzle.Suite);

      --  SIMD > SSE4.1 (Singles)
      Result.Add_Test (Test_SIMD_SSE4_1_Math.Suite);

      --  SIMD > AVX (Doubles)
      Result.Add_Test (Test_SIMD_AVX_Arithmetic.Suite);
      Result.Add_Test (Test_SIMD_AVX_Compare.Suite);
      Result.Add_Test (Test_SIMD_AVX_Math.Suite);
      Result.Add_Test (Test_SIMD_AVX_Swizzle.Suite);

      --  SIMD > AVX2 (Doubles)
      Result.Add_Test (Test_SIMD_AVX2_Swizzle.Suite);

      --  SIMD > FMA (Singles)
      Result.Add_Test (Test_SIMD_FMA_Singles_Arithmetic.Suite);

      --  SIMD > FMA (Doubles)
      Result.Add_Test (Test_SIMD_FMA_Doubles_Arithmetic.Suite);

      --  Transforms (Singles)
      Result.Add_Test (Test_Transforms_Singles_Matrices.Suite);
      Result.Add_Test (Test_Transforms_Singles_Vectors.Suite);
      Result.Add_Test (Test_Transforms_Singles_Quaternions.Suite);

      --  Transforms (Doubles)
      Result.Add_Test (Test_Transforms_Doubles_Matrices.Suite);
      Result.Add_Test (Test_Transforms_Doubles_Vectors.Suite);

      --  Trees
      Result.Add_Test (Test_Scene_Trees.Suite);

      return Result;
   end Suite;

   procedure Runner is new AUnit.Run.Test_Runner (Suite);

   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Reporter.Set_Use_ANSI_Colors (True);
   Runner (Reporter);
end Orka_Tests;
