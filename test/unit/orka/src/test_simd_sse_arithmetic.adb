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

with Ahven; use Ahven;

with GL.Types;

with Orka.SIMD.SSE.Singles.Arithmetic;

package body Test_SIMD_SSE_Arithmetic is

   use GL.Types;

   use Orka.SIMD;
   use Orka.SIMD.SSE.Singles;
   use Orka.SIMD.SSE.Singles.Arithmetic;

   overriding
   procedure Initialize (T : in out Test) is
   begin
      T.Set_Name ("Arithmetic");

      T.Add_Test_Routine (Test_Multiply'Access, "Test '*' operator");
      T.Add_Test_Routine (Test_Divide'Access, "Test '/' operator");
      T.Add_Test_Routine (Test_Divide_By_Zero'Access, "Test exception raised by '/' operator");
      T.Add_Test_Routine (Test_Divide_Or_Zero'Access, "Test Divide_Or_Zero function");
      T.Add_Test_Routine (Test_Add'Access, "Test '+' operator");
      T.Add_Test_Routine (Test_Subtract'Access, "Test '-' operator");
      T.Add_Test_Routine (Test_Minus'Access, "Test unary '-' operator");
      T.Add_Test_Routine (Test_Multiply_Vector'Access, "Test '*' operator on matrix and vector");
      T.Add_Test_Routine (Test_Multiply_Matrices'Access, "Test '*' operator on matrices");
      T.Add_Test_Routine (Test_Abs'Access, "Test 'abs' operator");
      T.Add_Test_Routine (Test_Sum'Access, "Test Sum function");
   end Initialize;

   procedure Test_Multiply is
      Left  : constant m128 := (1.0, 2.0, 3.0, 4.0);
      Right : constant m128 := (0.0, 1.0, 2.0, 3.0);

      Expected : constant m128 := (0.0, 2.0, 6.0, 12.0);
      Result   : constant m128 := Left * Right;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Multiply;

   procedure Test_Divide is
      Left  : constant m128 := (0.0, 2.0, 4.0, 4.5);
      Right : constant m128 := (1.0, 1.0, 2.0, 1.5);

      Expected : constant m128 := (0.0, 2.0, 2.0, 3.0);
      Result   : constant m128 := Left / Right;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Divide;

   procedure Test_Divide_By_Zero is
      Left  : constant m128 := (0.0, 1.0, 2.0, 3.0);
      Right : constant m128 := (0.0, 0.0, 0.0, 0.0);

      Result : constant m128 := Left / Right;
   begin
      declare
         Result_X : constant Single := Result (X);
         Result_Y : constant Single := Result (Y);
         Result_Z : constant Single := Result (Z);
         Result_W : constant Single := Result (W);
         pragma Unreferenced (Result_X);
         pragma Unreferenced (Result_Y);
         pragma Unreferenced (Result_Z);
         pragma Unreferenced (Result_W);
      begin
         Fail ("Expected Constraint_Error exception");
      end;
   exception
      when Constraint_Error =>
         null;
   end Test_Divide_By_Zero;

   procedure Test_Divide_Or_Zero is
      Left  : constant m128 := (0.0, 1.0, 2.0, 3.0);
      Right : constant m128 := (0.0, 0.0, 0.0, 0.0);

      Expected : constant m128 := (0.0, 0.0, 0.0, 0.0);
      Result   : constant m128 := Divide_Or_Zero (Left, Right);
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Divide_Or_Zero;

   procedure Test_Add is
      Left  : constant m128 := (0.0, 1.0, 1.5, 2.0);
      Right : constant m128 := (0.0, 0.0, 1.5, 2.0);

      Expected : constant m128 := (0.0, 1.0, 3.0, 4.0);
      Result   : constant m128 := Left + Right;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Add;

   procedure Test_Subtract is
      Left  : constant m128 := (0.0, 1.0, 1.5, 0.0);
      Right : constant m128 := (0.0, 0.0, 1.5, 2.0);

      Expected : constant m128 := (0.0, 1.0, 0.0, -2.0);
      Result   : constant m128 := Left - Right;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Subtract;

   procedure Test_Minus is
      Elements : constant m128 := (0.0, 1.0, 1.5, -2.0);

      Expected : constant m128 := (0.0, -1.0, -1.5, 2.0);
      Result   : constant m128 := -Elements;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Minus;

   procedure Test_Multiply_Vector is
      --  Matrix is an array of columns
      Left  : constant m128_Array := ((1.0, 5.0, 9.0, 13.0),
                                      (2.0, 6.0, 10.0, 14.0),
                                      (3.0, 7.0, 11.0, 15.0),
                                      (4.0, 8.0, 12.0, 16.0));

      Right : constant m128 := (2.0, 1.0, 1.0, 1.0);

      Expected : constant m128 := (11.0, 31.0, 51.0, 71.0);

      Result : constant m128 := Left * Right;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Multiply_Vector;

   procedure Test_Multiply_Matrices is
      --  Each matrix is an array of columns
      Left  : constant m128_Array := ((1.0, 5.0, 9.0, 13.0),
                                      (2.0, 6.0, 10.0, 14.0),
                                      (3.0, 7.0, 11.0, 15.0),
                                      (4.0, 8.0, 12.0, 16.0));

      Right : constant m128_Array := ((2.0, 1.0, 1.0, 1.0),
                                      (1.0, 2.0, 1.0, 1.0),
                                      (1.0, 1.0, 2.0, 1.0),
                                      (1.0, 1.0, 1.0, 2.0));

      Expected : constant m128_Array := ((11.0, 31.0, 51.0, 71.0),
                                         (12.0, 32.0, 52.0, 72.0),
                                         (13.0, 33.0, 53.0, 73.0),
                                         (14.0, 34.0, 54.0, 74.0));

      Result : constant m128_Array := Left * Right;
   begin
      for I in Index_Homogeneous loop
         for J in Index_Homogeneous loop
            Assert (Expected (I) (J) = Result (I) (J), "Unexpected Single at " & Index_Homogeneous'Image (I));
         end loop;
      end loop;
   end Test_Multiply_Matrices;

   procedure Test_Abs is
      Elements  : constant m128 := (1.0, -2.0, -3.0, 0.0);

      Expected : constant m128 := (1.0, 2.0, 3.0, 0.0);
      Result   : constant m128 := abs Elements;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Abs;

   procedure Test_Sum is
      Elements  : constant m128 := (-1.0, 2.0, -3.0, 4.0);

      Expected : constant GL.Types.Single := 2.0;
      Result   : constant GL.Types.Single := Sum (Elements);
   begin
      Assert (Expected = Result, "Unexpected Single " & GL.Types.Single'Image (Result));
   end Test_Sum;

end Test_SIMD_SSE_Arithmetic;
