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

with Orka.SIMD.AVX.Doubles.Arithmetic;

package body Test_SIMD_AVX_Arithmetic is

   use GL.Types;

   use Orka.SIMD;
   use Orka.SIMD.AVX.Doubles;
   use Orka.SIMD.AVX.Doubles.Arithmetic;

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
      T.Add_Test_Routine (Test_Multiply_Matrices'Access, "Test '*' operator on matrices");
   end Initialize;

   procedure Test_Multiply is
      Left  : constant m256d := (1.0, 2.0, 3.0, 4.0);
      Right : constant m256d := (0.0, 1.0, 2.0, 3.0);

      Expected : constant m256d := (0.0, 2.0, 6.0, 12.0);
      Result   : constant m256d := Left * Right;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Multiply;

   procedure Test_Divide is
      Left  : constant m256d := (0.0, 2.0, 4.0, 4.5);
      Right : constant m256d := (1.0, 1.0, 2.0, 1.5);

      Expected : constant m256d := (0.0, 2.0, 2.0, 3.0);
      Result   : constant m256d := Left / Right;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Divide;

   procedure Test_Divide_By_Zero is
      Left  : constant m256d := (0.0, 1.0, 2.0, 3.0);
      Right : constant m256d := (0.0, 0.0, 0.0, 0.0);

      Result : constant m256d := Left / Right;
   begin
      declare
         Result_X : constant Double := Result (X);
         Result_Y : constant Double := Result (Y);
         Result_Z : constant Double := Result (Z);
         Result_W : constant Double := Result (W);
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
      Left  : constant m256d := (0.0, 1.0, 2.0, 3.0);
      Right : constant m256d := (0.0, 0.0, 0.0, 0.0);

      Expected : constant m256d := (0.0, 0.0, 0.0, 0.0);
      Result   : constant m256d := Divide_Or_Zero (Left, Right);
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Divide_Or_Zero;

   procedure Test_Add is
      Left  : constant m256d := (0.0, 1.0, 1.5, 2.0);
      Right : constant m256d := (0.0, 0.0, 1.5, 2.0);

      Expected : constant m256d := (0.0, 1.0, 3.0, 4.0);
      Result   : constant m256d := Left + Right;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Add;

   procedure Test_Subtract is
      Left  : constant m256d := (0.0, 1.0, 1.5, 0.0);
      Right : constant m256d := (0.0, 0.0, 1.5, 2.0);

      Expected : constant m256d := (0.0, 1.0, 0.0, -2.0);
      Result   : constant m256d := Left - Right;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Subtract;

   procedure Test_Minus is
      Elements : constant m256d := (0.0, 1.0, 1.5, -2.0);

      Expected : constant m256d := (0.0, -1.0, -1.5, 2.0);
      Result   : constant m256d := -Elements;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Minus;

   procedure Test_Multiply_Matrices is
      --  Each matrix is an array of columns
      Left  : constant m256d_Array := ((1.0, 5.0, 9.0, 13.0),
                                      (2.0, 6.0, 10.0, 14.0),
                                      (3.0, 7.0, 11.0, 15.0),
                                      (4.0, 8.0, 12.0, 16.0));

      Right : constant m256d_Array := ((2.0, 1.0, 1.0, 1.0),
                                      (1.0, 2.0, 1.0, 1.0),
                                      (1.0, 1.0, 2.0, 1.0),
                                      (1.0, 1.0, 1.0, 2.0));

      Expected : constant m256d_Array := ((11.0, 31.0, 51.0, 71.0),
                                         (12.0, 32.0, 52.0, 72.0),
                                         (13.0, 33.0, 53.0, 73.0),
                                         (14.0, 34.0, 54.0, 74.0));

      Result : constant m256d_Array := Left * Right;
   begin
      for I in Index_Homogeneous loop
         for J in Index_Homogeneous loop
            Assert (Expected (I) (J) = Result (I) (J), "Unexpected Double at " & Index_Homogeneous'Image (I));
         end loop;
      end loop;
   end Test_Multiply_Matrices;

end Test_SIMD_AVX_Arithmetic;
