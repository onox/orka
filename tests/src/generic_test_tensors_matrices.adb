--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2021 onox <denkpadje@gmail.com>
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

with AUnit.Assertions;
with AUnit.Test_Caller;
with AUnit.Test_Fixtures;

package body Generic_Test_Tensors_Matrices is

   use Tensors;
   use SIMD_CPU;
   use type Tensors.Element;

   use AUnit.Assertions;

   Abs_Tolerance : constant Element := 100.0 * Element'Model_Epsilon;

   procedure Assert (Expected, Result : Element; Message : String) is
      function Is_Similar (Expected, Result : Element) return Boolean is
        (abs (Result - Expected) <= Abs_Tolerance + 1.0e-05 * abs Expected);
   begin
      Assert (Is_Similar (Expected, Result), Message);
   end Assert;

   procedure Assert_Equal (Expected : Element_Array; Actual : CPU_Tensor) is
   begin
      Assert (Actual.Elements = Expected'Length,
        "Unexpected size of tensor: " & Actual.Elements'Image);

      for I in Expected'Range loop
         Assert (Expected (I) = Actual (I), "Unexpected element at index " & I'Image & ": " &
           Element'Image (Actual (I)) & " instead of " & Element'Image (Expected (I)));
      end loop;
   end Assert_Equal;

   ----------------------------------------------------------------------------

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Flatten (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := Identity (3);
      Tensor_2 : constant CPU_Tensor := Tensor_1.Flatten;
   begin
      Assert (Tensor_1.Dimensions = 2, "Unexpected dimensions:" & Tensor_1.Dimensions'Image);
      Assert (Tensor_2.Dimensions = 1, "Unexpected dimensions:" & Tensor_2.Dimensions'Image);

      Assert (Tensor_1.Shape = (3, 3), "Unexpected shape:" & Image (Tensor_1.Shape));
      Assert (Tensor_2.Shape = (1 => 9), "Unexpected shape:" & Image (Tensor_2.Shape));

      Assert (Tensor_1.Elements = 9, "Unexpected number of elements:" & Tensor_1.Elements'Image);
      Assert (Tensor_2.Elements = 9, "Unexpected number of elements:" & Tensor_2.Elements'Image);
   end Test_Flatten;

   procedure Test_Identity_Square (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := Identity (3);

      Tensor_2 : constant CPU_Tensor := Identity (3, Offset => 1);
      --  0 1 0
      --  0 0 1
      --  0 0 0

      Tensor_3 : constant CPU_Tensor := Identity (3, Offset => -1);
      --  0 0 0
      --  1 0 0
      --  0 1 0

      Values_1 : constant Element_Array := (1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);
      Values_2 : constant Element_Array := (0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0);
      Values_3 : constant Element_Array := (0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0);
   begin
      Assert_Equal (Values_1, Tensor_1.Flatten);
      Assert_Equal (Values_2, Tensor_2.Flatten);
      Assert_Equal (Values_3, Tensor_3.Flatten);
   end Test_Identity_Square;

   procedure Test_Identity_Not_Square (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := Identity (3);
      Tensor_2 : constant CPU_Tensor := Identity (Rows => 3, Columns => 3);
      --  1 0 0
      --  0 1 0
      --  0 0 1

      Tensor_3 : constant CPU_Tensor := Identity (Rows => 3, Columns => 2);
      --  1 0
      --  0 1
      --  0 0

      Tensor_4 : constant CPU_Tensor := Identity (Rows => 2, Columns => 3, Offset => 1);
      --  0 1 0
      --  0 0 1

      Values_3 : constant Element_Array := (1.0, 0.0, 0.0, 1.0, 0.0, 0.0);
      Values_4 : constant Element_Array := (0.0, 1.0, 0.0, 0.0, 0.0, 1.0);
   begin
      Assert (Tensor_1 = Tensor_2, "Identity matrices not equal");

      Assert_Equal (Values_3, Tensor_3.Flatten);
      Assert_Equal (Values_4, Tensor_4.Flatten);
   end Test_Identity_Not_Square;

   procedure Test_Reshape (Object : in out Test) is
      Values : constant Element_Array := (1.0, 2.0, 3.0, 4.0, 5.0, 6.0);

      Shape_1D : constant Tensor_Shape := (1 => Values'Length);
      Shape_2D : constant Tensor_Shape := (2, 3);

      Tensor_1 : constant CPU_Tensor := To_Tensor (Values);
      Tensor_2 : constant CPU_Tensor := Tensor_1.Reshape (Shape_2D);

      Tensor_3 : constant CPU_Tensor := Tensor_2.Reshape (Tensor_1.Elements);
      Tensor_4 : constant CPU_Tensor := Tensor_1.Reshape (Tensor_1.Elements);
   begin
      Assert (Tensor_1.Shape = Shape_1D, "Unexpected shape: " & Image (Tensor_1.Shape));
      Assert (Tensor_2.Shape = Shape_2D, "Unexpected shape: " & Image (Tensor_2.Shape));
      Assert (Tensor_3.Shape = Shape_1D, "Unexpected shape: " & Image (Tensor_3.Shape));
      Assert (Tensor_4.Shape = Shape_1D, "Unexpected shape: " & Image (Tensor_4.Shape));
   end Test_Reshape;

   procedure Test_Concatenate (Object : in out Test) is
      Left_1 : constant CPU_Tensor := Diagonal ((1.0, 2.0, 3.0));
      --  1 0 0
      --  0 2 0
      --  0 0 3

      Right_1 : constant CPU_Tensor := To_Tensor ((4.0, 5.0, 6.0, 7.0, 8.0, 9.0)).Reshape ((2, 3));
      --  4 5 6
      --  7 8 9

      Right_2 : constant CPU_Tensor := To_Tensor ((4.0, 5.0, 6.0, 7.0, 8.0, 9.0)).Reshape ((3, 2));
      --  4 5
      --  6 7
      --  8 9

      Expected_1 : constant Element_Array :=
        (1.0, 0.0, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0);
      Expected_2 : constant Element_Array :=
        (1.0, 0.0, 0.0, 4.0, 5.0, 0.0, 2.0, 0.0, 6.0, 7.0, 0.0, 0.0, 3.0, 8.0, 9.0);

      Actual_1 : constant CPU_Tensor := Left_1.Concatenate (Right_1, Dimension => 1);
      --  1 0 0
      --  0 2 0
      --  0 0 3
      --  4 5 6
      --  7 8 9

      Actual_2 : constant CPU_Tensor := Left_1.Concatenate (Right_2, Dimension => 2);
      --  1 0 0 4 5
      --  0 2 0 6 7
      --  0 0 3 8 9
   begin
      Assert (Actual_1 = (Left_1 & Right_1), "Tensors not equal");
      Assert_Equal (Expected_1, Actual_1.Flatten);
      Assert_Equal (Expected_2, Actual_2.Flatten);
   end Test_Concatenate;

   procedure Test_Main_Diagonal (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := Identity (3);

      Tensor_2 : constant CPU_Tensor := Identity (3, Offset => 1);
      --  0 1 0
      --  0 0 1
      --  0 0 0

      Expected_1 : constant Element_Array := (1.0, 1.0, 1.0);
      Expected_2 : constant Element_Array := (0.0, 0.0, 0.0);
      Expected_3 : constant Element_Array := (1.0, 1.0, 0.0);

      Actual_1 : constant CPU_Tensor := Tensor_1.Main_Diagonal;
      Actual_2 : constant CPU_Tensor := Tensor_2.Main_Diagonal;
      Actual_3 : constant CPU_Tensor := Tensor_2.Main_Diagonal (Offset => 1);
   begin
      Assert_Equal (Expected_1, Actual_1);
      Assert_Equal (Expected_2, Actual_2);
      Assert_Equal (Expected_3, Actual_3);
   end Test_Main_Diagonal;

   procedure Test_Diagonal (Object : in out Test) is
      Expected_1 : constant Element_Array := (1.0, 2.0, 3.0);
      Expected_2 : constant Element_Array := (0.0, 0.0, 0.0);
      Expected_3 : constant Element_Array := (1.0, 2.0, 0.0);

      Tensor_1 : constant CPU_Tensor := Diagonal (Expected_1);
      --  1 0 0
      --  0 2 0
      --  0 0 3

      Tensor_2 : constant CPU_Tensor := Diagonal (Expected_1, Offset => 1);
      --  0 1 0
      --  0 0 2
      --  0 0 0

      Tensor_3 : constant CPU_Tensor := Diagonal (To_Tensor (Expected_1));
   begin
      Assert_Equal (Expected_1, Tensor_1.Main_Diagonal);
      Assert_Equal (Expected_2, Tensor_2.Main_Diagonal);
      Assert_Equal (Expected_3, Tensor_2.Main_Diagonal (Offset => 1));

      Assert_Equal (Expected_1, Tensor_3.Main_Diagonal);
   end Test_Diagonal;

   procedure Test_Trace (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := Diagonal ((1.0, 2.0, 3.0));
      Tensor_2 : constant CPU_Tensor := Diagonal ((1.0, 2.0, 3.0, 4.0, 5.0));
      Tensor_3 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0, 6.0));

      Expected_1 : constant Element := 6.0;
      Expected_2 : constant Element := 15.0;
      Expected_3 : constant Element := 6.0;
      Expected_4 : constant Element := 5.0;

      Actual_1 : constant Element := Tensor_1.Trace;
      Actual_2 : constant Element := Tensor_2.Trace;
      Actual_3 : constant Element := Tensor_3.Reshape ((2, 3)).Trace;
      Actual_4 : constant Element := Tensor_3.Reshape ((3, 2)).Trace;
   begin
      Assert (Expected_1 = Actual_1, "Unexpected trace: " & Actual_1'Image);
      Assert (Expected_2 = Actual_2, "Unexpected trace: " & Actual_2'Image);
      Assert (Expected_3 = Actual_3, "Unexpected trace: " & Actual_3'Image);
      Assert (Expected_4 = Actual_4, "Unexpected trace: " & Actual_4'Image);
   end Test_Trace;

   procedure Test_Set_Value_Index_Row (Object : in out Test) is
      Tensor : CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Shape => (3, 2));
      --  1 2
      --  3 4
      --  5 6

      Expected : constant Element_Array := (1.0, 2.0, 7.0, 8.0, 5.0, 6.0);
   begin
      Tensor.Set (2, To_Tensor ((7.0, 8.0)));

      Assert_Equal (Expected, Tensor.Flatten);
   end Test_Set_Value_Index_Row;

   procedure Test_Set_Value_Index_Range (Object : in out Test) is
      Tensor_1 : CPU_Tensor := Linear_Space (1.0, 16.0, Count => 16).Reshape ((4, 4));
      --   1  2  3  4
      --   5  6  7  8
      --   9 10 11 12
      --  13 14 15 16

      Tensor_2 : constant CPU_Tensor := Linear_Space (2.0, 9.0, Count => 8).Reshape ((2, 4));

      Expected : constant Element_Array :=
        (1.0, 2.0, 3.0, 4.0,
         2.0, 3.0, 4.0, 5.0,
         6.0, 7.0, 8.0, 9.0,
         13.0, 14.0, 15.0, 16.0);
   begin
      Tensor_1.Set (Range_Type'(2, 3), Tensor_2);

      Assert_Equal (Expected, Tensor_1.Flatten);
   end Test_Set_Value_Index_Range;

   procedure Test_Constant_Indexing_Index_Row (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := Diagonal ((1.0, 2.0, 3.0));
      --  1 0 0
      --  0 2 0
      --  0 0 3

      Tensor_2 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0, 6.0));
      Tensor_3 : constant CPU_Tensor := Tensor_2.Reshape ((3, 2));
      --  1 2
      --  3 4
      --  5 6
   begin
      Assert_Equal ((1.0, 0.0, 0.0), Tensor_1 (1));
      Assert_Equal ((0.0, 2.0, 0.0), Tensor_1 (2));
      Assert_Equal ((0.0, 0.0, 3.0), Tensor_1 (3));

      Assert_Equal ((5.0, 6.0), Tensor_3 (3));
   end Test_Constant_Indexing_Index_Row;

   procedure Test_Constant_Indexing_Index_Value (Object : in out Test) is
      Main_Diagonal : constant Element_Array := (1.0, 2.0, 3.0);
      Tensor : constant CPU_Tensor := Diagonal (Main_Diagonal);
   begin
      for Index in Main_Diagonal'Range loop
         Assert (Main_Diagonal (Index) = Tensor ((Index, Index)),
           "Unexpected element: " & Element'Image (Tensor ((Index, Index))));
      end loop;
   end Test_Constant_Indexing_Index_Value;

   procedure Test_Constant_Indexing_Index_Boolean (Object : in out Test) is
      Values : constant Boolean_Array := (True, False, True);

      Main_Diagonal : constant Element_Array := (1.0, 2.0, 3.0);
      Tensor : constant CPU_Tensor := Diagonal (Main_Diagonal) /= 2.0;
   begin
      for Index in Values'Range loop
         Assert (Values (Index) = Tensor ((Index, Index)),
           "Unexpected element: " & Boolean'Image (Tensor ((Index, Index))));
      end loop;
   end Test_Constant_Indexing_Index_Boolean;

   procedure Test_Constant_Indexing_Range (Object : in out Test) is
      Tensor : constant CPU_Tensor := Diagonal ((1.0, 2.0, 3.0, 4.0));
      --  1 0 0 0
      --  0 2 0 0
      --  0 0 3 0
      --  0 0 0 4

      Expected_1 : constant CPU_Tensor := Tensor (Range_Type'(2, 3));
      --  0 2 0 0
      --  0 0 3 0

      Expected_2 : constant CPU_Tensor := Tensor (Tensor_Range'((3, 4), (2, 4)));
      --  0 3 0
      --  0 0 4

      Actual_1   : constant CPU_Tensor := To_Tensor ((0.0, 2.0, 0.0, 0.0, 0.0, 0.0, 3.0, 0.0))
        .Reshape ((2, 4));
      Actual_2   : constant CPU_Tensor := To_Tensor ((0.0, 3.0, 0.0, 0.0, 0.0, 4.0))
        .Reshape ((2, 3));
   begin
      Assert (Expected_1 = Actual_1, "Unexpected tensor of shape " & Image (Actual_1.Shape));
      Assert (Expected_2 = Actual_2, "Unexpected tensor of shape " & Image (Actual_2.Shape));
   end Test_Constant_Indexing_Range;

   procedure Test_Constant_Indexing_Tensor (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0, 6.0)).Reshape ((2, 3));
      --  1 2 3
      --  4 5 6

      Expected_1 : constant Element_Array := (5.0, 6.0);
      Expected_2 : constant Element_Array := (1.0, 2.0, 3.0, 4.0);
      Expected_3 : constant Element_Array := (2.0, 4.0, 6.0);

      Actual_1 : constant CPU_Tensor := Tensor (Tensor > 4.0);
      --  5 6

      Actual_2 : constant CPU_Tensor := Tensor (Tensor <= 4.0);
      --  1 2 3 4

      Actual_3 : constant CPU_Tensor := Tensor (Tensor mod 2.0 = 0.0);
      --  2 4 6

      Actual_4 : constant CPU_Tensor := Tensor (Tensor = 0.0);
   begin
      Assert_Equal (Expected_1, Actual_1);
      Assert_Equal (Expected_2, Actual_2);
      Assert_Equal (Expected_3, Actual_3);

      Assert (Actual_4.Elements = 0, "Unexpected number of elements: " & Actual_4.Elements'Image);
   end Test_Constant_Indexing_Tensor;

   procedure Test_Operator_Multiply_Inner (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := Array_Range (9.0).Reshape ((3, 3));
      Tensor_2 : constant CPU_Tensor := Array_Range (16.0).Reshape ((4, 4));

      Tensor_3 : constant CPU_Tensor := Array_Range (4.0);

      Expected_1 : constant Element_Array :=
        (15.0, 18.0, 21.0,
         42.0, 54.0, 66.0,
         69.0, 90.0, 111.0);

      Expected_2 : constant Element_Array :=
        (56.0,   62.0,  68.0,  74.0,
         152.0, 174.0, 196.0, 218.0,
         248.0, 286.0, 324.0, 362.0,
         344.0, 398.0, 452.0, 506.0);

      Expected_3 : constant Element_Array := (14.0, 38.0, 62.0, 86.0);

      Actual_1 : constant CPU_Tensor := Tensor_1 * Tensor_1;
      Actual_2 : constant CPU_Tensor := Tensor_2 * Tensor_2;

      --  1D vector and 2D vector
      Actual_3 : constant CPU_Tensor := Tensor_2 * Tensor_3;
      Actual_4 : constant CPU_Tensor := Tensor_2 * Tensor_3.Reshape ((4, 1));
   begin
      Assert_Equal (Expected_1, Actual_1.Flatten);
      Assert_Equal (Expected_2, Actual_2.Flatten);
      Assert_Equal (Expected_3, Actual_3.Flatten);
      Assert_Equal (Expected_3, Actual_4.Flatten);
   end Test_Operator_Multiply_Inner;

   procedure Test_Operator_Power (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := Array_Range (16.0).Reshape ((4, 4));
      Tensor_2 : constant CPU_Tensor :=
        To_Tensor ((1.0,  2.0, -3.0,
                    4.0, -5.0,  6.0,
                   -7.0,  8.0,  9.0)).Reshape ((3, 3));

      Expected_1 : constant CPU_Tensor := Identity (4);
      Expected_2 : constant CPU_Tensor := Tensor_1;
      Expected_3 : constant CPU_Tensor := Expected_2 * Tensor_1;
      Expected_4 : constant CPU_Tensor := Expected_3 * Tensor_1;

      Expected_5 : constant CPU_Tensor := Tensor_2.Inverse;
      Expected_6 : constant CPU_Tensor := Expected_5 ** 2;

      Expected_7 : constant Element_Array :=
        (2.0**1,    2.0**2,    2.0**(-3),
         2.0**4,    2.0**(-5), 2.0**6,
         2.0**(-7), 2.0**8,    2.0**9);

      Actual_1 : constant CPU_Tensor := Tensor_1 ** 0;
      Actual_2 : constant CPU_Tensor := Tensor_1 ** 1;
      Actual_3 : constant CPU_Tensor := Tensor_1 ** 2;
      Actual_4 : constant CPU_Tensor := Tensor_1 ** 3;

      Actual_5 : constant CPU_Tensor := Tensor_2 ** (-1);
      Actual_6 : constant CPU_Tensor := Tensor_2 ** (-2);

      Actual_7 : constant CPU_Tensor := 2.0 ** Tensor_2;
   begin
      Assert (Expected_1 = Actual_1, "Tensor ** 0 /= I");
      Assert (Expected_2 = Actual_2, "Tensor ** 1 /= Tensor");
      Assert (Expected_3 = Actual_3, "Tensor ** 2 /= Tensor * Tensor");
      Assert (Expected_4 = Actual_4, "Tensor ** 3 /= Tensor * Tensor * Tensor");

      Assert (Expected_5 = Actual_5, "Tensor ** -1 /= Tensor.Inverse");
      Assert (Expected_6 = Actual_6, "Tensor ** -2 /= Tensor.Inverse ** 2");

      Assert_Equal (Expected_7, Actual_7.Flatten);
   end Test_Operator_Power;

   procedure Test_Outer (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((1 => 1.0));

      Expected_1 : constant Element_Array := (1.0, 2.0, 3.0, 2.0, 4.0, 6.0, 3.0, 6.0, 9.0);
      Expected_2 : constant Element_Array := (1.0, 2.0, 3.0);
      Expected_3 : constant Element_Array := (1.0, 2.0, 3.0);

      Actual_1   : constant CPU_Tensor := Outer (Tensor_1, Tensor_1);
      Actual_2   : constant CPU_Tensor := Outer (Tensor_1, Tensor_2);
      Actual_3   : constant CPU_Tensor := Outer (Tensor_2, Tensor_1);
   begin
      Assert (Actual_1.Shape = (3, 3), "Unexpected shape: " & Image (Actual_1.Shape));
      Assert (Actual_2.Shape = (3, 1), "Unexpected shape: " & Image (Actual_2.Shape));
      Assert (Actual_3.Shape = (1, 3), "Unexpected shape: " & Image (Actual_3.Shape));

      Assert_Equal (Expected_1, Actual_1.Flatten);
      Assert_Equal (Expected_2, Actual_2.Flatten);
      Assert_Equal (Expected_3, Actual_3.Flatten);
   end Test_Outer;

   procedure Test_Inverse (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0,  2.0, 3.0, 4.0)).Reshape ((2, 2));
      pragma Assert (1.0 * 4.0 - 2.0 * 3.0 /= 0.0);

      Tensor_2 : constant CPU_Tensor :=
        To_Tensor ((1.0,  2.0, -3.0,
                    4.0, -5.0,  6.0,
                   -7.0,  8.0,  9.0)).Reshape ((3, 3));

      Tensor_3 : constant CPU_Tensor :=
        To_Tensor ((1.0, -2.0, -1.0,
                   -1.0,  5.0,  6.0,
                    5.0, -4.0,  5.0)).Reshape ((3, 3));

      Expected_1 : constant CPU_Tensor := Identity (2);
      Expected_2 : constant CPU_Tensor := Tensor_2;

      Actual_1 : constant CPU_Tensor := Tensor_1.Inverse * Tensor_1;
      Actual_2 : constant CPU_Tensor := Tensor_2.Inverse.Inverse;
   begin
      Assert (All_Close (Expected_1, Actual_1), "A^-1 * A /= I");
      Assert (All_Close (Expected_2, Actual_2), "(A^-1)^-1 /= A");

      begin
         declare
            Actual_3 : constant CPU_Tensor := Tensor_3.Inverse;
         begin
            Assert (False, "Tensor not singular");
         end;
      exception
         when Orka.Numerics.Singular_Matrix =>
            null;
      end;
   end Test_Inverse;

   procedure Test_Transpose (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := Linear_Space (1.0, 15.0, Count => 15).Reshape ((5, 3));
      Tensor_2 : constant CPU_Tensor := Linear_Space (1.0, 6.0, Count => 6).Reshape ((2, 3));

      Expected_1 : constant CPU_Tensor := To_Tensor
        ((1.0, 4.0, 7.0, 10.0, 13.0,
          2.0, 5.0, 8.0, 11.0, 14.0,
          3.0, 6.0, 9.0, 12.0, 15.0), Shape => (3, 5));

      Expected_2 : constant CPU_Tensor := To_Tensor
        ((1.0, 4.0,
          2.0, 5.0,
          3.0, 6.0), Shape => (3, 2));

      Actual_1 : constant CPU_Tensor := Tensor_1.Transpose;
      Actual_2 : constant CPU_Tensor := Tensor_2.Transpose;
   begin
      Assert (Expected_1 = Actual_1,
        "Unexpected transpose of " & Image (Actual_1.Shape) & " tensor");
      Assert (Expected_2 = Actual_2,
        "Unexpected transpose of " & Image (Actual_2.Shape) & " tensor");
   end Test_Transpose;

   procedure Test_Solve (Object : in out Test) is
      Tensor_A : constant CPU_Tensor :=
        To_Tensor ((1.0, -2.0,  1.0,
                    0.0,  2.0, -8.0,
                   -4.0,  5.0,  9.0)).Reshape ((3, 3));

      Tensor_B : constant CPU_Tensor := To_Tensor ((0.0, 8.0, -9.0));

      Solution : Solution_Kind;

      Expected : constant Element_Array := (29.0, 16.0, 3.0);
      Actual   : constant CPU_Tensor    := Solve (Tensor_A, Tensor_B, Solution);
   begin
      Assert_Equal (Expected, Actual);
      Assert (Solution = Unique, "Unexpected number of solutions: " & Solution'Image);
   end Test_Solve;

   procedure Test_Solve_Triangular (Object : in out Test) is
      Tensor_A : constant CPU_Tensor :=
        To_Tensor ((2.0, -3.0,  4.0,
                    0.0,  6.0, -8.0,
                    0.0,  0.0,  9.0)).Reshape ((3, 3));

      Tensor_B : constant CPU_Tensor := To_Tensor ((0.0, 8.0, -9.0));

      Expected : constant Element_Array := (2.0, 0.0, -1.0);
      Actual   : constant CPU_Tensor    := Solve (Tensor_A, Tensor_B, Upper);
   begin
      Assert_Equal (Expected, Actual);
   end Test_Solve_Triangular;

   procedure Test_Divide_By (Object : in out Test) is
      Tensor_A : constant CPU_Tensor := To_Tensor
        ((3.0, 4.0, 5.0,
          1.0, 3.0, 1.0,
          3.0, 5.0, 9.0),
         Shape => (3, 3));

      --  Upper
      Tensor_B : constant CPU_Tensor := To_Tensor
        ((4.0, 9.0, 2.0,
          0.0, 3.0, 6.0,
          0.0, 0.0, 2.0),
         Shape => (3, 3));

      --  Lower
      Tensor_C : constant CPU_Tensor := Tensor_B.Transpose;

      A_Slash_C1 : constant CPU_Tensor := Divide_By (Tensor_A, Tensor_C, Lower);
      A_Slash_C2 : constant CPU_Tensor := Divide_By (Tensor_A, Tensor_C);

      A_Slash_B1 : constant CPU_Tensor := Divide_By (Tensor_A, Tensor_B, Upper);
      A_Slash_B2 : constant CPU_Tensor := Divide_By (Tensor_A, Tensor_B);
   begin
      Assert (All_Close (A_Slash_C1, A_Slash_C2), "Results of Divide_By w/ and w/o form differs");
      Assert (All_Close (A_Slash_B1, A_Slash_B2), "Results of Divide_By w/ and w/o form differs");

      Assert (All_Close (A_Slash_B1 * Tensor_B, Tensor_A), "Unexpected value returned Divide_By");
      Assert (All_Close (A_Slash_C1 * Tensor_C, Tensor_A), "Unexpected value returned Divide_By");
   end Test_Divide_By;

   procedure Test_Upper_Triangular (Object : in out Test) is
      Tensor : constant CPU_Tensor := Linear_Space (1.0, 9.0, Count => 9).Reshape ((3, 3));
      --   1  2  3
      --   4  5  6
      --   7  8  9

      Expected : constant Element_Array := (1.0, 2.0, 3.0, 0.0, 5.0, 6.0, 0.0, 0.0, 9.0);

      Actual : constant CPU_Tensor := Tensor.Upper_Triangular;
   begin
      Assert_Equal (Expected, Actual.Flatten);
   end Test_Upper_Triangular;

   procedure Test_QR (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor :=
        To_Tensor ((12.0, -51.0,   4.0, 52.0, -20.1,
                     6.0, 167.0, -68.0, -1.0,  11.0,
                    -4.0,  24.0, -41.0,  0.0,   5.1,
                     2.0,   3.0,   4.0,  5.0,   6.0)).Reshape ((4, 5));

      Tensor_2 : constant CPU_Tensor :=
        To_Tensor ((12.0, -51.0,   4.0, 52.0,
                     6.0, 167.0, -68.0, -1.0,
                    -4.0,  24.0, -41.0,  0.0,
                     2.0,   3.0,   4.0,  5.0)).Reshape ((4, 4));

      Tensor_3 : constant CPU_Tensor :=
        To_Tensor ((12.0, -51.0,   4.0,
                     6.0, 167.0, -68.0,
                    -4.0,  24.0, -41.0,
                     2.0,   3.0,   4.0)).Reshape ((4, 3));

      QR_1 : constant CPU_QR_Factorization := CPU_QR_Factorization (QR (Tensor_1, Complete));
      QR_2 : constant CPU_QR_Factorization := CPU_QR_Factorization (QR (Tensor_2, Complete));
      QR_3 : constant CPU_QR_Factorization := CPU_QR_Factorization (QR (Tensor_3, Complete));

      Actual_1 : constant CPU_Tensor := QR_1.Q * QR_1.R;
      Actual_2 : constant CPU_Tensor := QR_2.Q * QR_2.R;
      Actual_3 : constant CPU_Tensor := QR_3.Q * QR_3.R;
   begin
      Assert (QR_1.Q.Shape = (Tensor_1.Shape (1), Tensor_1.Shape (1)),
        "Unexpected shape " & Image (QR_1.Q.Shape) & " of Q");
      Assert (QR_2.Q.Shape = (Tensor_2.Shape (1), Tensor_2.Shape (1)),
        "Unexpected shape " & Image (QR_2.Q.Shape) & " of Q");
      Assert (QR_3.Q.Shape = (Tensor_3.Shape (1), Tensor_3.Shape (1)),
        "Unexpected shape " & Image (QR_3.Q.Shape) & " of Q");

      Assert (QR_1.R.Shape = Tensor_1.Shape, "Unexpected shape " & Image (QR_1.R.Shape) & " of R");
      Assert (QR_2.R.Shape = Tensor_2.Shape, "Unexpected shape " & Image (QR_2.R.Shape) & " of R");
      Assert (QR_3.R.Shape = Tensor_3.Shape, "Unexpected shape " & Image (QR_3.R.Shape) & " of R");

      Assert (All_Close (Tensor_1, Actual_1, Absolute_Tolerance => Abs_Tolerance), "A /= Q * R");
      Assert (All_Close (Tensor_2, Actual_2, Absolute_Tolerance => Abs_Tolerance), "A /= Q * R");
      Assert (All_Close (Tensor_3, Actual_3, Absolute_Tolerance => Abs_Tolerance), "A /= Q * R");
   end Test_QR;

   procedure Test_Cholesky (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := Identity (Size => 3);
      Tensor_2 : constant CPU_Tensor := Tensor_1 * 2.0;
      Tensor_3 : constant CPU_Tensor := Tensor_2 + 1.0;
      Tensor_4 : constant CPU_Tensor := Tensor_2 - 1.0;

      Expected_1 : constant CPU_Tensor := Tensor_1;
      Expected_2 : constant CPU_Tensor := Tensor_2.Sqrt;

      Actual_1   : constant CPU_Tensor := Tensor_1.Cholesky;
      Actual_2   : constant CPU_Tensor := Tensor_2.Cholesky;
   begin
      Assert (All_Close (Expected_1, Actual_1), "Cholesky (I) /= I");
      Assert (All_Close (Expected_2, Actual_2), "Cholesky (I * 2) /= Sqrt (I)");

      begin
         declare
            Actual_3 : constant CPU_Tensor := Tensor_3.Cholesky;
         begin
            null;
         end;
      exception
         when Orka.Numerics.Not_Positive_Definite_Matrix =>
            Assert (False, "Unexpectedly raised exception Not_Positive_Definite_Matrix");
      end;

      begin
         declare
            Actual_4 : constant CPU_Tensor := Tensor_4.Cholesky;
         begin
            Assert (False, "Exception Not_Positive_Definite_Matrix not raised");
         end;
      exception
         when Orka.Numerics.Not_Positive_Definite_Matrix =>
            null;
      end;
   end Test_Cholesky;

   procedure Test_Cholesky_Downdate (Object : in out Test) is
      package Random is new Generic_Random (CPU_Tensor);

      N : constant Natural := 100;

      U : constant CPU_Tensor := Random.Uniform ((1 .. 2 => N));
      A : constant CPU_Tensor := CPU_Tensor'(U * U.Transpose) + Element (N) * Identity (N);

      R : constant CPU_Tensor := Cholesky (A, Upper);
      V : constant CPU_Tensor := Random.Uniform ((1 => N));

      D1 : constant CPU_Tensor := Cholesky (A - Outer (V, V), Upper);
      D2 : constant CPU_Tensor := Cholesky_Update (R, V, Downdate);
   begin
      --  A wider absolute tolerance than All_Close's default is needed
      --  for single precision elements
      Assert (All_Close (D1, D2, Absolute_Tolerance => 10.0 * Element'Model_Epsilon),
        "Unexpected downdated Cholesky factorization");
   end Test_Cholesky_Downdate;

   procedure Test_Shapes_Least_Squares (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor :=
        To_Tensor ((12.0, -51.0,   4.0, 52.0, -20.1,
                     6.0, 167.0, -68.0, -1.0,  11.0,
                    -4.0,  24.0, -41.0,  0.0,   5.1,
                     2.0,   3.0,   4.0,  5.0,   6.0)).Reshape ((4, 5));

      Tensor_2 : constant CPU_Tensor :=
        To_Tensor ((12.0, -51.0,   4.0, 52.0,
                     6.0, 167.0, -68.0, -1.0,
                    -4.0,  24.0, -41.0,  0.0,
                     2.0,   3.0,   4.0,  5.0)).Reshape ((4, 4));

      Tensor_3 : constant CPU_Tensor :=
        To_Tensor ((12.0, -51.0,   4.0,
                     6.0, 167.0, -68.0,
                    -4.0,  24.0, -41.0,
                     2.0,   3.0,   4.0)).Reshape ((4, 3));

      B1 : constant CPU_Tensor := To_Tensor ((123.456, 78.901, 65.34, -5.34), Shape => (4, 1));
      B2 : constant CPU_Tensor := To_Tensor ((459.014, 25.146, 195.0, 0.12), Shape => (4, 1));

      B_1D : constant CPU_Tensor := To_Tensor ((123.456, 78.901, 65.34, -5.34));
      B_2D : constant CPU_Tensor := Concatenate (B1, B2, Dimension => 2);

      --  Test matrices with 1-D and 2-D B's
      procedure Test_Shape_Least_Squares (A : CPU_Tensor) is
         QR_A : constant CPU_QR_Factorization := CPU_QR_Factorization (QR_For_Least_Squares (A));

         X_1D : constant CPU_Tensor := Least_Squares (QR_A, B_1D);
         X_2D : constant CPU_Tensor := Least_Squares (QR_A, B_2D);

         Y_1D : constant CPU_Tensor := Least_Squares (A, B_1D);
         Y_2D : constant CPU_Tensor := Least_Squares (A, B_2D);
      begin
         Assert (X_1D.Dimensions = B_1D.Dimensions, "Unexpected number of dimensions of X");
         Assert (X_2D.Dimensions = B_2D.Dimensions, "Unexpected number of dimensions of X");

         Assert (X_2D.Shape (2) = B_2D.Shape (2), "Columns of X /= columns B");

         Assert (X_1D.Shape (1) = A.Shape (2), "Rows of X /= columns of A");
         Assert (X_2D.Shape (1) = A.Shape (2), "Rows of X /= columns of A");

         Assert (All_Close (X_1D, Y_1D), "Unxpected least-squares solution");
         Assert (All_Close (X_2D, Y_2D), "Unxpected least-squares solution");
      end Test_Shape_Least_Squares;
   begin
      Test_Shape_Least_Squares (Tensor_1);
      Test_Shape_Least_Squares (Tensor_2);
      Test_Shape_Least_Squares (Tensor_3);
   end Test_Shapes_Least_Squares;

   procedure Test_Values_Least_Squares (Object : in out Test) is
      Tensor : constant CPU_Tensor :=
        To_Tensor ((1.0,  5.0,
                    1.0, -2.0,
                    1.0, -4.0,
                    1.0,  1.0)).Reshape ((4, 2));

      QR_Tensor : constant CPU_QR_Factorization :=
        CPU_QR_Factorization (QR_For_Least_Squares (Tensor));

      B : constant CPU_Tensor := To_Tensor ((2.0, 3.0, -3.0, 7.0));

      Expected : constant CPU_Tensor := To_Tensor ((9.0 / 4.0, 23.0 / 46.0));
      Actual   : constant CPU_Tensor := Least_Squares (QR_Tensor, B);
   begin
      Assert (All_Close (Expected, Actual), "Unexpected least-squares solution");

      --  Test orthogonal projection of B is A * x and Q * Q^T * b
      --  where Q is the reduced orthogonal matrix
      declare
         QQT : constant CPU_Tensor := QR_Tensor.Q * QR_Tensor.Q.Transpose;

         Ax   : constant CPU_Tensor := Tensor * Actual;
         QQTb : constant CPU_Tensor := QQT * B;
      begin
         Assert (All_Close (Ax, QQTb), "Unexpected orthogonal projection");
      end;
   end Test_Values_Least_Squares;

   procedure Test_Constrained_Least_Squares (Object : in out Test) is
      --  Let A = I and b = 0 to reduce constrained least-squares problem
      --  to the least norm problem
      I    : constant CPU_Tensor := Identity (Size => 10);
      Zero : constant CPU_Tensor := Zeros (Elements => 10);

      --  This example is from Section 16.1.1 of [1] in which
      --  a sequence of 10 forces are applied (m = 1.0 kg) to
      --  achieve a final velocity of 0.0 m/s and a position of 1.0 m.
      --
      --  A force i (in 1 .. 10) is applied during t in i - 1 .. i.
      --  Afterwards velocity of object is constant, thus change in position
      --  because of force i is (10.0 - i + 0.5) m.
      --
      --  [1] "Introduction to Applied Linear Algebra", Boyd S., Vandenberge L., 2018
      C : constant CPU_Tensor :=
        To_Tensor ((1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
                    9.5, 8.5, 7.5, 6.5, 5.5, 4.5, 3.5, 2.5, 1.5, 0.5)).Reshape ((2, 10));
      D : constant CPU_Tensor := To_Tensor ((0.0, 1.0), Shape => (2, 1));

      Expected : constant Element := 0.0121212121212121;
      Actual   : constant Element := Constrained_Least_Squares (I, Zero, C, D).Norm**2;
   begin
      Assert (Expected, Actual, "Unexpected squared norm of constrained least-squares solution");
   end Test_Constrained_Least_Squares;

   procedure Test_Any_True (Object : in out Test) is
   begin
      Assert (False, "FIXME");
   end Test_Any_True;

   procedure Test_All_True (Object : in out Test) is
   begin
      Assert (False, "FIXME");
   end Test_All_True;

   procedure Test_Reduction_Binary_Operator (Object : in out Test) is
   begin
      Assert (False, "FIXME");
   end Test_Reduction_Binary_Operator;

   procedure Test_Reduction_Number (Object : in out Test) is
   begin
      Assert (False, "FIXME");
   end Test_Reduction_Number;

   ----------------------------------------------------------------------------

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(Tensors - " & Suite_Name & " - Matrices) ";
   begin
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Flatten", Test_Flatten'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Identity (square)", Test_Identity_Square'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Identity (not square)", Test_Identity_Not_Square'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Reshape", Test_Reshape'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Concatenate", Test_Concatenate'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Main_Diagonal", Test_Main_Diagonal'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Diagonal", Test_Diagonal'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Trace", Test_Trace'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Test indexing row using index", Test_Constant_Indexing_Index_Row'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test indexing value using index", Test_Constant_Indexing_Index_Value'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test indexing value using index (boolean)",
         Test_Constant_Indexing_Index_Boolean'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test indexing using range", Test_Constant_Indexing_Range'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test indexing using tensor", Test_Constant_Indexing_Tensor'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Test set row using index", Test_Set_Value_Index_Row'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test set row using range", Test_Set_Value_Index_Range'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '*' operator (inner product)", Test_Operator_Multiply_Inner'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '**'", Test_Operator_Power'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Outer (outer product)", Test_Outer'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Inverse", Test_Inverse'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Transpose", Test_Transpose'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Solve", Test_Solve'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Solve (triangular)", Test_Solve_Triangular'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Divide_By", Test_Divide_By'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Upper_Triangular", Test_Upper_Triangular'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function QR", Test_QR'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Cholesky", Test_Cholesky'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Cholesky_Update (downdate)", Test_Cholesky_Downdate'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Least_Squares (shapes)", Test_Shapes_Least_Squares'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Least_Squares (values)", Test_Values_Least_Squares'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Constrained_Least_Squares", Test_Constrained_Least_Squares'Access));

      --  TODO Statistics: Min, Max, Quantile, Median, Mean, Variance (with Dimension parameter)

      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Any_True", Test_Any_True'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function All_True", Test_All_True'Access));

      --  Expressions
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test reduction binary operator", Test_Reduction_Binary_Operator'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test reduction number", Test_Reduction_Number'Access));

      --  TODO Cumulative, Reduce (with Dimension parameter)

      return Test_Suite'Access;
   end Suite;

end Generic_Test_Tensors_Matrices;
