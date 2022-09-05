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

with Ada.Numerics.Generic_Elementary_Functions;

with AUnit.Assertions;
with AUnit.Test_Caller;
with AUnit.Test_Fixtures;

with Orka.OS;

package body Generic_Test_Tensors_Vectors is

   use Tensors;
   use SIMD_CPU;
   use type Tensors.Element;

   use AUnit.Assertions;

   Sizes : constant array (Positive range <>) of Natural := (1, 2, 4, 5, 8, 10);

   Pi : constant := Ada.Numerics.Pi;

   package EF is new Ada.Numerics.Generic_Elementary_Functions (Element);

   package Random is new Generic_Random (CPU_Tensor);

   procedure Assert (Expected, Result : Element; Message : String) is
      function Is_Similar (Expected, Result : Element) return Boolean is
        (abs (Result - Expected) <= Element'Model_Epsilon + 1.0e-05 * abs Expected);
   begin
      Assert (Is_Similar (Expected, Result), Message);
   end Assert;

   procedure Assert_Shape_And_Elements
     (Tensor   : CPU_Tensor;
      Shape    : Tensor_Shape;
      Size     : Natural;
      Expected : Element) is
   begin
      Assert (Tensor.Elements = Size,
        "Unexpected size: " & Tensor.Elements'Image & " for size " & Size'Image);
      Assert (Tensor.Dimensions = 1,
        "Expected dimensions: " & Tensor.Dimensions'Image & " for size " & Size'Image);
      Assert (Tensor.Shape = Shape,
        "Unexpected shape: " & Image (Tensor.Shape) & " for size " & Size'Image);

      for I in 1 .. Size loop
         declare
            Value : constant Element := Tensor (I);
         begin
            Assert (Expected = Value, "Unexpected element: " & Value'Image);
         end;
      end loop;
   end Assert_Shape_And_Elements;

   procedure Assert_Equal (Expected : Element_Array; Actual : CPU_Tensor) is
   begin
      Assert (Actual.Elements = Expected'Length,
        "Unexpected size of tensor: " & Actual.Elements'Image);

      for I in Expected'Range loop
         Assert (Expected (I), Actual (I),
           "Unexpected element at index " & I'Image & ": " &
           Element'Image (Actual (I)) & " instead of " & Element'Image (Expected (I)));
      end loop;
   end Assert_Equal;

   procedure Assert_Boolean_Equal (Expected : Boolean_Array; Actual : CPU_Tensor) is
   begin
      Assert (Actual.Elements = Expected'Length,
        "Unexpected size of tensor: " & Actual.Elements'Image);

      for I in Expected'Range loop
         Assert (Expected (I) = Actual (I),
           "Unexpected element at index " & I'Image & ": " &
           Boolean'Image (Actual (I)) & " instead of " & Boolean'Image (Expected (I)));
      end loop;
   end Assert_Boolean_Equal;

   ----------------------------------------------------------------------------

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Zeros (Object : in out Test) is
   begin
      for I of Sizes loop
         declare
            Shape  : constant Tensor_Shape := (1 => I);

            Tensor_1 : constant CPU_Tensor := Zeros (I);
            Tensor_2 : constant CPU_Tensor := Zeros (Shape);
         begin
            Assert_Shape_And_Elements (Tensor_1, Shape, I, 0.0);
            Assert_Shape_And_Elements (Tensor_2, Shape, I, 0.0);
         end;
      end loop;
   end Test_Zeros;

   procedure Test_Ones (Object : in out Test) is
   begin
      for I of Sizes loop
         declare
            Shape  : constant Tensor_Shape := (1 => I);

            Tensor_1 : constant CPU_Tensor := Ones (I);
            Tensor_2 : constant CPU_Tensor := Ones (Shape);
         begin
            Assert_Shape_And_Elements (Tensor_1, Shape, I, 1.0);
            Assert_Shape_And_Elements (Tensor_2, Shape, I, 1.0);
         end;
      end loop;
   end Test_Ones;

   procedure Test_To_Tensor (Object : in out Test) is
      procedure Test_Arrays (Values : Element_Array) is
         Tensor : constant CPU_Tensor := To_Tensor (Values);
      begin
         Assert (Tensor.Elements = Values'Length,
           "Unexpected size of tensor: " & Tensor.Elements'Image);
         Assert_Equal (Values, Tensor);
      end Test_Arrays;
   begin
      for I of Sizes loop
         declare
            Values : constant Element_Array (1 .. I) := (others => Element (I));
         begin
            Test_Arrays (Values);
         end;
      end loop;
   end Test_To_Tensor;

   procedure Test_Linear_Space (Object : in out Test) is
      Expected_1 : constant Element_Array := (1.0, 2.0, 3.0, 4.0, 5.0);
      Expected_2 : constant Element_Array := (1.0, 1.8, 2.6, 3.4, 4.2);
      Expected_3 : constant Element_Array := (1.0, 0.75, 0.5, 0.25, 0.0);
      Expected_4 : constant Element_Array := (1.0, 0.8, 0.6, 0.4, 0.2);
      Expected_5 : constant Element_Array := (1.0, 1.0, 1.0);
      Expected_6 : constant Element_Array := (0.0, 0.0, 0.0);

      --  Increasing
      Actual_1 : constant CPU_Tensor := Linear_Space (1.0, 5.0, Count => 5);
      Actual_2 : constant CPU_Tensor := Linear_Space (1.0, 5.0, Count => 5, Interval => Half_Open);

      --  Decreasing
      Actual_3 : constant CPU_Tensor := Linear_Space (1.0, 0.0, Count => 5);
      Actual_4 : constant CPU_Tensor := Linear_Space (1.0, 0.0, Count => 5, Interval => Half_Open);

      --  Degenerate
      Actual_5 : constant CPU_Tensor := Linear_Space (1.0, 1.0, Count => 3);
      Actual_6 : constant CPU_Tensor := Linear_Space (0.0, 0.0, Count => 3, Interval => Half_Open);
   begin
      Assert_Equal (Expected_1, Actual_1);
      Assert_Equal (Expected_2, Actual_2);
      Assert_Equal (Expected_3, Actual_3);
      Assert_Equal (Expected_4, Actual_4);
      Assert_Equal (Expected_5, Actual_5);
      Assert_Equal (Expected_6, Actual_6);
   end Test_Linear_Space;

   procedure Test_Log_Space (Object : in out Test) is
      use EF;

      Expected_1 : constant Element_Array := (1.0e1, 1.0e2, 1.0e3, 1.0e4);
      Expected_2 : constant Element_Array := (1.0e4, 1.0e3, 1.0e2, 1.0e1);
      Expected_3 : constant Element_Array := (10.0**1.0, 10.0**1.75, 10.0**2.5, 10.0**3.25);
      Expected_4 : constant Element_Array := (1.0, 1.0, 1.0, 1.0);
      Expected_5 : constant Element_Array := (1.0, 1.0, 1.0, 1.0);
      Expected_6 : constant Element_Array := (2.0, 4.0, 8.0);

      --  Increasing
      Actual_1 : constant CPU_Tensor := Log_Space (1.0, 4.0, Count => 4);

      --  Decreasing
      Actual_2 : constant CPU_Tensor := Log_Space (4.0, 1.0, Count => 4);

      Actual_3 : constant CPU_Tensor := Log_Space (1.0, 4.0, Count => 4, Interval => Half_Open);

      --  Degenerate
      Actual_4 : constant CPU_Tensor := Log_Space (0.0, 0.0, Count => 4);
      Actual_5 : constant CPU_Tensor := Log_Space (0.0, 0.0, Count => 4, Interval => Half_Open);

      --  Base
      Actual_6 : constant CPU_Tensor := Log_Space (1.0, 3.0, Count => 3, Base => 2.0);
   begin
      Assert_Equal (Expected_1, Actual_1);
      Assert_Equal (Expected_2, Actual_2);
      Assert_Equal (Expected_3, Actual_3);
      Assert_Equal (Expected_4, Actual_4);
      Assert_Equal (Expected_5, Actual_5);
      Assert_Equal (Expected_6, Actual_6);
   end Test_Log_Space;

   procedure Test_Geometric_Space (Object : in out Test) is
      Expected_1 : constant Element_Array := (1.0, 10.0, 100.0, 1_000.0);
      Expected_2 : constant Element_Array := (1.0, 10.0, 100.0);
      Expected_3 : constant Element_Array := (64.0, 32.0, 16.0, 8.0, 4.0, 2.0);

      Actual_1 : constant CPU_Tensor := Geometric_Space (1.0, 1_000.0, 4);
      Actual_2 : constant CPU_Tensor := Geometric_Space (1.0, 1_000.0, 3, Interval => Half_Open);

      Actual_3 : constant CPU_Tensor := Geometric_Space (64.0, 2.0, 6, Base => 2.0);
      --  Base needed here to prevent precision-loss with singles
   begin
      Assert_Equal (Expected_1, Actual_1);
      Assert_Equal (Expected_2, Actual_2);
      Assert_Equal (Expected_3, Actual_3);
   end Test_Geometric_Space;

   procedure Test_Array_Range (Object : in out Test) is
      Expected_1 : constant Element_Array := (0.0, 1.0, 2.0);
      Expected_2 : constant Element_Array := (2.0, 3.0, 4.0);
      Expected_3 : constant Element_Array := (1.0, 1.5, 2.0);

      Actual_1 : constant CPU_Tensor := Array_Range (3.0);
      Actual_2 : constant CPU_Tensor := Array_Range (2.0, 5.0);
      Actual_3 : constant CPU_Tensor := Array_Range (1.0, 2.5, 0.5);
   begin
      Assert_Equal (Expected_1, Actual_1);
      Assert_Equal (Expected_2, Actual_2);
      Assert_Equal (Expected_3, Actual_3);
   end Test_Array_Range;

   procedure Test_Set_Value_Index (Object : in out Test) is
      Tensor : CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0));

      Expected : constant Element_Array := (1.0, 4.0, 5.0);
   begin
      Tensor.Set ((1 => 2), 4.0);
      Tensor.Set (3, 5.0);

      Assert_Equal (Expected, Tensor);
   end Test_Set_Value_Index;

   procedure Test_Set_Value_Index_Boolean (Object : in out Test) is
      Tensor : CPU_Tensor := To_Boolean_Tensor ((True, False, False, True, False));

      Expected : constant Boolean_Array := (False, True, False, False, True);
   begin
      Tensor.Set ((1 => 2), True);
      Tensor.Set ((1 => 4), False);
      Tensor.Set (1, False);
      Tensor.Set (5, True);

      Assert_Boolean_Equal (Expected, Tensor);
   end Test_Set_Value_Index_Boolean;

   procedure Test_Constant_Indexing_Index (Object : in out Test) is
      Values : constant Element_Array := (1.0, 2.0, 3.0, 4.0, 5.0);
      Tensor : constant CPU_Tensor := To_Tensor (Values);
   begin
      for I in Values'Range loop
         Assert (Values (I) = Tensor (I), "Unexpected element at index " & I'Image);
      end loop;
   end Test_Constant_Indexing_Index;

   procedure Test_Constant_Indexing_Index_Boolean (Object : in out Test) is
      Values : constant Boolean_Array := (False, True, False, True, True, False);
      Tensor : constant CPU_Tensor := To_Boolean_Tensor (Values);
   begin
      for I in Values'Range loop
         Assert (Values (I) = Tensor (I), "Unexpected element at index " & I'Image);
      end loop;
   end Test_Constant_Indexing_Index_Boolean;

   procedure Test_Constant_Indexing_Range (Object : in out Test) is
      Values : constant Element_Array := (1.0, 2.0, 3.0, 4.0, 5.0);
      Tensor : constant CPU_Tensor := To_Tensor (Values);
   begin
      for I in 1 .. 3 loop
         declare
            Actual : constant CPU_Tensor := Tensor (Range_Type'(Start => I, Stop => I + I - 1));
         begin
            Assert (Actual.Elements = I, "Unexpected size of tensor: " & Actual.Elements'Image);

            for J in 0 .. Actual.Elements - 1 loop
               Assert (Element (I + J) = Actual (J + 1), "Unexpected element at index " & I'Image);
            end loop;
         end;
      end loop;
   end Test_Constant_Indexing_Range;

   procedure Test_Constant_Indexing_Tensor (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 5.0, 6.0, 7.0, 8.0, 9.0));

      Expected_1 : constant Element_Array := (1.0, 2.0);
      Expected_2 : constant Element_Array := (1.0, 2.0, 4.0, 5.0);
      Expected_3 : constant Element_Array := (1.0, 3.0, 5.0, 7.0, 9.0);

      Actual_1   : constant CPU_Tensor := Tensor_1 (Tensor_1 < 3.0);
      Actual_2   : constant CPU_Tensor := Tensor_1 (Tensor_1 /= 3.0);
      Actual_3   : constant CPU_Tensor := Tensor_2 (Tensor_2 mod 2.0 /= 0.0);
   begin
      Assert_Equal (Expected_1, Actual_1);
      Assert_Equal (Expected_2, Actual_2);
      Assert_Equal (Expected_3, Actual_3);
   end Test_Constant_Indexing_Tensor;

   procedure Test_Operator_Concatenate (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := Ones (4);
      Tensor_2 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0));
      Tensor_3 : constant CPU_Tensor := Zeros (2);

      Expected_1 : constant Element_Array := (1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 3.0, 0.0, 0.0);
      Expected_2 : constant Element_Array := (0.0, 0.0, 1.0, 2.0, 3.0);

      Actual_1   : constant CPU_Tensor := Tensor_1 & Tensor_2 & Tensor_3;
      Actual_2   : constant CPU_Tensor := Tensor_3 & Tensor_2;
   begin
      Assert_Equal (Expected_1, Actual_1);
      Assert_Equal (Expected_2, Actual_2);
   end Test_Operator_Concatenate;

   procedure Test_Operator_Multiply_Inner (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((2.0, 3.0, 4.0, 5.0, 6.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0));

      Expected : constant Element := 70.0;
      Actual   : constant Element := Tensor_1 * Tensor_2;
   begin
      Assert (Expected = Actual, "Unexpected element: " & Actual'Image);
   end Test_Operator_Multiply_Inner;

   procedure Test_Norm (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0));

      Expected : constant Element := EF.Sqrt (1.0**2 + 2.0**2 + 3.0**2 + 4.0**2 + 5.0**2);
      Actual   : constant Element := Tensor.Norm;
   begin
      Assert (Expected = Actual, "Unexpected norm: " & Actual'Image);
   end Test_Norm;

   procedure Test_Normalize (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0)).Normalize;

      Expected : constant Element := 1.0;
      Actual   : constant Element := Tensor.Norm;
   begin
      Assert (Expected = Actual, "Unexpected norm after normalization: " & Actual'Image);
   end Test_Normalize;

   procedure Test_Standardize (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0)).Standardize;

      Expected_Mean : constant Element := 0.0;
      Expected_Std  : constant Element := 1.0;

      Actual_Mean  : constant Element := Tensor.Mean;
      Actual_Std   : constant Element := Tensor.Standard_Deviation;
   begin
      Assert (Expected_Mean, Actual_Mean, "Unexpected mean: " & Actual_Mean'Image);
      Assert (Expected_Std, Actual_Std, "Unexpected standard deviation: " & Actual_Std'Image);
   end Test_Standardize;

   procedure Test_Correlation_Coefficient (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((3.0, 0.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((0.0, 3.0));
      Tensor_3 : constant CPU_Tensor := To_Tensor ((-3.0, 0.0));
      Tensor_4 : constant CPU_Tensor := To_Tensor ((2.0, 2.0));

      --  Create a line with positions (x_i, y_i) where each x_i is a value
      --  from Tensor_5 (the horizontal axis), while y_i are values from
      --  Tensor_6 (vertical axis). The line will zigzag in a horizontal
      --  direction, indicating the two tensors are uncorrelated.
      --  An increasing line would indicate positive correlation, while
      --  a decreasing line indicates negative correlation
      Tensor_5 : constant CPU_Tensor := To_Tensor ((0.0,  1.0, 2.0,  3.0, 4.0,  5.0, 6.0));
      Tensor_6 : constant CPU_Tensor := To_Tensor ((1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0));

      Expected_1 : constant Element := 0.0;
      Expected_2 : constant Element := -1.0;
      Expected_3 : constant Element := 1.0;
      Expected_4 : constant Element := 0.0;

      Actual_1 : constant Element := Correlation_Coefficient (Tensor_5, Tensor_6);
      Actual_2 : constant Element := Correlation_Coefficient (Tensor_1, Tensor_3);
      Actual_3 : constant Element := Correlation_Coefficient (Tensor_2, Tensor_2);
      Actual_4 : constant Element := Correlation_Coefficient (Tensor_1, Tensor_4);
   begin
      Assert (Expected_1 = Actual_1, "Unexpected correlation coeff.: " & Actual_1'Image);
      Assert (Expected_2 = Actual_2, "Unexpected correlation coeff.: " & Actual_2'Image);
      Assert (Expected_3 = Actual_3, "Unexpected correlation coeff.: " & Actual_3'Image);
      Assert (Expected_4 = Actual_4, "Unexpected correlation coeff.: " & Actual_4'Image);
   end Test_Correlation_Coefficient;

   procedure Test_Operator_Add_Tensors (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((4.0, 5.0, 6.0));

      Expected : constant Element_Array := (5.0, 7.0, 9.0);
      Actual   : constant CPU_Tensor := Tensor_1 + Tensor_2;
   begin
      Assert_Equal (Expected, Actual);
   end Test_Operator_Add_Tensors;

   procedure Test_Operator_Add_Element_Tensor (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0));

      Expected : constant Element_Array := (3.0, 4.0, 5.0);

      Actual_1 : constant CPU_Tensor := 2.0 + Tensor;
      Actual_2 : constant CPU_Tensor := Tensor + 2.0;
   begin
      Assert_Equal (Expected, Actual_1);
      Assert_Equal (Expected, Actual_2);
   end Test_Operator_Add_Element_Tensor;

   procedure Test_Operator_Subtract_Tensors (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0, 5.0, 0.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((4.0, 2.0, 6.0));

      Expected : constant Element_Array := (-3.0, 3.0, -6.0);
      Actual   : constant CPU_Tensor := Tensor_1 - Tensor_2;
   begin
      Assert_Equal (Expected, Actual);
   end Test_Operator_Subtract_Tensors;

   procedure Test_Operator_Subtract_Element_Tensor (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0));
   begin
      Assert_Equal ((1.0, 0.0, -1.0), 2.0 - Tensor);
      Assert_Equal ((-1.0, 0.0, 1.0), Tensor - 2.0);
      Assert_Equal ((-1.0, -2.0, -3.0), -Tensor);
   end Test_Operator_Subtract_Element_Tensor;

   procedure Test_Operator_Power_Tensors (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((0.0, 0.0, 0.5, 0.5, 0.5, 1.0, 1.0, 0.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 0.0, 1.0, 2.0, 0.0, 1.0, 0.0));

      Expected : constant Element_Array := (0.0, 0.0, 1.0, 0.5, 0.25, 1.0, 1.0, 1.0);
      Actual   : constant CPU_Tensor := Tensor_1 ** Tensor_2;
   begin
      Assert_Equal (Expected, Actual);
   end Test_Operator_Power_Tensors;

   procedure Test_Operator_Power_Element_Tensor (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((0.0, 0.5, 1.0, 2.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((0.1, 0.5, 1.0, 2.0));
   begin
      Assert_Equal ((1.0, 1.0, 1.0, 1.0), 1.0 ** Tensor_1);
      Assert_Equal ((0.0, 0.0, 0.0, 0.0), 0.0 ** Tensor_2);
      Assert_Equal ((1.0, 1.0, 1.0, 1.0), Tensor_2 ** 0.0);
      Assert_Equal ((0.0, 0.5, 1.0, 2.0), Tensor_1 ** 1.0);
   end Test_Operator_Power_Element_Tensor;

   procedure Test_Function_Power_Tensor_Element (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((0.0, 0.5, 1.0, 2.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((0.5, 1.0, 2.0));

      Expected_1 : constant Element_Array := (1.0, 1.0, 1.0, 1.0);
      Expected_2 : constant Element_Array := (0.0, 0.5, 1.0, 2.0);
      Expected_3 : constant Element_Array := (0.0, 0.25, 1.0, 4.0);

      Expected_4 : constant Element_Array := (2.0, 1.0, 0.5);

      Actual_1 : constant CPU_Tensor := Power (Tensor_1, 0);
      Actual_2 : constant CPU_Tensor := Power (Tensor_1, 1);
      Actual_3 : constant CPU_Tensor := Power (Tensor_1, 2);
      Actual_4 : constant CPU_Tensor := Power (Tensor_2, -1);
   begin
      Assert_Equal (Expected_1, Actual_1);
      Assert_Equal (Expected_2, Actual_2);
      Assert_Equal (Expected_3, Actual_3);
      Assert_Equal (Expected_4, Actual_4);
   end Test_Function_Power_Tensor_Element;

   procedure Test_Function_Multiply_Tensors (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((-1.0, 0.0, 2.0));

      Expected : constant Element_Array := (-1.0, 0.0, 6.0);
      Actual   : constant CPU_Tensor := Multiply (Tensor_1, Tensor_2);
   begin
      Assert_Equal (Expected, Actual);
   end Test_Function_Multiply_Tensors;

   procedure Test_Operator_Multiply_Element_Tensor (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0));

      Expected : constant Element_Array := (2.0, 4.0, 6.0);

      Actual_1 : constant CPU_Tensor := 2.0 * Tensor;
      Actual_2 : constant CPU_Tensor := Tensor * 2.0;
   begin
      Assert_Equal (Expected, Actual_1);
      Assert_Equal (Expected, Actual_2);
   end Test_Operator_Multiply_Element_Tensor;

   procedure Test_Operator_Divide_Tensors (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0, 0.0, 3.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((-1.0, 2.0, 2.0));

      Expected : constant Element_Array := (-1.0, 0.0, 1.5);
      Actual   : constant CPU_Tensor := Tensor_1 / Tensor_2;
   begin
      Assert_Equal (Expected, Actual);
   end Test_Operator_Divide_Tensors;

   procedure Test_Operator_Divide_Element_Tensor (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 4.0));

      Expected_1 : constant Element_Array := (2.0, 1.0, 0.5);
      Expected_2 : constant Element_Array := (0.5, 1.0, 2.0);

      Actual_1 : constant CPU_Tensor := 2.0 / Tensor;
      Actual_2 : constant CPU_Tensor := Tensor / 2.0;
   begin
      Assert_Equal (Expected_1, Actual_1);
      Assert_Equal (Expected_2, Actual_2);
   end Test_Operator_Divide_Element_Tensor;

   procedure Test_Divide_Or_Zero (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((-1.0, 0.0, 3.0, 2.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((0.0, 2.0, 2.0, 0.0));

      Expected : constant Element_Array := (0.0, 0.0, 1.5, 0.0);
      Actual   : constant CPU_Tensor    := Divide_Or_Zero (Tensor_1, Tensor_2);
   begin
      Assert_Equal (Expected, Actual);
   end Test_Divide_Or_Zero;

   procedure Test_Operator_Mod_Tensors (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((8.0, -8.0, 8.0, -8.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((5.0, 5.0, -5.0, -5.0));

      Expected : constant Element_Array := (3.0, 2.0, -2.0, -3.0);
      Actual   : constant CPU_Tensor := Tensor_1 mod Tensor_2;
   begin
      Assert_Equal (Expected, Actual);
   end Test_Operator_Mod_Tensors;

   procedure Test_Operator_Mod_Element_Tensor (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((8.0, -8.0));

      Expected : constant Element_Array := (3.0, 2.0);
      Actual   : constant CPU_Tensor := Tensor mod 5.0;
   begin
      Assert_Equal (Expected, Actual);
   end Test_Operator_Mod_Element_Tensor;

   procedure Test_Operator_Rem_Tensors (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((8.0, -8.0, 8.0, -8.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((5.0, 5.0, -5.0, -5.0));

      Expected : constant Element_Array := (3.0, -3.0, 3.0, -3.0);
      Actual   : constant CPU_Tensor := Tensor_1 rem Tensor_2;
   begin
      Assert_Equal (Expected, Actual);
   end Test_Operator_Rem_Tensors;

   procedure Test_Operator_Rem_Element_Tensor (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((8.0, -8.0));

      Expected : constant Element_Array := (3.0, -3.0);
      Actual   : constant CPU_Tensor := Tensor rem 5.0;
   begin
      Assert_Equal (Expected, Actual);
   end Test_Operator_Rem_Element_Tensor;

   procedure Test_Operator_Abs (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((-1.0, 0.0, 2.0));

      Expected : constant Element_Array := (1.0, 0.0, 2.0);
      Actual   : constant CPU_Tensor := abs Tensor;
   begin
      Assert_Equal (Expected, Actual);
   end Test_Operator_Abs;

   procedure Test_Function_Sqrt (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((0.0, 1.0, 4.0));

      Expected : constant Element_Array := (0.0, 1.0, 2.0);
      Actual   : constant CPU_Tensor := Sqrt (Tensor);
   begin
      Assert_Equal (Expected, Actual);
   end Test_Function_Sqrt;

   procedure Test_Function_Ceil (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((-1.0, 0.0, -0.5, 0.5, -1.5, 1.5));

      Expected : constant Element_Array := (-1.0, 0.0, 0.0, 1.0, -1.0, 2.0);
      Actual   : constant CPU_Tensor := Ceil (Tensor);
   begin
      Assert_Equal (Expected, Actual);
   end Test_Function_Ceil;

   procedure Test_Function_Floor (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((-1.0, 0.0, -0.5, 0.5, -1.5, 1.5));

      Expected : constant Element_Array := (-1.0, 0.0, -1.0, 0.0, -2.0, 1.0);
      Actual   : constant CPU_Tensor := Floor (Tensor);
   begin
      Assert_Equal (Expected, Actual);
   end Test_Function_Floor;

   procedure Test_Function_Round (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((-1.0, 0.0, -0.5, 0.5, -0.6, 0.6, -1.5, 1.5));

      Expected : constant Element_Array := (-1.0, 0.0, 0.0, 0.0, -1.0, 1.0, -2.0, 2.0);
      Actual   : constant CPU_Tensor := Round (Tensor);
   begin
      Assert_Equal (Expected, Actual);
   end Test_Function_Round;

   procedure Test_Function_Truncate (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((-1.0, 0.0, -0.5, 0.5, -1.5, 1.5));

      Expected : constant Element_Array := (-1.0, 0.0, 0.0, 0.0, -1.0, 1.0);
      Actual   : constant CPU_Tensor := Truncate (Tensor);
   begin
      Assert_Equal (Expected, Actual);
   end Test_Function_Truncate;

   procedure Test_Function_Degrees (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((0.0, -0.5 * Pi, 0.5 * Pi, -Pi, Pi, 2.0 * Pi));

      Expected : constant Element_Array := (0.0, -90.0, 90.0, -180.0, 180.0, 360.0);
      Actual   : constant CPU_Tensor := Degrees (Tensor);
   begin
      Assert_Equal (Expected, Actual);
   end Test_Function_Degrees;

   procedure Test_Function_Radians (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((0.0, -90.0, 90.0, -180.0, 180.0, 360.0));

      Expected : constant Element_Array := (0.0, -0.5 * Pi, 0.5 * Pi, -Pi, Pi, 2.0 * Pi);
      Actual   : constant CPU_Tensor := Radians (Tensor);
   begin
      Assert_Equal (Expected, Actual);
   end Test_Function_Radians;

   procedure Test_Function_Min_Tensors (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, -3.0, -2.0, 0.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((0.0, 3.0, 3.0, -3.0, 2.0));

      Expected : constant Element_Array := (0.0, 2.0, -3.0, -3.0, 0.0);
      Actual   : constant CPU_Tensor := Min (Tensor_1, Tensor_2);
   begin
      Assert_Equal (Expected, Actual);
   end Test_Function_Min_Tensors;

   procedure Test_Function_Max_Tensors (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, -3.0, -2.0, 0.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((0.0, 3.0, 3.0, -3.0, 2.0));

      Expected : constant Element_Array := (1.0, 3.0, 3.0, -2.0, 2.0);
      Actual   : constant CPU_Tensor := Max (Tensor_1, Tensor_2);
   begin
      Assert_Equal (Expected, Actual);
   end Test_Function_Max_Tensors;

   procedure Test_Function_Min_Tensor_Element (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((1.0, 2.0, -3.0, -2.0, 0.0));

      Expected_1 : constant Element_Array := (0.0, 0.0, -3.0, -2.0, 0.0);
      Expected_2 : constant Element_Array := (-1.0, -1.0, -3.0, -2.0, -1.0);

      Actual_1 : constant CPU_Tensor := Min (0.0, Tensor);
      Actual_2 : constant CPU_Tensor := Min (Tensor, -1.0);
   begin
      Assert_Equal (Expected_1, Actual_1);
      Assert_Equal (Expected_2, Actual_2);
   end Test_Function_Min_Tensor_Element;

   procedure Test_Function_Max_Tensor_Element (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((1.0, 2.0, -3.0, -2.0, 0.0));

      Expected_1 : constant Element_Array := (1.0, 2.0, 0.0, 0.0, 0.0);
      Expected_2 : constant Element_Array := (1.0, 2.0, -1.0, -1.0, 0.0);

      Actual_1 : constant CPU_Tensor := Max (0.0, Tensor);
      Actual_2 : constant CPU_Tensor := Max (Tensor, -1.0);
   begin
      Assert_Equal (Expected_1, Actual_1);
      Assert_Equal (Expected_2, Actual_2);
   end Test_Function_Max_Tensor_Element;

   procedure Test_Operator_And_Not_Tensors (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Boolean_Tensor ((False, True, False, True));
      Tensor_2 : constant CPU_Tensor := To_Boolean_Tensor ((False, True, True, False));

      Expected : constant CPU_Tensor := To_Boolean_Tensor ((False, False, True, False));
      Actual   : constant CPU_Tensor := And_Not (Tensor_1, Tensor_2);
   begin
      Assert (Expected = Actual, "Unexpected element in tensor");
   end Test_Operator_And_Not_Tensors;

   procedure Test_Operator_And_Tensors (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Boolean_Tensor ((False, True, False, True));
      Tensor_2 : constant CPU_Tensor := To_Boolean_Tensor ((False, True, True, False));

      Expected : constant CPU_Tensor := To_Boolean_Tensor ((False, True, False, False));
      Actual   : constant CPU_Tensor := Tensor_1 and Tensor_2;
   begin
      Assert (Expected = Actual, "Unexpected element in tensor");
   end Test_Operator_And_Tensors;

   procedure Test_Operator_Or_Tensors (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Boolean_Tensor ((False, True, False, True));
      Tensor_2 : constant CPU_Tensor := To_Boolean_Tensor ((False, True, True, False));

      Expected : constant CPU_Tensor := To_Boolean_Tensor ((False, True, True, True));
      Actual   : constant CPU_Tensor := Tensor_1 or Tensor_2;
   begin
      Assert (Expected = Actual, "Unexpected element in tensor");
   end Test_Operator_Or_Tensors;

   procedure Test_Operator_Xor_Tensors (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Boolean_Tensor ((False, True, False, True));
      Tensor_2 : constant CPU_Tensor := To_Boolean_Tensor ((False, True, True, False));

      Expected : constant CPU_Tensor := To_Boolean_Tensor ((False, False, True, True));
      Actual   : constant CPU_Tensor := Tensor_1 xor Tensor_2;
   begin
      Assert (Expected = Actual, "Unexpected element in tensor");
   end Test_Operator_Xor_Tensors;

   procedure Test_Operator_Not_Tensors (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Boolean_Tensor ((False, True, False, True));

      Expected : constant CPU_Tensor := To_Boolean_Tensor ((True, False, True, False));
      Actual   : constant CPU_Tensor := not Tensor;
   begin
      Assert (Expected = Actual, "Unexpected element in tensor");
   end Test_Operator_Not_Tensors;

   procedure Test_Operator_Equals_Tensors (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0));

      Tensor_3 : constant CPU_Tensor := To_Tensor ((0.0, 2.0, 3.0, 4.0, 5.0));
      Tensor_4 : constant CPU_Tensor := To_Tensor ((0.0, 1.0, 3.0, 2.0, 5.0));

      Expected : constant CPU_Tensor := To_Boolean_Tensor ((True, False, True, False, True));
   begin
      Assert (Tensor_1 = Tensor_2, "Tensors not equal");
      Assert (not (Tensor_1 = Tensor_3), "Tensors equal");

      Assert (Expected = (Tensor_3 = Tensor_4), "Tensors not equal for some elements");
   end Test_Operator_Equals_Tensors;

   procedure Test_Operator_Equals_Boolean_Tensors (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Boolean_Tensor ((True, False, True, False, True));
      Tensor_2 : constant CPU_Tensor := To_Boolean_Tensor ((False, False, True, True, True));

      Expected : constant CPU_Tensor := To_Boolean_Tensor ((False, True, True, False, True));
   begin
      Assert (Expected = (Tensor_1 = Tensor_2), "Tensors not equal for some booleans");
   end Test_Operator_Equals_Boolean_Tensors;

   procedure Test_Operator_Equals_Element_Tensor (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0));

      Expected : constant CPU_Tensor := To_Boolean_Tensor ((False, True, False, False, False));
   begin
      Assert (Expected = (2.0 = Tensor), "Tensor not equal to element");
      Assert (Expected = (Tensor = 2.0), "Tensor not equal to element");
   end Test_Operator_Equals_Element_Tensor;

   procedure Test_Operator_Not_Equals_Tensors (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((0.0, 2.0, 3.0, 4.0, 5.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((0.0, 1.0, 3.0, 2.0, 5.0));

      Expected : constant CPU_Tensor := To_Boolean_Tensor ((False, True, False, True, False));
   begin
      Assert (Expected = (Tensor_1 /= Tensor_2), "Tensors equal for some elements");
   end Test_Operator_Not_Equals_Tensors;

   procedure Test_Operator_Not_Equals_Boolean_Tensors (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Boolean_Tensor ((True, False, True, False, True));
      Tensor_2 : constant CPU_Tensor := To_Boolean_Tensor ((False, False, True, True, True));

      Expected : constant CPU_Tensor := To_Boolean_Tensor ((True, False, False, True, False));
   begin
      Assert (Expected = (Tensor_1 /= Tensor_2), "Tensors equal for some booleans");
   end Test_Operator_Not_Equals_Boolean_Tensors;

   procedure Test_Operator_Not_Equals_Element_Tensor (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0));

      Expected : constant CPU_Tensor := To_Boolean_Tensor ((True, False, True, True, True));
   begin
      Assert (Expected = (2.0 /= Tensor), "Tensor equal to element");
      Assert (Expected = (Tensor /= 2.0), "Tensor equal to element");
   end Test_Operator_Not_Equals_Element_Tensor;

   procedure Test_Operator_Less_Than_Tensors (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 4.0, 0.0, -2.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((1.0, 3.0, 3.0, 0.0, -1.0));

      Expected : constant CPU_Tensor := To_Boolean_Tensor ((False, True, False, False, True));
   begin
      Assert (Expected = (Tensor_1 < Tensor_2), "Tensor not less than other for some elements");
   end Test_Operator_Less_Than_Tensors;

   procedure Test_Operator_Less_Than_Element_Tensor (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0));

      Expected_1 : constant CPU_Tensor := To_Boolean_Tensor ((False, False, True, True, True));
      Expected_2 : constant CPU_Tensor := To_Boolean_Tensor ((True, True, True, False, False));
   begin
      Assert (Expected_1 = (2.0 < Tensor), "Element not less than tensor");
      Assert (Expected_2 = (Tensor < 4.0), "Tensor not less than element");
   end Test_Operator_Less_Than_Element_Tensor;

   procedure Test_Operator_Less_Equals_Tensors (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 4.0, 0.0, -2.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((1.0, 3.0, 3.0, 0.0, -1.0));

      Expected : constant CPU_Tensor := To_Boolean_Tensor ((True, True, False, True, True));
   begin
      Assert (Expected = (Tensor_1 <= Tensor_2),
        "Tensor not less than or equal to other for some elements");
   end Test_Operator_Less_Equals_Tensors;

   procedure Test_Operator_Less_Equals_Element_Tensor (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0));

      Expected_1 : constant CPU_Tensor := To_Boolean_Tensor ((False, False, True, True, True));
      Expected_2 : constant CPU_Tensor := To_Boolean_Tensor ((True, True, True, False, False));
   begin
      Assert (Expected_1 = (3.0 <= Tensor), "Element not less than tensor");
      Assert (Expected_2 = (Tensor <= 3.0), "Tensor not less than element");
   end Test_Operator_Less_Equals_Element_Tensor;

   procedure Test_Operator_Greater_Than_Tensors (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 4.0, 0.0, -1.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((1.0, 3.0, 3.0, 0.0, -2.0));

      Expected : constant CPU_Tensor := To_Boolean_Tensor ((False, False, True, False, True));
   begin
      Assert (Expected = (Tensor_1 > Tensor_2), "Tensor not greater than other for some elements");
   end Test_Operator_Greater_Than_Tensors;

   procedure Test_Operator_Greater_Than_Element_Tensor (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0));

      Expected_1 : constant CPU_Tensor := To_Boolean_Tensor ((True, True, True, False, False));
      Expected_2 : constant CPU_Tensor := To_Boolean_Tensor ((False, False, True, True, True));
   begin
      Assert (Expected_1 = (4.0 > Tensor), "Element not greater than tensor");
      Assert (Expected_2 = (Tensor > 2.0), "Tensor not greater than element");
   end Test_Operator_Greater_Than_Element_Tensor;

   procedure Test_Operator_Greater_Equals_Tensors (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 4.0, 0.0, -1.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((1.0, 3.0, 3.0, 0.0, -2.0));

      Expected : constant CPU_Tensor := To_Boolean_Tensor ((True, False, True, True, True));
   begin
      Assert (Expected = (Tensor_1 >= Tensor_2),
        "Tensor not greater than or equal to other for some elements");
   end Test_Operator_Greater_Equals_Tensors;

   procedure Test_Operator_Greater_Equals_Element_Tensor (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0));

      Expected_1 : constant CPU_Tensor := To_Boolean_Tensor ((True, True, True, False, False));
      Expected_2 : constant CPU_Tensor := To_Boolean_Tensor ((False, False, True, True, True));
   begin
      Assert (Expected_1 = (3.0 >= Tensor), "Element not greater than or equal to tensor");
      Assert (Expected_2 = (Tensor >= 3.0), "Tensor not greater than or equal to element");
   end Test_Operator_Greater_Equals_Element_Tensor;

   procedure Test_Any_True (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Boolean_Tensor ((False, False, False));
      Tensor_2 : constant CPU_Tensor := To_Boolean_Tensor ((True, False, False));
      Tensor_3 : constant CPU_Tensor := To_Boolean_Tensor ((True, True, True));

      Tensor_4 : constant CPU_Tensor := To_Boolean_Tensor ((True, True, True, True, True));
      Tensor_5 : constant CPU_Tensor := To_Boolean_Tensor ((True, True, True, True, False));
      Tensor_6 : constant CPU_Tensor := To_Boolean_Tensor ((True, False, False, False, False));
      Tensor_7 : constant CPU_Tensor := To_Boolean_Tensor ((False, False, False, False, False));
   begin
      Assert (not Tensor_1.Any_True, "Some element of tensor is true");
      Assert (Tensor_2.Any_True, "No element of tensor is true");
      Assert (Tensor_3.Any_True, "No element of tensor is true");
      Assert (Tensor_4.Any_True, "No element of tensor is true");
      Assert (Tensor_5.Any_True, "No element of tensor is true");
      Assert (Tensor_6.Any_True, "No element of tensor is true");
      Assert (not Tensor_7.Any_True, "Some element of tensor is true");
   end Test_Any_True;

   procedure Test_All_True (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Boolean_Tensor ((False, False, False));
      Tensor_2 : constant CPU_Tensor := To_Boolean_Tensor ((True, False, False));
      Tensor_3 : constant CPU_Tensor := To_Boolean_Tensor ((True, True, True));

      Tensor_4 : constant CPU_Tensor := To_Boolean_Tensor ((True, True, True, True, True));
      Tensor_5 : constant CPU_Tensor := To_Boolean_Tensor ((True, True, True, True, False));
   begin
      Assert (not Tensor_1.All_True, "All elements of tensor are true");
      Assert (not Tensor_2.All_True, "All elements of tensor are true");
      Assert (Tensor_3.All_True, "Not all elements of tensor are true");
      Assert (Tensor_4.All_True, "Not all elements of tensor are true");
      Assert (not Tensor_5.All_True, "All elements of tensor are true");
   end Test_All_True;

   procedure Test_Reduction_Binary_Operator (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((0.1, 0.2, 0.4, 0.2, 0.5));

      Expression_Sum     : constant CPU_Expression := X + Y;
      Expression_Diff    : constant CPU_Expression := X - Y;
      Expression_Product : constant CPU_Expression := X * Y;
      Expression_Divide  : constant CPU_Expression := X / Y;

      Expected_1 : constant Element := 15.0;
      Expected_2 : constant Element := -15.0;
      Expected_3 : constant Element := 120.0;
      Expected_4 : constant Element := 1250.0;

      Actual_1 : constant Element := Tensor_1.Reduce (Expression_Sum, 0.0);
      Actual_2 : constant Element := Tensor_1.Reduce (Expression_Diff, 0.0);
      Actual_3 : constant Element := Tensor_1.Reduce (Expression_Product, 1.0);
      Actual_4 : constant Element := Tensor_2.Reduce (Expression_Divide, 1.0);
   begin
      Assert (Expected_1 = Actual_1, "Unexpected reduction result: " & Actual_1'Image);
      Assert (Expected_2 = Actual_2, "Unexpected reduction result: " & Actual_2'Image);
      Assert (Expected_3 = Actual_3, "Unexpected reduction result: " & Actual_3'Image);
      Assert (Expected_4 = Actual_4, "Unexpected reduction result: " & Actual_4'Image);
   end Test_Reduction_Binary_Operator;

   procedure Test_Reduction_Associative_Binary_Operator (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((0.1, 0.2, 0.4, 0.2, 0.5));

      Expression_Sum     : constant CPU_Expression := X + Y;
      Expression_Product : constant CPU_Expression := X * Y;

      Expected_1 : constant Element := 15.0;
      Expected_3 : constant Element := 120.0;

      Actual_1 : constant Element := Tensor_1.Reduce (Expression_Sum, 0.0);
      Actual_3 : constant Element := Tensor_1.Reduce (Expression_Product, 1.0);
   begin
      Assert (Expected_1 = Actual_1, "Unexpected reduction result: " & Actual_1'Image);
      Assert (Expected_3 = Actual_3, "Unexpected reduction result: " & Actual_3'Image);
   end Test_Reduction_Associative_Binary_Operator;

   procedure Test_Reduction_Unary_Operator (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((-1.0, -2.0, -3.0, -4.0, -5.0));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((4.0, 9.0, 16.0, 25.0, 36.0));

      Expression_Minus    : constant CPU_Expression := X + (-Y);
      Expression_Absolute : constant CPU_Expression := X + abs Y;
      Expression_Sqrt     : constant CPU_Expression := X + Sqrt (Y);

      Expected_1 : constant Element := 15.0;
      Expected_2 : constant Element := 15.0;
      Expected_3 : constant Element := 20.0;

      Actual_1 : constant Element := Tensor_1.Reduce (Expression_Minus, 0.0);
      Actual_2 : constant Element := Tensor_1.Reduce (Expression_Absolute, 0.0);
      Actual_3 : constant Element := Tensor_2.Reduce (Expression_Sqrt, 0.0);
   begin
      Assert (Expected_1 = Actual_1, "Unexpected reduction result: " & Actual_1'Image);
      Assert (Expected_2 = Actual_2, "Unexpected reduction result: " & Actual_2'Image);
      Assert (Expected_3 = Actual_3, "Unexpected reduction result: " & Actual_3'Image);
   end Test_Reduction_Unary_Operator;

   procedure Test_Reduction_Number (Object : in out Test) is
      Tensor : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0));

      Expression_1 : constant CPU_Expression := X + Y + 1.0;
      Expression_2 : constant CPU_Expression := X + Y - 1.0;
      Expression_3 : constant CPU_Expression := 2.0 * (X + Y) + 1.0;
      Expression_4 : constant CPU_Expression := (X + Y) * 2.0 + 2.0;
      Expression_5 : constant CPU_Expression := 1.0 + X + Y;
      Expression_6 : constant CPU_Expression := 1.0 - X + Y;
      Expression_7 : constant CPU_Expression := X + Y / 2.0;
      Expression_8 : constant CPU_Expression := X + 18.0 / Y;
      Expression_9 : constant CPU_Expression := X + Max (1.0, Min (Y, 2.0));
      Expression_0 : constant CPU_Expression := X + Max (Min (2.0, Y), 1.0);

      Expected_1 : constant Element := 20.0;
      Expected_2 : constant Element := 11.0;
      Expected_3 : constant Element := 145.0;
      Expected_4 : constant Element := 240.0;
      Expected_5 : constant Element := 20.0;
      Expected_6 : constant Element := 3.0;
      Expected_7 : constant Element := 8.0;
      Expected_8 : constant Element := 42.0;
      Expected_9 : constant Element := 9.0;
      Expected_0 : constant Element := 10.0;

      Actual_1 : constant Element := Tensor.Reduce (Expression_1, 0.0);
      Actual_2 : constant Element := Tensor.Reduce (Expression_2, 1.0);
      Actual_3 : constant Element := Tensor.Reduce (Expression_3, 0.0);
      Actual_4 : constant Element := Tensor.Reduce (Expression_4, 2.0);
      Actual_5 : constant Element := Tensor.Reduce (Expression_5, 0.0);
      Actual_6 : constant Element := Tensor.Reduce (Expression_6, 1.0);
      Actual_7 : constant Element := Tensor.Reduce (Expression_7, 0.5);
      Actual_8 : constant Element := Tensor.Reduce (Expression_8, 0.9);
      Actual_9 : constant Element := Tensor.Reduce (Expression_9, 0.0);
      Actual_0 : constant Element := Tensor.Reduce (Expression_0, 1.0);
   begin
      Assert (Expected_1 = Actual_1, "Unexpected reduction result: " & Actual_1'Image);
      Assert (Expected_2 = Actual_2, "Unexpected reduction result: " & Actual_2'Image);
      Assert (Expected_3 = Actual_3, "Unexpected reduction result: " & Actual_3'Image);
      Assert (Expected_4 = Actual_4, "Unexpected reduction result: " & Actual_4'Image);
      Assert (Expected_5 = Actual_5, "Unexpected reduction result: " & Actual_5'Image);
      Assert (Expected_6 = Actual_6, "Unexpected reduction result: " & Actual_6'Image);
      Assert (Expected_7 = Actual_7, "Unexpected reduction result: " & Actual_7'Image);
      Assert (Expected_8 = Actual_8, "Unexpected reduction result: " & Actual_8'Image);
      Assert (Expected_9 = Actual_9, "Unexpected reduction result: " & Actual_9'Image);
      Assert (Expected_0 = Actual_0, "Unexpected reduction result: " & Actual_0'Image);
   end Test_Reduction_Number;

   procedure Test_Reduction_Sum (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0));
      Tensor_2 : constant CPU_Tensor := Linear_Space (1.0, 100.0, Count => 100);

      Expected_1 : constant Element := 15.0;
      Expected_2 : constant Element := 5050.0;

      Actual_1   : constant Element := Tensor_1.Sum;
      Actual_2   : constant Element := Tensor_2.Sum;
   begin
      Assert (Expected_1 = Actual_1, "Unexpected reduction result: " & Actual_1'Image);
      Assert (Expected_2 = Actual_2, "Unexpected reduction result: " & Actual_2'Image);
   end Test_Reduction_Sum;

   procedure Test_Reduction_Product (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0));
      Tensor_2 : constant CPU_Tensor := Linear_Space (1.0, 10.0, Count => 10);

      Expected_1 : constant Element := 120.0;
      Expected_2 : constant Element := 3628800.0;

      Actual_1   : constant Element := Tensor_1.Product;
      Actual_2   : constant Element := Tensor_2.Product;
   begin
      Assert (Expected_1 = Actual_1, "Unexpected reduction result: " & Actual_1'Image);
      Assert (Expected_2 = Actual_2, "Unexpected reduction result: " & Actual_2'Image);
   end Test_Reduction_Product;

   procedure Test_Function_Min (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0));
      Tensor_2 : constant CPU_Tensor := Linear_Space (-10.0, 10.0, Count => 10);

      Expected_1 : constant Element := 1.0;
      Expected_2 : constant Element := -10.0;

      Actual_1   : constant Element := Tensor_1.Min;
      Actual_2   : constant Element := Tensor_2.Min;
   begin
      Assert (Expected_1 = Actual_1, "Unexpected reduction result: " & Actual_1'Image);
      Assert (Expected_2 = Actual_2, "Unexpected reduction result: " & Actual_2'Image);
   end Test_Function_Min;

   procedure Test_Function_Max (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0));
      Tensor_2 : constant CPU_Tensor := Linear_Space (-10.0, 10.0, Count => 10);

      Expected_1 : constant Element := 5.0;
      Expected_2 : constant Element := 10.0;

      Actual_1   : constant Element := Tensor_1.Max;
      Actual_2   : constant Element := Tensor_2.Max;
   begin
      Assert (Expected_1, Actual_1, "Unexpected reduction result: " & Actual_1'Image);
      Assert (Expected_2, Actual_2, "Unexpected reduction result: " & Actual_2'Image);
   end Test_Function_Max;

   procedure Test_Function_Quantile (Object : in out Test) is
   begin
      Assert (False, "FIXME");
   end Test_Function_Quantile;

   procedure Test_Function_Median (Object : in out Test) is
   begin
      Assert (False, "FIXME");
   end Test_Function_Median;

   procedure Test_Function_Mean (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0, 6.0));
      Tensor_2 : constant CPU_Tensor := Array_Range (1.0, 100.0);

      Expected_1 : constant Element := 3.5;
      Expected_2 : constant Element := 50.0;

      Actual_1   : constant Element := Tensor_1.Mean;
      Actual_2   : constant Element := Tensor_2.Mean;
   begin
      Assert (Expected_1 = Actual_1, "Unexpected mean: " & Actual_1'Image);
      Assert (Expected_2 = Actual_2, "Unexpected mean: " & Actual_2'Image);
   end Test_Function_Mean;

   procedure Test_Function_Variance (Object : in out Test) is
      Tensor_1 : constant CPU_Tensor := To_Tensor ((1 .. 4 => 1.0e7));
      Tensor_2 : constant CPU_Tensor := To_Tensor ((4.0, 7.0, 13.0, 16.0));

      Tensor : constant CPU_Tensor := Tensor_1 + Tensor_2;

      Actual_1   : constant Element := Tensor.Variance;
      Actual_2   : constant Element := Tensor.Variance (Offset => 1);

      Expected_1 : constant Element := 22.5;
      Expected_2 : constant Element := 30.0;
   begin
      Assert (Expected_1 = Actual_1, "Unexpected variance: " & Actual_1'Image);
      Assert (Expected_2 = Actual_2, "Unexpected variance: " & Actual_2'Image);
   end Test_Function_Variance;

   ----------------------------------------------------------------------------

   procedure Test_Random_Uniform (Object : in out Test) is
      Expected_Mean     : constant := 0.5;
      Expected_Variance : constant := 1.0 / 12.0;

      Tensor : constant CPU_Tensor := Random.Uniform ((1 => 100_000));
   begin
      Assert (abs (Tensor.Mean - Expected_Mean) <= 0.01,
        "Unexpected mean for Uniform: " & Tensor.Mean'Image
           & " instead of " & Element'Image (Expected_Mean));

      Assert (abs (Tensor.Variance - Expected_Variance) <= 0.1,
        "Unexpected variance for Uniform: " & Tensor.Variance'Image
           & " instead of " & Element'Image (Expected_Variance));
   end Test_Random_Uniform;

   procedure Test_Random_Normal (Object : in out Test) is
      Expected_Mean     : constant := 0.0;
      Expected_Variance : constant := 1.0;

      Tensor : constant CPU_Tensor := Random.Normal ((1 => 100_000));
   begin
      Assert (abs (Tensor.Mean - Expected_Mean) <= 0.01,
        "Unexpected mean for Normal: " & Tensor.Mean'Image
           & " instead of " & Element'Image (Expected_Mean));

      Assert (abs (Tensor.Variance - Expected_Variance) <= 0.1,
        "Unexpected variance for Normal: " & Tensor.Variance'Image
           & " instead of " & Element'Image (Expected_Variance));
   end Test_Random_Normal;

   procedure Test_Random_Binomial (Object : in out Test) is
      Trials : constant := 20_000;

      --  This example is from numpy.random.binomial's documentation:
      --  Repeat some experiment 9 times with a success probability of 0.1 each.
      --  What is the probability that all 9 experiments fail? Result is ~ 0.38 or 38 %
      Tensor : constant CPU_Tensor := Random.Binomial ((1 => Trials), N => 9, P => 0.1);
      Result : constant Element    := CPU_Tensor'(1.0 and (Tensor = 0.0)).Sum / Element (Trials);
   begin
      Assert (Result in 0.37 .. 0.40, "Unexpected result binomial trials: " & Result'Image);
   end Test_Random_Binomial;

   procedure Test_Random_Geometric (Object : in out Test) is
      P : constant := 0.5;

      Expected_Mean     : constant := (1.0 - P) / P;
      Expected_Variance : constant := (1.0 - P) / P**2;

      Tensor : constant CPU_Tensor := Random.Geometric ((1 => 100_000), P => P);
   begin
      Assert (abs (Tensor.Mean - Expected_Mean) <= 0.01,
        "Unexpected mean for Geometric: " & Tensor.Mean'Image
           & " instead of " & Element'Image (Expected_Mean));

      Assert (abs (Tensor.Variance - Expected_Variance) <= 0.1,
        "Unexpected variance for Geometric: " & Tensor.Variance'Image
           & " instead of " & Element'Image (Expected_Variance));
   end Test_Random_Geometric;

   procedure Test_Random_Exponential (Object : in out Test) is
      Lambda : constant := 1.5;

      Expected_Mean     : constant := 1.0 / Lambda;
      Expected_Variance : constant := 1.0 / Lambda**2;

      Tensor : constant CPU_Tensor := Random.Exponential ((1 => 100_000), Lambda => Lambda);
   begin
      Assert (abs (Tensor.Mean - Expected_Mean) <= 0.01,
        "Unexpected mean for Exponential: " & Tensor.Mean'Image
           & " instead of " & Element'Image (Expected_Mean));

      Assert (abs (Tensor.Variance - Expected_Variance) <= 0.1,
        "Unexpected variance for Exponential: " & Tensor.Variance'Image
           & " instead of " & Element'Image (Expected_Variance));
   end Test_Random_Exponential;

   procedure Test_Random_Pareto (Object : in out Test) is
      Xm : constant := 1.0;
      A  : constant := 4.0;
      --  The variance test below fails with a chance of 10.0 % for A = 3.0 and 0.5 % for A = 3.5

      Expected_Mean     : constant := (A * Xm) / (A - 1.0);
      Expected_Variance : constant := (Xm**2 * A) / ((A - 1.0)**2 * (A - 2.0));

      Tensor : constant CPU_Tensor := Random.Pareto ((1 => 100_000), Xm => Xm, Alpha => A);
   begin
      --  A > 1.0 otherwise mean is infinite
      Assert (abs (Tensor.Mean - Expected_Mean) <= 0.01,
        "Unexpected mean for Pareto: " & Tensor.Mean'Image
           & " instead of " & Element'Image (Expected_Mean));

      --  A > 2.0 otherwise variance is infinite
      Assert (abs (Tensor.Variance - Expected_Variance) <= 0.1,
        "Unexpected variance for Pareto: " & Tensor.Variance'Image
           & " instead of " & Element'Image (Expected_Variance));
   end Test_Random_Pareto;

   procedure Test_Random_Laplace (Object : in out Test) is
      Mean : constant := 2.5;
      B    : constant := 0.5;

      Expected_Mean     : constant := Mean;
      Expected_Variance : constant := 2.0 * B**2;

      Tensor : constant CPU_Tensor := Random.Laplace ((1 => 100_000), Mean => Mean, B => B);
   begin
      Assert (abs (Tensor.Mean - Expected_Mean) <= 0.01,
        "Unexpected mean for Laplace: " & Tensor.Mean'Image
           & " instead of " & Element'Image (Expected_Mean));

      Assert (abs (Tensor.Variance - Expected_Variance) <= 0.1,
        "Unexpected variance for Laplace: " & Tensor.Variance'Image
           & " instead of " & Element'Image (Expected_Variance));
   end Test_Random_Laplace;

   procedure Test_Random_Rayleigh (Object : in out Test) is
      use EF;

      Sigma : constant := 1.5;

      Expected_Mean     : constant Element := Sigma * Sqrt (Pi / 2.0);
      Expected_Variance : constant := (4.0 - Pi) / 2.0 * Sigma**2;

      Tensor : constant CPU_Tensor := Random.Rayleigh ((1 => 100_000), Sigma => Sigma);
   begin
      Assert (abs (Tensor.Mean - Expected_Mean) <= 0.01,
        "Unexpected mean for Rayleigh: " & Tensor.Mean'Image
           & " instead of " & Element'Image (Expected_Mean));

      Assert (abs (Tensor.Variance - Expected_Variance) <= 0.1,
        "Unexpected variance for Rayleigh: " & Tensor.Variance'Image
           & " instead of " & Element'Image (Expected_Variance));
   end Test_Random_Rayleigh;

   procedure Test_Random_Weibull (Object : in out Test) is
      K      : constant := 1.5;
      Lambda : constant := 1.0;

      use EF;

      G1 : constant := 1.0 / 12.0;
      G2 : constant := 1.0 / 1440.0;
      G3 : constant := 239.0 / 362880.0;

      function Gamma (X : Element) return Element is
        (Sqrt (2.0 * Pi / X) * (1.0 / Ada.Numerics.e * (X + G1 / X + G2 / X**3 + G3 / X**5))**X);
      --  Nemes's approximation [1] of the gamma function
      --  See https://en.wikipedia.org/wiki/Stirling%27s_approximation
      --
      --  [1] "New asymptotic expansion for the Gamma(x) function", Nemes, G., 2007

      Expected_Mean     : constant Element := Lambda * Gamma (1.0 + 1.0 / K);
      Expected_Variance : constant Element :=
        Lambda**2 * (Gamma (1.0 + 2.0 / K) - Gamma (1.0 + 1.0 / K)**2);

      Tensor : constant CPU_Tensor := Random.Weibull ((1 => 100_000), K => K, Lambda => Lambda);
   begin
      Assert (abs (Tensor.Mean - Expected_Mean) <= 0.1,
        "Unexpected mean for Weibull: " & Tensor.Mean'Image
           & " instead of " & Element'Image (Expected_Mean));

      Assert (abs (Tensor.Variance - Expected_Variance) <= 0.2,
        "Unexpected variance for Weibull: " & Tensor.Variance'Image
           & " instead of " & Element'Image (Expected_Variance));
   end Test_Random_Weibull;

   procedure Test_Random_Poisson (Object : in out Test) is
      Lambda : constant := 10.0;

      Expected_Mean     : constant Element := Lambda;
      Expected_Variance : constant Element := Lambda;

      Tensor : constant CPU_Tensor := Random.Poisson ((1 => 100_000), Lambda => Lambda);
   begin
      Assert (abs (Tensor.Mean - Expected_Mean) <= 0.03,
        "Unexpected mean for Poisson: " & Tensor.Mean'Image
           & " instead of " & Element'Image (Expected_Mean));

      Assert (abs (Tensor.Variance - Expected_Variance) <= 0.2,
        "Unexpected variance for Poisson: " & Tensor.Variance'Image
           & " instead of " & Element'Image (Expected_Variance));
   end Test_Random_Poisson;

   procedure Test_Random_Gamma (Object : in out Test) is
      K     : constant := 5.0;
      Theta : constant := 2.0;

      Expected_Mean     : constant Element := K * Theta;
      Expected_Variance : constant Element := K * Theta ** 2;

      Tensor : constant CPU_Tensor := Random.Gamma ((1 => 100_000), K => K, Theta => Theta);
   begin
      Assert (abs (Tensor.Mean - Expected_Mean) <= 0.03,
        "Unexpected mean for gamma: " & Tensor.Mean'Image
           & " instead of " & Element'Image (Expected_Mean));

      Assert (abs (Tensor.Variance - Expected_Variance) <= 0.5,
        "Unexpected variance for gamma: " & Tensor.Variance'Image
           & " instead of " & Element'Image (Expected_Variance));
   end Test_Random_Gamma;

   procedure Test_Random_Beta (Object : in out Test) is
      Alpha : constant := 2.0;
      Beta  : constant := 5.0;

      Expected_Mean     : constant Element := Alpha / (Alpha + Beta);
      Expected_Variance : constant Element :=
        (Alpha * Beta) / ((Alpha + Beta)**2 * (Alpha + Beta + 1.0));

      Tensor : constant CPU_Tensor := Random.Beta ((1 => 100_000), Alpha => Alpha, Beta => Beta);
   begin
      Assert (abs (Tensor.Mean - Expected_Mean) <= 0.01,
        "Unexpected mean for Beta: " & Tensor.Mean'Image
           & " instead of " & Element'Image (Expected_Mean));

      Assert (abs (Tensor.Variance - Expected_Variance) <= 0.1,
        "Unexpected variance for beta: " & Tensor.Variance'Image
           & " instead of " & Element'Image (Expected_Variance));
   end Test_Random_Beta;

   procedure Test_Random_Chi_Squared (Object : in out Test) is
      K : constant := 4;

      Expected_Mean     : constant Element := Element (K);
      Expected_Variance : constant Element := Element (K * 2);

      Tensor : constant CPU_Tensor := Random.Chi_Squared ((1 => 100_000), K => K);
   begin
      Assert (abs (Tensor.Mean - Expected_Mean) <= 0.03,
        "Unexpected mean for Chi_Squared: " & Tensor.Mean'Image
           & " instead of " & Element'Image (Expected_Mean));

      Assert (abs (Tensor.Variance - Expected_Variance) <= 0.5,
        "Unexpected variance for Chi_Squared: " & Tensor.Variance'Image
           & " instead of " & Element'Image (Expected_Variance));
   end Test_Random_Chi_Squared;

   procedure Test_Random_Student_T (Object : in out Test) is
      Trials    : constant := 100_000;
      True_Mean : constant := 7725.0;

      --  This example is from numpy.random.standard_t's documentation
      Data : constant CPU_Tensor := To_Tensor
        ((5260.0, 5470.0, 5640.0, 6180.0, 6390.0, 6515.0, 6805.0, 7515.0, 7515.0, 8230.0, 8770.0));

      T : constant Element := Random.Test_Statistic_T_Test (Data, True_Mean);

      --  The null hypothesis must be rejected if the distribution > positive T
      --  or < negative T
      Tensor : constant CPU_Tensor := Random.Student_T ((1 => Trials), V => Data.Elements - 1);
      Result : constant Element    := Sum (1.0 and (Tensor >= abs T)) / Element (Trials);
   begin
      Assert (Result in 0.008 .. 0.010, "Unexpected result Student's t trials: " & Result'Image);
   end Test_Random_Student_T;

   ----------------------------------------------------------------------------

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(Tensors - " & Suite_Name & " - Vectors) ";
   begin
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Zeros", Test_Zeros'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Ones", Test_Ones'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function To_Tensor", Test_To_Tensor'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Linear_Space", Test_Linear_Space'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Log_Space", Test_Log_Space'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Geometric_Space", Test_Geometric_Space'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Array_Range", Test_Array_Range'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Test indexing using index", Test_Constant_Indexing_Index'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test indexing using index (boolean)",
         Test_Constant_Indexing_Index_Boolean'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test indexing using range", Test_Constant_Indexing_Range'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test indexing using tensor", Test_Constant_Indexing_Tensor'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Test set value using index", Test_Set_Value_Index'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test set value using index (boolean)", Test_Set_Value_Index_Boolean'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '&' operator", Test_Operator_Concatenate'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '*' operator (inner product)", Test_Operator_Multiply_Inner'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Norm", Test_Norm'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Normalize", Test_Normalize'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Standardize", Test_Standardize'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Correlation_Coefficient", Test_Correlation_Coefficient'Access));

      --  Element-wise operations
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '+' operator (tensors)", Test_Operator_Add_Tensors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '+' operator (element, tensor)", Test_Operator_Add_Element_Tensor'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '-' operator (tensors)", Test_Operator_Subtract_Tensors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '-' operator (element, tensor)",
         Test_Operator_Subtract_Element_Tensor'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '**' operator (tensors)", Test_Operator_Power_Tensors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '**' operator (element, tensor)",
         Test_Operator_Power_Element_Tensor'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '*' operator", Test_Operator_Multiply_Element_Tensor'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Multiply", Test_Function_Multiply_Tensors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Power", Test_Function_Power_Tensor_Element'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '/' operator (tensors)", Test_Operator_Divide_Tensors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '/' operator (element, tensor)",
         Test_Operator_Divide_Element_Tensor'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Divide_Or_Zero", Test_Divide_Or_Zero'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test 'mod' operator (tensors)", Test_Operator_Mod_Tensors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test 'mod' operator (element, tensor)", Test_Operator_Mod_Element_Tensor'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test 'rem' operator (tensors)", Test_Operator_Rem_Tensors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test 'rem' operator (element, tensor)", Test_Operator_Rem_Element_Tensor'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test 'abs' operator", Test_Operator_Abs'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Sqrt", Test_Function_Sqrt'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Ceil", Test_Function_Ceil'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Floor", Test_Function_Floor'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Round", Test_Function_Round'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Truncate", Test_Function_Truncate'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Min", Test_Function_Min_Tensors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Max", Test_Function_Max_Tensors'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Min (element, tensor)", Test_Function_Min_Tensor_Element'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Max (element, tensor)", Test_Function_Max_Tensor_Element'Access));

      --  TODO Exp, Log, Log10, Log2
      --  TODO Trigonometry: Sin, Cos, Tan, Arcsin, Arccos, Arctan

      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Degrees", Test_Function_Degrees'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Radians", Test_Function_Radians'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Min", Test_Function_Min'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Max", Test_Function_Max'Access));

      --  Statistics
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Quantile", Test_Function_Quantile'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Median", Test_Function_Median'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Mean", Test_Function_Mean'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Variance", Test_Function_Variance'Access));

      --  Logical
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function And_Not (tensors)", Test_Operator_And_Not_Tensors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test 'and' operator (tensors)", Test_Operator_And_Tensors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test 'or' operator (tensors)", Test_Operator_Or_Tensors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test 'xor' operator (tensors)", Test_Operator_Xor_Tensors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test 'not' operator (tensor)", Test_Operator_Not_Tensors'Access));

      --  Comparisons
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '=' operator (tensors)", Test_Operator_Equals_Tensors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '=' operator (boolean tensors)",
         Test_Operator_Equals_Boolean_Tensors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '=' operator (element, tensor)",
         Test_Operator_Equals_Element_Tensor'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '/=' operator (tensors)", Test_Operator_Not_Equals_Tensors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '/=' operator (boolean tensors)",
         Test_Operator_Not_Equals_Boolean_Tensors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '/=' operator (element, tensor)",
         Test_Operator_Not_Equals_Element_Tensor'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '<' operator (tensors)", Test_Operator_Less_Than_Tensors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '<' operator (element, tensor)",
         Test_Operator_Less_Than_Element_Tensor'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '<=' operator (tensors)", Test_Operator_Less_Equals_Tensors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '<=' operator (element, tensor)",
         Test_Operator_Less_Equals_Element_Tensor'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '>' operator (tensors)", Test_Operator_Greater_Than_Tensors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '>' operator (element, tensor)",
         Test_Operator_Greater_Than_Element_Tensor'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '>=' operator (tensors)", Test_Operator_Greater_Equals_Tensors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '>=' operator (element, tensor)",
         Test_Operator_Greater_Equals_Element_Tensor'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Any_True", Test_Any_True'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function All_True", Test_All_True'Access));

      --  Expressions
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test reduction binary operator", Test_Reduction_Binary_Operator'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test associative reduction binary operator",
         Test_Reduction_Associative_Binary_Operator'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test reduction unary operator", Test_Reduction_Unary_Operator'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test reduction number", Test_Reduction_Number'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Test reduction in function Sum", Test_Reduction_Sum'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test reduction in function Product", Test_Reduction_Product'Access));

      --  Random
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test random uniform distribution", Test_Random_Uniform'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test random normal distribution", Test_Random_Normal'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test random binomial distribution", Test_Random_Binomial'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test random geometric distribution", Test_Random_Geometric'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test random exponential distribution", Test_Random_Exponential'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test random Pareto distribution", Test_Random_Pareto'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test random Laplace distribution", Test_Random_Laplace'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test random Rayleigh distribution", Test_Random_Rayleigh'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test random Weibull distribution", Test_Random_Weibull'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test random Poisson distribution", Test_Random_Poisson'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test random gamma distribution", Test_Random_Gamma'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test random beta distribution", Test_Random_Beta'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test random chi-squared distribution", Test_Random_Chi_Squared'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test random Student's t-distribution", Test_Random_Student_T'Access));

      return Test_Suite'Access;
   end Suite;

begin
   Reset_Random (Orka.OS.Monotonic_Clock);
end Generic_Test_Tensors_Vectors;
