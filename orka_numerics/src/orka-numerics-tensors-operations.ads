--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2023 onox <denkpadje@gmail.com>
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

private generic
   type Tensor_Type (<>) is new Tensor with private;

   with procedure Make_Upper_Triangular (Object : in out Tensor_Type; Offset : Integer := 0);

   with procedure Scale_Row (Ab : in out Tensor_Type; I : Index_Type; Scale : Element);
   with procedure Swap_Rows (Ab : in out Tensor_Type; I, J : Index_Type);

   with procedure Forward_Substitute (Ab : in out Tensor_Type; Index, Pivot_Index : Index_Type);
   with procedure Back_Substitute    (Ab : in out Tensor_Type; Index, Pivot_Index : Index_Type);

   type Expression_Type (<>) is new Expression with private;

   type QR_Factorization_Type (<>) is new QR_Factorization with private;

   with function Create_QR_Factorization
     (Q, R         : Tensor_Type;
      Determinancy : Matrix_Determinancy) return QR_Factorization_Type;

   with function Q (Object : QR_Factorization_Type'Class) return Tensor_Type;
   with function R (Object : QR_Factorization_Type'Class) return Tensor_Type;
package Orka.Numerics.Tensors.Operations is
   pragma Preelaborate;

   overriding function Get (Object : Tensor_Type; Index : Index_Type) return Element;
   overriding function Get (Object : Tensor_Type; Index : Index_Type) return Boolean;

   overriding function Get (Object : Tensor_Type; Index : Range_Type) return Tensor_Type;

   overriding procedure Set
     (Object : in out Tensor_Type;
      Index  : Index_Type;
      Value  : Tensor_Type);

   overriding procedure Set
     (Object : in out Tensor_Type;
      Index  : Range_Type;
      Value  : Tensor_Type);

   overriding procedure Set (Object : in out Tensor_Type; Index : Index_Type; Value : Element);
   overriding procedure Set (Object : in out Tensor_Type; Index : Index_Type; Value : Boolean);

   ----------------------------------------------------------------------------
   --                              Constructors                              --
   ----------------------------------------------------------------------------

   overriding function Zeros (Shape : Tensor_Shape) return Tensor_Type;
   overriding function Zeros (Elements : Positive)  return Tensor_Type;

   overriding function Ones (Shape : Tensor_Shape) return Tensor_Type;
   overriding function Ones (Elements : Positive)  return Tensor_Type;

   overriding
   function To_Tensor (Elements : Element_Array) return Tensor_Type;

   overriding
   function To_Boolean_Tensor (Booleans : Boolean_Array) return Tensor_Type;

   overriding
   function Geometric_Space
     (Start, Stop : Element;
      Count       : Positive;
      Interval    : Interval_Kind := Closed;
      Base        : Element := 10.0) return Tensor_Type;

   overriding
   function Array_Range (Start, Stop : Element; Step : Element := 1.0) return Tensor_Type;

   overriding
   function Array_Range (Stop : Element) return Tensor_Type;

   overriding
   function Identity (Size : Positive; Offset : Integer := 0) return Tensor_Type;

   ----------------------------------------------------------------------------

   overriding
   function Upper_Triangular (Object : Tensor_Type; Offset : Integer := 0) return Tensor_Type;

   overriding
   function Trace (Object : Tensor_Type; Offset : Integer := 0) return Element;

   overriding
   function Flatten (Object : Tensor_Type) return Tensor_Type;

   overriding
   function Reshape (Object : Tensor_Type; Elements : Positive) return Tensor_Type;

   overriding
   function "&" (Left, Right : Tensor_Type) return Tensor_Type;

   ----------------------------------------------------------------------------
   --                            Matrix operations                           --
   ----------------------------------------------------------------------------

   overriding function "**" (Left : Tensor_Type; Right : Integer) return Tensor_Type;

   overriding
   function Inverse (Object : Tensor_Type) return Tensor_Type;

   overriding
   function Solve (A, B : Tensor_Type; Solution : out Solution_Kind) return Tensor_Type;

   overriding
   function QR (Object : Tensor_Type) return Tensor_Type;

   overriding
   function QR (Object : Tensor_Type; Mode : QR_Mode := Reduced) return QR_Factorization'Class;

   overriding
   function QR_For_Least_Squares (Object : Tensor_Type) return QR_Factorization'Class;

   overriding
   function Least_Squares (Object : QR_Factorization'Class; B : Tensor_Type) return Tensor_Type;

   overriding
   function Least_Squares (A, B : Tensor_Type) return Tensor_Type;

   overriding
   function Constrained_Least_Squares (A, B, C, D : Tensor_Type) return Tensor_Type;

   overriding
   function Cholesky (Object : Tensor_Type; Form : Triangular_Form := Lower) return Tensor_Type;

   overriding
   function Cholesky_Update
     (R, V : Tensor_Type;
      Mode : Update_Mode) return Tensor_Type;

   overriding
   function Solve (A, B : Tensor_Type; Form : Triangular_Form) return Tensor_Type;

   overriding
   function Divide_By (B, A : Tensor_Type) return Tensor_Type;

   overriding
   function Divide_By (B, A : Tensor_Type; Form : Triangular_Form) return Tensor_Type;

   ----------------------------------------------------------------------------
   --                            Vector operations                           --
   ----------------------------------------------------------------------------

   overriding
   function Norm (Object : Tensor_Type) return Element;

   overriding
   function Normalize (Object : Tensor_Type) return Tensor_Type;

   overriding
   function Standardize (Object : Tensor_Type) return Tensor_Type;

   overriding
   function Correlation_Coefficient (Left, Right : Tensor_Type) return Correlation_Element;

   ----------------------------------------------------------------------------
   --                         Element-wise operations                        --
   ----------------------------------------------------------------------------

   overriding function "*" (Left : Element; Right : Tensor_Type) return Tensor_Type;

   overriding function "+" (Left : Element; Right : Tensor_Type) return Tensor_Type;

   overriding function "-" (Left : Tensor_Type; Right : Element) return Tensor_Type;

   overriding function "mod" (Left, Right : Tensor_Type) return Tensor_Type;

   overriding function "rem" (Left, Right : Tensor_Type) return Tensor_Type;

   overriding function "mod" (Left : Tensor_Type; Right : Element) return Tensor_Type;

   overriding function "rem" (Left : Tensor_Type; Right : Element) return Tensor_Type;

   overriding function Power (Left : Tensor_Type; Right : Integer) return Tensor_Type;

   overriding function Min (Left : Element; Right : Tensor_Type) return Tensor_Type;

   overriding function Max (Left : Element; Right : Tensor_Type) return Tensor_Type;

   overriding function Log10 (Object : Tensor_Type) return Tensor_Type;

   overriding function Log2 (Object : Tensor_Type) return Tensor_Type;

   overriding function Degrees (Object : Tensor_Type) return Tensor_Type;

   overriding function Radians (Object : Tensor_Type) return Tensor_Type;

   overriding function Sum (Object : Tensor_Type) return Element;

   overriding function Product (Object : Tensor_Type) return Element;

   overriding
   function Sum (Object : Tensor_Type; Axis : Tensor_Axis) return Tensor_Type;

   overriding
   function Product (Object : Tensor_Type; Axis : Tensor_Axis) return Tensor_Type;

   ----------------------------------------------------------------------------
   --                               Statistics                               --
   ----------------------------------------------------------------------------

   overriding function Min (Object : Tensor_Type) return Element;

   overriding function Max (Object : Tensor_Type) return Element;

   overriding function Min (Object : Tensor_Type; Axis : Tensor_Axis) return Tensor_Type;

   overriding function Max (Object : Tensor_Type; Axis : Tensor_Axis) return Tensor_Type;

   overriding function Quantile (Object : Tensor_Type; P : Probability) return Element;

   overriding function Median (Object : Tensor_Type) return Element;

   overriding function Mean (Object : Tensor_Type) return Element;

   overriding
   function Variance (Object : Tensor_Type; Offset : Natural := 0) return Element;

   overriding
   function Standard_Deviation (Object : Tensor_Type; Offset : Natural := 0) return Element;

   ----------------------------------------------------------------------------

   overriding
   function Mean (Object : Tensor_Type; Axis : Tensor_Axis) return Tensor_Type;

   overriding
   function Variance
     (Object : Tensor_Type;
      Axis   : Tensor_Axis;
      Offset : Natural := 0) return Tensor_Type;

   overriding
   function Median (Object : Tensor_Type; Axis : Tensor_Axis) return Tensor_Type;

   overriding
   function Standard_Deviation
     (Object : Tensor_Type;
      Axis   : Tensor_Axis;
      Offset : Natural := 0) return Tensor_Type;

   ----------------------------------------------------------------------------
   --                              Comparisons                               --
   ----------------------------------------------------------------------------

   overriding function "="  (Left : Tensor_Type; Right : Element) return Tensor_Type;

   overriding function "/=" (Left : Tensor_Type; Right : Element) return Tensor_Type;

   ----------------------------------------------------------------------------

   overriding function "="  (Left : Element; Right : Tensor_Type) return Tensor_Type;

   overriding function "/=" (Left : Element; Right : Tensor_Type) return Tensor_Type;

   overriding function ">"  (Left : Element; Right : Tensor_Type) return Tensor_Type;

   overriding function "<"  (Left : Element; Right : Tensor_Type) return Tensor_Type;

   overriding function ">=" (Left : Element; Right : Tensor_Type) return Tensor_Type;

   overriding function "<=" (Left : Element; Right : Tensor_Type) return Tensor_Type;

   ----------------------------------------------------------------------------

   overriding function And_Not (Left, Right : Tensor_Type) return Tensor_Type;

   overriding function "=" (Left, Right : Tensor_Type) return Boolean;

   overriding
   function All_Close
     (Left, Right        : Tensor_Type;
      Relative_Tolerance : Element := 1.0e-05;
      Absolute_Tolerance : Element := Element_Type'Model_Epsilon) return Boolean;

end Orka.Numerics.Tensors.Operations;
