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
   type Expression_Type (<>) is new Expression with private;

   type Real_Tensor_Type (<>) is new Tensor_Type and Real_Tensor with private;

   with procedure Make_Upper_Triangular (Object : in out Real_Tensor_Type; Offset : Integer := 0);
   with procedure Swap_Rows (Ab : in out Real_Tensor_Type; I, J : Index_Type);
   with procedure Forward_Substitute (Ab : in out Real_Tensor_Type; Index, Pivot_Index : Index_Type);
   with procedure Back_Substitute    (Ab : in out Real_Tensor_Type; Index, Pivot_Index : Index_Type);

   type QR_Factorization_Type (<>) is new QR_Factorization with private;

   with function Create_QR_Factorization
     (Q, R         : Real_Tensor_Type;
      Determinancy : Matrix_Determinancy) return QR_Factorization_Type;

   with function Q (Object : QR_Factorization_Type'Class) return Real_Tensor_Type;
   with function R (Object : QR_Factorization_Type'Class) return Real_Tensor_Type;

   with function "-" (Value : Element) return Element is <>;
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
   function Array_Range (Start, Stop : Element) return Tensor_Type;

   overriding
   function Array_Range (Stop : Element) return Tensor_Type;

   overriding
   function Flatten (Object : Tensor_Type) return Tensor_Type;

   overriding
   function Reshape (Object : Tensor_Type; Elements : Positive) return Tensor_Type;

   overriding
   function "&" (Left, Right : Tensor_Type) return Tensor_Type;

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

   ----------------------------------------------------------------------------
   --                        Floating-point operations                       --
   ----------------------------------------------------------------------------

   overriding
   function Geometric_Space
     (Start, Stop : Real_Element;
      Count       : Positive;
      Interval    : Interval_Kind := Closed;
      Base        : Real_Element := 10.0) return Real_Tensor_Type;

   overriding
   function Array_Range (Start, Stop, Step : Element) return Real_Tensor_Type;

   overriding
   function Identity (Size : Positive; Offset : Integer := 0) return Real_Tensor_Type;

   ----------------------------------------------------------------------------

   overriding
   function Upper_Triangular (Object : Real_Tensor_Type; Offset : Integer := 0) return Real_Tensor_Type;

   overriding
   function Trace (Object : Real_Tensor_Type; Offset : Integer := 0) return Element;

   ----------------------------------------------------------------------------
   --                            Matrix operations                           --
   ----------------------------------------------------------------------------

   overriding function "**" (Left : Real_Tensor_Type; Right : Integer) return Real_Tensor_Type;

   overriding
   function Inverse (Object : Real_Tensor_Type) return Real_Tensor_Type;

   overriding
   function Solve (A, B : Real_Tensor_Type; Solution : out Solution_Kind) return Real_Tensor_Type;

   overriding
   function QR (Object : Real_Tensor_Type) return Real_Tensor_Type;

   overriding
   function QR (Object : Real_Tensor_Type; Mode : QR_Mode := Reduced) return QR_Factorization'Class;

   overriding
   function QR_For_Least_Squares (Object : Real_Tensor_Type) return QR_Factorization'Class;

   overriding
   function Least_Squares (Object : QR_Factorization'Class; B : Real_Tensor_Type) return Real_Tensor_Type;

   overriding
   function Least_Squares (A, B : Real_Tensor_Type) return Real_Tensor_Type;

   overriding
   function Constrained_Least_Squares (A, B, C, D : Real_Tensor_Type) return Real_Tensor_Type;

   overriding
   function Cholesky (Object : Real_Tensor_Type; Form : Triangular_Form := Lower) return Real_Tensor_Type;

   overriding
   function Cholesky_Update
     (R, V : Real_Tensor_Type;
      Mode : Update_Mode) return Real_Tensor_Type;

   overriding
   function Solve (A, B : Real_Tensor_Type; Form : Triangular_Form) return Real_Tensor_Type;

   overriding
   function Divide_By (B, A : Real_Tensor_Type) return Real_Tensor_Type;

   overriding
   function Divide_By (B, A : Real_Tensor_Type; Form : Triangular_Form) return Real_Tensor_Type;

   ----------------------------------------------------------------------------
   --                            Vector operations                           --
   ----------------------------------------------------------------------------

   overriding
   function Norm (Object : Real_Tensor_Type) return Element;

   overriding
   function Normalize (Object : Real_Tensor_Type) return Real_Tensor_Type;

   overriding
   function Standardize (Object : Real_Tensor_Type) return Real_Tensor_Type;

   overriding
   function Correlation_Coefficient (Left, Right : Real_Tensor_Type) return Correlation_Element;

   ----------------------------------------------------------------------------
   --                               Statistics                               --
   ----------------------------------------------------------------------------

   overriding function Quantile (Object : Real_Tensor_Type; P : Probability) return Element;

   overriding function Median (Object : Real_Tensor_Type) return Element;

   overriding function Mean (Object : Real_Tensor_Type) return Element;

   overriding
   function Variance (Object : Real_Tensor_Type; Offset : Natural := 0) return Element;

   overriding
   function Standard_Deviation (Object : Real_Tensor_Type; Offset : Natural := 0) return Element;

   ----------------------------------------------------------------------------

   overriding
   function Mean (Object : Real_Tensor_Type; Axis : Tensor_Axis) return Real_Tensor_Type;

   overriding
   function Variance
     (Object : Real_Tensor_Type;
      Axis   : Tensor_Axis;
      Offset : Natural := 0) return Real_Tensor_Type;

   overriding
   function Median (Object : Real_Tensor_Type; Axis : Tensor_Axis) return Real_Tensor_Type;

   overriding
   function Standard_Deviation
     (Object : Real_Tensor_Type;
      Axis   : Tensor_Axis;
      Offset : Natural := 0) return Real_Tensor_Type;

   ----------------------------------------------------------------------------
   --                              Comparisons                               --
   ----------------------------------------------------------------------------

   overriding
   function All_Close
     (Left, Right        : Real_Tensor_Type;
      Relative_Tolerance : Real_Element := 1.0e-05;
      Absolute_Tolerance : Real_Element := Real_Element'Model_Epsilon) return Boolean;

end Orka.Numerics.Tensors.Operations;
