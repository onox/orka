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

generic
   type Vector_Index_Type is (<>);

   type Integer_Type is range <>;
   type Integer_Vector_Type is array (Vector_Index_Type) of Integer_Type;

   --  Arithmetic
   with function "+" (Left, Right : Integer_Vector_Type) return Integer_Vector_Type;

   --  Comparison
   with function "and" (Left, Right : Integer_Vector_Type) return Integer_Vector_Type;

   --  Shift
   with function Shift_Elements_Left  (Elements : Integer_Vector_Type) return Integer_Vector_Type;
   with function Shift_Elements_Right (Elements : Integer_Vector_Type) return Integer_Vector_Type;

   --  Logical
   with function All_Ones  (Elements, Mask : Integer_Vector_Type) return Boolean;
   with function All_Zeros (Elements, Mask : Integer_Vector_Type) return Boolean;

   type Vector_Type is array (Vector_Index_Type) of Element_Type;

   --  Arithmetic
   with function "*" (Left, Right : Vector_Type) return Vector_Type;
   with function "/" (Left, Right : Vector_Type) return Vector_Type;
   with function "+" (Left, Right : Vector_Type) return Vector_Type;
   with function "-" (Left, Right : Vector_Type) return Vector_Type;
   with function "-" (Elements : Vector_Type) return Vector_Type;
   with function "abs" (Elements : Vector_Type) return Vector_Type;
   with function Sum (Elements : Vector_Type) return Element_Type;
   with function Divide_Or_Zero (Left, Right : Vector_Type) return Vector_Type;
   with function Sqrt (Elements : Vector_Type) return Vector_Type;
   with function Min (Left, Right : Vector_Type) return Vector_Type;
   with function Max (Left, Right : Vector_Type) return Vector_Type;

   --  Logical
   with function Ceil (Elements : Vector_Type) return Vector_Type;
   with function Floor (Elements : Vector_Type) return Vector_Type;
   with function Round (Elements : Vector_Type) return Vector_Type;
   with function Truncate (Elements : Vector_Type) return Vector_Type;

   --  Comparison
   with function And_Not (Left, Right : Vector_Type) return Vector_Type;
   with function "and" (Left, Right : Vector_Type) return Vector_Type;
   with function "or" (Left, Right : Vector_Type) return Vector_Type;
   with function "xor" (Left, Right : Vector_Type) return Vector_Type;
   with function "=" (Left, Right : Vector_Type) return Vector_Type;
   with function "/=" (Left, Right : Vector_Type) return Vector_Type;
   with function ">" (Left, Right : Vector_Type) return Vector_Type;
   with function "<" (Left, Right : Vector_Type) return Vector_Type;
   with function ">=" (Left, Right : Vector_Type) return Vector_Type;
   with function "<=" (Left, Right : Vector_Type) return Vector_Type;

   type Random_Number_State is limited private;

   with procedure Next (State : in out Random_Number_State; Value : out Vector_Type);
   with procedure Reset (State : out Random_Number_State; Seed : Duration);
package Orka.Numerics.Tensors.SIMD_CPU is
   pragma Preelaborate;

   type CPU_Tensor (<>) is new Tensor with private;

   overriding function Is_Materialized (Object : CPU_Tensor) return Boolean is (True);

   overriding procedure Materialize (Object : in out CPU_Tensor) is null;

   overriding function Kind (Object : CPU_Tensor) return Data_Type;

   overriding function Get (Object : CPU_Tensor; Index : Index_Type) return Element;
   overriding function Get (Object : CPU_Tensor; Index : Index_Type) return Boolean;
   overriding function Get (Object : CPU_Tensor; Index : Index_Type) return CPU_Tensor;

   overriding function Get (Object : CPU_Tensor; Index : Range_Type) return CPU_Tensor;

   overriding function Get (Object : CPU_Tensor; Index : Tensor_Index) return Element;
   overriding function Get (Object : CPU_Tensor; Index : Tensor_Index) return Boolean;
   overriding function Get (Object : CPU_Tensor; Index : Tensor_Range) return CPU_Tensor;

   overriding function Get (Object : CPU_Tensor; Index : CPU_Tensor) return CPU_Tensor;

   ----------------------------------------------------------------------------

   overriding procedure Set (Object : in out CPU_Tensor; Index : Index_Type; Value : Element);
   overriding procedure Set (Object : in out CPU_Tensor; Index : Index_Type; Value : Boolean);

   overriding procedure Set (Object : in out CPU_Tensor; Index : Tensor_Index; Value : Element);
   overriding procedure Set (Object : in out CPU_Tensor; Index : Tensor_Index; Value : Boolean);

   overriding procedure Set (Object : in out CPU_Tensor; Index : Index_Type;   Value : CPU_Tensor);
   overriding procedure Set (Object : in out CPU_Tensor; Index : Range_Type;   Value : CPU_Tensor);
   overriding procedure Set (Object : in out CPU_Tensor; Index : Tensor_Range; Value : CPU_Tensor);

   ----------------------------------------------------------------------------

   overriding
   function Image (Object : CPU_Tensor) return String;

   overriding
   function Shape (Object : CPU_Tensor) return Tensor_Shape;

   overriding
   function Elements (Object : CPU_Tensor) return Natural;

   overriding
   function Axes (Object : CPU_Tensor) return Tensor_Axis;

   ----------------------------------------------------------------------------

   overriding
   function Empty (Shape : Tensor_Shape) return CPU_Tensor;

   overriding
   function Fill (Shape : Tensor_Shape; Value : Element) return CPU_Tensor;

   overriding
   function Zeros (Elements : Positive) return CPU_Tensor;

   overriding
   function Zeros (Shape : Tensor_Shape) return CPU_Tensor;

   overriding
   function Ones (Elements : Positive) return CPU_Tensor;

   overriding
   function Ones (Shape : Tensor_Shape) return CPU_Tensor;

   overriding
   function To_Tensor (Elements : Element_Array; Shape : Tensor_Shape) return CPU_Tensor;

   overriding
   function To_Tensor (Elements : Element_Array) return CPU_Tensor;

   overriding
   function To_Boolean_Tensor
     (Booleans : Boolean_Array;
      Shape    : Tensor_Shape) return CPU_Tensor;

   overriding
   function To_Boolean_Tensor (Booleans : Boolean_Array) return CPU_Tensor;

   overriding
   function Array_Range (Stop : Element) return CPU_Tensor;

   overriding
   function Array_Range (Start, Stop : Element; Step : Element := 1.0) return CPU_Tensor;

   overriding
   function Linear_Space
     (Start, Stop : Element;
      Count       : Positive;
      Interval    : Interval_Kind := Closed) return CPU_Tensor;

   overriding
   function Log_Space
     (Start, Stop : Element;
      Count       : Positive;
      Interval    : Interval_Kind := Closed;
      Base        : Element := 10.0) return CPU_Tensor;

   overriding
   function Geometric_Space
     (Start, Stop : Element;
      Count       : Positive;
      Interval    : Interval_Kind := Closed;
      Base        : Element := 10.0) return CPU_Tensor;

   overriding
   function Identity (Size : Positive; Offset : Integer := 0) return CPU_Tensor;

   overriding
   function Identity (Rows, Columns : Positive; Offset : Integer := 0) return CPU_Tensor;

   overriding
   function Upper_Triangular (Object : CPU_Tensor; Offset : Integer := 0) return CPU_Tensor;

   overriding
   function Main_Diagonal (Object : CPU_Tensor; Offset : Integer := 0) return CPU_Tensor;

   overriding
   function Diagonal (Elements : Element_Array; Offset : Integer := 0) return CPU_Tensor;

   overriding
   function Diagonal (Elements : CPU_Tensor; Offset : Integer := 0) return CPU_Tensor;

   overriding
   function Trace (Object : CPU_Tensor; Offset : Integer := 0) return Element;

   overriding
   function Reshape (Object : CPU_Tensor; Shape : Tensor_Shape) return CPU_Tensor;

   overriding
   function Reshape (Object : CPU_Tensor; Elements : Positive) return CPU_Tensor;

   overriding
   function Flatten (Object : CPU_Tensor) return CPU_Tensor;

   overriding
   function Concatenate
     (Left, Right : CPU_Tensor;
      Axis   : Tensor_Axis) return CPU_Tensor;

   overriding
   function "&" (Left, Right : CPU_Tensor) return CPU_Tensor;

   ----------------------------------------------------------------------------
   --                            Matrix operations                           --
   ----------------------------------------------------------------------------

   overriding
   function "*" (Left, Right : CPU_Tensor) return CPU_Tensor;

   overriding
   function "*" (Left, Right : CPU_Tensor) return Element;

   overriding
   function "**" (Left : CPU_Tensor; Right : Integer) return CPU_Tensor;

   overriding
   function Outer (Left, Right : CPU_Tensor) return CPU_Tensor;

   overriding
   function Inverse (Object : CPU_Tensor) return CPU_Tensor;

   overriding
   function Transpose (Object : CPU_Tensor) return CPU_Tensor;

   overriding
   function Solve (A, B : CPU_Tensor; Solution : out Solution_Kind) return CPU_Tensor;

   overriding
   function Solve (A, B : CPU_Tensor; Form : Triangular_Form) return CPU_Tensor;

   overriding
   function Divide_By (B, A : CPU_Tensor) return CPU_Tensor;

   overriding
   function Divide_By (B, A : CPU_Tensor; Form : Triangular_Form) return CPU_Tensor;

   type CPU_QR_Factorization (<>) is new QR_Factorization with private;

   overriding
   function Determinancy (Object : CPU_QR_Factorization) return Matrix_Determinancy;

   function Q (Object : CPU_QR_Factorization'Class) return CPU_Tensor;
   function R (Object : CPU_QR_Factorization'Class) return CPU_Tensor;

   overriding
   function QR (Object : CPU_Tensor) return CPU_Tensor;

   overriding
   function QR (Object : CPU_Tensor; Mode : QR_Mode := Reduced) return QR_Factorization'Class;

   overriding
   function QR_For_Least_Squares (Object : CPU_Tensor) return QR_Factorization'Class;

   overriding
   function Least_Squares (Object : QR_Factorization'Class; B : CPU_Tensor) return CPU_Tensor;
--     with Pre'Class => CPU_QR_Factorization (Object).Q.Rows = B.Rows;
   --  Note: commented to avoid GNAT bug box

   overriding
   function Least_Squares (A, B : CPU_Tensor) return CPU_Tensor;

   overriding
   function Constrained_Least_Squares (A, B, C, D : CPU_Tensor) return CPU_Tensor;

   overriding
   function Cholesky (Object : CPU_Tensor; Form : Triangular_Form := Lower) return CPU_Tensor;

   overriding
   function Cholesky_Update
     (R, V : CPU_Tensor;
      Mode : Update_Mode) return CPU_Tensor;

   ----------------------------------------------------------------------------
   --                            Vector operations                           --
   ----------------------------------------------------------------------------

   overriding function Norm (Object : CPU_Tensor) return Element;

   overriding function Normalize (Object : CPU_Tensor) return CPU_Tensor;

   overriding function Standardize (Object : CPU_Tensor) return CPU_Tensor;

   overriding
   function Correlation_Coefficient (Left, Right : CPU_Tensor) return Correlation_Element;

   ----------------------------------------------------------------------------
   --                         Element-wise operations                        --
   ----------------------------------------------------------------------------

   overriding function "+" (Left, Right : CPU_Tensor) return CPU_Tensor;

   overriding function "-" (Left, Right : CPU_Tensor) return CPU_Tensor;

   overriding function "/" (Left, Right : CPU_Tensor) return CPU_Tensor;

   overriding function Divide_Or_Zero (Left, Right : CPU_Tensor) return CPU_Tensor;

   overriding function "**" (Left, Right : CPU_Tensor) return CPU_Tensor;

   overriding function "**" (Left : CPU_Tensor; Right : Element) return CPU_Tensor;
   overriding function "**" (Left : Element; Right : CPU_Tensor) return CPU_Tensor;

   overriding function "*" (Left : Element; Right : CPU_Tensor) return CPU_Tensor;
   overriding function "*" (Left : CPU_Tensor; Right : Element) return CPU_Tensor;

   overriding function "/" (Left : Element; Right : CPU_Tensor) return CPU_Tensor;
   overriding function "/" (Left : CPU_Tensor; Right : Element) return CPU_Tensor;

   overriding function "+" (Left : Element; Right : CPU_Tensor) return CPU_Tensor;
   overriding function "+" (Left : CPU_Tensor; Right : Element) return CPU_Tensor;

   overriding function "-" (Left : Element; Right : CPU_Tensor) return CPU_Tensor;
   overriding function "-" (Left : CPU_Tensor; Right : Element) return CPU_Tensor;

   overriding function "-" (Object : CPU_Tensor) return CPU_Tensor;

   overriding function "mod" (Left, Right : CPU_Tensor) return CPU_Tensor;
   overriding function "rem" (Left, Right : CPU_Tensor) return CPU_Tensor;

   overriding function "mod" (Left : CPU_Tensor; Right : Element) return CPU_Tensor;
   overriding function "rem" (Left : CPU_Tensor; Right : Element) return CPU_Tensor;

   overriding function "abs" (Object : CPU_Tensor) return CPU_Tensor;

   ----------------------------------------------------------------------------

   overriding function Add (Left, Right : CPU_Tensor) return CPU_Tensor renames "+";

   overriding function Subtract (Left, Right : CPU_Tensor) return CPU_Tensor renames "-";

   overriding function Multiply (Left, Right : CPU_Tensor) return CPU_Tensor;

   overriding function Power (Left : CPU_Tensor; Right : Integer) return CPU_Tensor;

   overriding function Divide (Left, Right : CPU_Tensor) return CPU_Tensor renames "/";

   ----------------------------------------------------------------------------

   overriding function Min (Left : Element; Right : CPU_Tensor) return CPU_Tensor;
   overriding function Min (Left : CPU_Tensor; Right : Element) return CPU_Tensor;

   overriding function Max (Left : Element; Right : CPU_Tensor) return CPU_Tensor;
   overriding function Max (Left : CPU_Tensor; Right : Element) return CPU_Tensor;

   overriding function Sqrt (Object : CPU_Tensor) return CPU_Tensor;

   overriding function Ceil  (Object : CPU_Tensor) return CPU_Tensor;
   overriding function Floor (Object : CPU_Tensor) return CPU_Tensor;
   overriding function Round (Object : CPU_Tensor) return CPU_Tensor;
   overriding function Truncate (Object : CPU_Tensor) return CPU_Tensor;

   overriding function Exp   (Object : CPU_Tensor) return CPU_Tensor;
   overriding function Log   (Object : CPU_Tensor) return CPU_Tensor;
   overriding function Log10 (Object : CPU_Tensor) return CPU_Tensor;
   overriding function Log2  (Object : CPU_Tensor) return CPU_Tensor;

   ----------------------------------------------------------------------------
   --                              Trigonometry                              --
   ----------------------------------------------------------------------------

   overriding function Sin (Object : CPU_Tensor) return CPU_Tensor;
   overriding function Cos (Object : CPU_Tensor) return CPU_Tensor;
   overriding function Tan (Object : CPU_Tensor) return CPU_Tensor;

   overriding function Arcsin (Object : CPU_Tensor) return CPU_Tensor;
   overriding function Arccos (Object : CPU_Tensor) return CPU_Tensor;
   overriding function Arctan (Left, Right : CPU_Tensor) return CPU_Tensor;

   overriding function Degrees (Object : CPU_Tensor) return CPU_Tensor;
   overriding function Radians (Object : CPU_Tensor) return CPU_Tensor;

   ----------------------------------------------------------------------------

   overriding
   function Reduce_Associative
     (Object    : CPU_Tensor;
      Subject   : Expression'Class;
      Initial   : Element) return Element;

   overriding
   function Reduce_Associative
     (Object  : CPU_Tensor;
      Subject : Expression'Class;
      Initial : Element;
      Axis    : Tensor_Axis) return CPU_Tensor;

   overriding
   function Reduce
     (Object    : CPU_Tensor;
      Subject   : Expression'Class;
      Initial   : Element) return Element;

   overriding
   function Reduce
     (Object  : CPU_Tensor;
      Subject : Expression'Class;
      Initial : Element;
      Axis    : Tensor_Axis) return CPU_Tensor;

   overriding function Sum (Object : CPU_Tensor) return Element;

   overriding function Product (Object : CPU_Tensor) return Element;

   overriding
   function Sum (Object : CPU_Tensor; Axis : Tensor_Axis) return CPU_Tensor;

   overriding
   function Product (Object : CPU_Tensor; Axis : Tensor_Axis) return CPU_Tensor;

   ----------------------------------------------------------------------------
   --                               Statistics                               --
   ----------------------------------------------------------------------------

   overriding function Min (Object : CPU_Tensor) return Element;
   overriding function Max (Object : CPU_Tensor) return Element;

   overriding function Quantile (Object : CPU_Tensor; P : Probability) return Element;
   overriding function Median (Object : CPU_Tensor) return Element;

   overriding function Mean (Object : CPU_Tensor) return Element;

   overriding
   function Variance (Object : CPU_Tensor; Offset : Natural := 0) return Element;

   overriding
   function Standard_Deviation (Object : CPU_Tensor; Offset : Natural := 0) return Element;

   overriding
   function Min (Left, Right : CPU_Tensor) return CPU_Tensor;

   overriding
   function Max (Left, Right : CPU_Tensor) return CPU_Tensor;

   overriding
   function Min (Object : CPU_Tensor; Axis : Tensor_Axis) return CPU_Tensor;

   overriding
   function Max (Object : CPU_Tensor; Axis : Tensor_Axis) return CPU_Tensor;

   overriding
   function Quantile
     (Object : CPU_Tensor;
      P      : Probability;
      Axis   : Tensor_Axis) return CPU_Tensor;

   overriding
   function Median (Object : CPU_Tensor; Axis : Tensor_Axis) return CPU_Tensor;

   overriding
   function Mean (Object : CPU_Tensor; Axis : Tensor_Axis) return CPU_Tensor;

   overriding
   function Variance
     (Object : CPU_Tensor;
      Axis   : Tensor_Axis;
      Offset : Natural := 0) return CPU_Tensor;

   overriding
   function Standard_Deviation
     (Object : CPU_Tensor;
      Axis   : Tensor_Axis;
      Offset : Natural := 0) return CPU_Tensor;

   ----------------------------------------------------------------------------
   --                                Logical                                 --
   ----------------------------------------------------------------------------

   overriding function And_Not (Left, Right : CPU_Tensor) return CPU_Tensor;

   overriding function "and" (Left, Right : CPU_Tensor) return CPU_Tensor;

   overriding function "and" (Left : Element; Right : CPU_Tensor) return CPU_Tensor;

   overriding function "or"  (Left, Right : CPU_Tensor) return CPU_Tensor;

   overriding function "xor" (Left, Right : CPU_Tensor) return CPU_Tensor;

   overriding function "not" (Object : CPU_Tensor) return CPU_Tensor;

   ----------------------------------------------------------------------------
   --                              Comparisons                               --
   ----------------------------------------------------------------------------

   overriding function "="  (Left : CPU_Tensor; Right : Element) return CPU_Tensor;
   overriding function "/=" (Left : CPU_Tensor; Right : Element) return CPU_Tensor;

   overriding function ">"  (Left : CPU_Tensor; Right : Element) return CPU_Tensor;
   overriding function "<"  (Left : CPU_Tensor; Right : Element) return CPU_Tensor;

   overriding function ">=" (Left : CPU_Tensor; Right : Element) return CPU_Tensor;
   overriding function "<=" (Left : CPU_Tensor; Right : Element) return CPU_Tensor;

   ----------------------------------------------------------------------------

   overriding function "="  (Left : Element; Right : CPU_Tensor) return CPU_Tensor;
   overriding function "/=" (Left : Element; Right : CPU_Tensor) return CPU_Tensor;

   overriding function ">"  (Left : Element; Right : CPU_Tensor) return CPU_Tensor;
   overriding function "<"  (Left : Element; Right : CPU_Tensor) return CPU_Tensor;

   overriding function ">=" (Left : Element; Right : CPU_Tensor) return CPU_Tensor;
   overriding function "<=" (Left : Element; Right : CPU_Tensor) return CPU_Tensor;

   ----------------------------------------------------------------------------

   overriding function "=" (Left, Right : CPU_Tensor) return Boolean;

   overriding function "="  (Left, Right : CPU_Tensor) return CPU_Tensor;
   overriding function "/=" (Left, Right : CPU_Tensor) return CPU_Tensor;

   overriding function ">"  (Left, Right : CPU_Tensor) return CPU_Tensor;
   overriding function "<"  (Left, Right : CPU_Tensor) return CPU_Tensor;

   overriding function ">=" (Left, Right : CPU_Tensor) return CPU_Tensor;
   overriding function "<=" (Left, Right : CPU_Tensor) return CPU_Tensor;

   ----------------------------------------------------------------------------

   overriding
   function All_Close
     (Left, Right        : CPU_Tensor;
      Relative_Tolerance : Element := 1.0e-05;
      Absolute_Tolerance : Element := Element_Type'Model_Epsilon) return Boolean;

   overriding
   function Any_True (Object : CPU_Tensor; Axis : Tensor_Axis) return CPU_Tensor;

   overriding
   function Any_True (Object : CPU_Tensor) return Boolean;

   overriding
   function All_True (Object : CPU_Tensor; Axis : Tensor_Axis) return CPU_Tensor;

   overriding
   function All_True (Object : CPU_Tensor) return Boolean;

   ----------------------------------------------------------------------------

   procedure Reset_Random (Seed : Duration);

   overriding function Random_Uniform (Shape : Tensor_Shape) return CPU_Tensor;
   --  Return a tensor with elements from a uniform distribution in [0.0, 1.0)

private

   type Vector_Array is array (Index_Type range <>) of Vector_Type;

   type CPU_Tensor (Axes : Tensor_Axis; Size : Natural; Kind : Data_Type)
     is new Tensor with
   record
      Shape : Tensor_Shape (1 .. Axes);
      Data  : Vector_Array (1 .. Size);
   end record;

   type CPU_QR_Factorization (Q_Size, R_Size : Natural) is new QR_Factorization with record
      Q : CPU_Tensor (Axes => 2, Size => Q_Size, Kind => Float_Type);
      R : CPU_Tensor (Axes => 2, Size => R_Size, Kind => Float_Type);
      Determinancy : Matrix_Determinancy;
   end record;

   function Q (Object : CPU_QR_Factorization'Class) return CPU_Tensor is (Object.Q);
   function R (Object : CPU_QR_Factorization'Class) return CPU_Tensor is (Object.R);

   overriding
   function Determinancy (Object : CPU_QR_Factorization) return Matrix_Determinancy is
     (Object.Determinancy);

   Random_State : Random_Number_State;

end Orka.Numerics.Tensors.SIMD_CPU;
