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

private with Ada.Finalization;

private with Orka.Rendering.Buffers;

with Orka.Resources.Locations;

generic
package Orka.Numerics.Tensors.CS_GPU is
   pragma Preelaborate;

   procedure Initialize_Shaders
     (Prefix_Sum, Tensors_GPU : Resources.Locations.Location_Ptr);

   type GPU_Tensor (<>) is new Tensor with private;

   overriding procedure Materialize (Object : in out GPU_Tensor);

   overriding function Is_Materialized (Object : GPU_Tensor) return Boolean;

   overriding function Kind (Object : GPU_Tensor) return Data_Type;

   overriding function Get (Object : GPU_Tensor; Index : Index_Type) return Element;
   overriding function Get (Object : GPU_Tensor; Index : Index_Type) return Boolean;
   overriding function Get (Object : GPU_Tensor; Index : Index_Type) return GPU_Tensor;

   overriding function Get (Object : GPU_Tensor; Index : Range_Type) return GPU_Tensor;

   overriding function Get (Object : GPU_Tensor; Index : Tensor_Index) return Element;
   overriding function Get (Object : GPU_Tensor; Index : Tensor_Index) return Boolean;
   overriding function Get (Object : GPU_Tensor; Index : Tensor_Range) return GPU_Tensor;

   overriding function Get (Object : GPU_Tensor; Index : GPU_Tensor) return GPU_Tensor;

   ----------------------------------------------------------------------------

   overriding procedure Set (Object : in out GPU_Tensor; Index : Index_Type; Value : Element);
   overriding procedure Set (Object : in out GPU_Tensor; Index : Index_Type; Value : Boolean);

   overriding procedure Set (Object : in out GPU_Tensor; Index : Tensor_Index; Value : Element);
   overriding procedure Set (Object : in out GPU_Tensor; Index : Tensor_Index; Value : Boolean);

   overriding procedure Set (Object : in out GPU_Tensor; Index : Index_Type;   Value : GPU_Tensor);
   overriding procedure Set (Object : in out GPU_Tensor; Index : Range_Type;   Value : GPU_Tensor);
   overriding procedure Set (Object : in out GPU_Tensor; Index : Tensor_Range; Value : GPU_Tensor);

   ----------------------------------------------------------------------------

   overriding
   function Image (Object : GPU_Tensor) return String;

   overriding
   function Shape (Object : GPU_Tensor) return Tensor_Shape;

   overriding
   function Elements (Object : GPU_Tensor) return Natural;

   overriding
   function Axes (Object : GPU_Tensor) return Tensor_Axis;

   ----------------------------------------------------------------------------

   overriding
   function Empty (Shape : Tensor_Shape) return GPU_Tensor;

   overriding
   function Fill (Shape : Tensor_Shape; Value : Element) return GPU_Tensor;

   overriding
   function Zeros (Elements : Positive) return GPU_Tensor;

   overriding
   function Zeros (Shape : Tensor_Shape) return GPU_Tensor;

   overriding
   function Ones (Elements : Positive) return GPU_Tensor;

   overriding
   function Ones (Shape : Tensor_Shape) return GPU_Tensor;

   overriding
   function To_Tensor (Elements : Element_Array; Shape : Tensor_Shape) return GPU_Tensor;

   overriding
   function To_Tensor (Elements : Element_Array) return GPU_Tensor;

   overriding
   function To_Boolean_Tensor
     (Booleans : Boolean_Array;
      Shape    : Tensor_Shape) return GPU_Tensor;

   overriding
   function To_Boolean_Tensor (Booleans : Boolean_Array) return GPU_Tensor;

   overriding
   function Array_Range (Stop : Element) return GPU_Tensor;

   overriding
   function Array_Range (Start, Stop : Element; Step : Element := 1.0) return GPU_Tensor;

   overriding
   function Linear_Space
     (Start, Stop : Element;
      Count       : Positive;
      Interval    : Interval_Kind := Closed) return GPU_Tensor;

   overriding
   function Log_Space
     (Start, Stop : Element;
      Count       : Positive;
      Interval    : Interval_Kind := Closed;
      Base        : Element := 10.0) return GPU_Tensor;

   overriding
   function Geometric_Space
     (Start, Stop : Element;
      Count       : Positive;
      Interval    : Interval_Kind := Closed;
      Base        : Element := 10.0) return GPU_Tensor;

   overriding
   function Identity (Size : Positive; Offset : Integer := 0) return GPU_Tensor;

   overriding
   function Identity (Rows, Columns : Positive; Offset : Integer := 0) return GPU_Tensor;

   overriding
   function Upper_Triangular (Object : GPU_Tensor; Offset : Integer := 0) return GPU_Tensor;

   overriding
   function Main_Diagonal (Object : GPU_Tensor; Offset : Integer := 0) return GPU_Tensor;

   overriding
   function Diagonal (Elements : Element_Array; Offset : Integer := 0) return GPU_Tensor;

   overriding
   function Diagonal (Elements : GPU_Tensor; Offset : Integer := 0) return GPU_Tensor;

   overriding
   function Trace (Object : GPU_Tensor; Offset : Integer := 0) return Element;

   overriding
   function Reshape (Object : GPU_Tensor; Shape : Tensor_Shape) return GPU_Tensor;

   overriding
   function Reshape (Object : GPU_Tensor; Elements : Positive) return GPU_Tensor;

   overriding
   function Flatten (Object : GPU_Tensor) return GPU_Tensor;

   overriding
   function Concatenate
     (Left, Right : GPU_Tensor;
      Axis   : Tensor_Axis) return GPU_Tensor;

   overriding
   function "&" (Left, Right : GPU_Tensor) return GPU_Tensor;

   ----------------------------------------------------------------------------
   --                            Matrix operations                           --
   ----------------------------------------------------------------------------

   overriding
   function "*" (Left, Right : GPU_Tensor) return GPU_Tensor;

   overriding
   function "*" (Left, Right : GPU_Tensor) return Element;

   overriding
   function "**" (Left : GPU_Tensor; Right : Integer) return GPU_Tensor;

   overriding
   function Outer (Left, Right : GPU_Tensor) return GPU_Tensor;

   overriding
   function Inverse (Object : GPU_Tensor) return GPU_Tensor;

   overriding
   function Transpose (Object : GPU_Tensor) return GPU_Tensor;

   overriding
   function Solve (A, B : GPU_Tensor; Solution : out Solution_Kind) return GPU_Tensor;

   overriding
   function Solve (A, B : GPU_Tensor; Form : Triangular_Form) return GPU_Tensor;

   overriding
   function Divide_By (B, A : GPU_Tensor) return GPU_Tensor;

   overriding
   function Divide_By (B, A : GPU_Tensor; Form : Triangular_Form) return GPU_Tensor;

   type GPU_QR_Factorization (<>) is new QR_Factorization with private;

   overriding
   function Determinancy (Object : GPU_QR_Factorization) return Matrix_Determinancy;

   function Q (Object : GPU_QR_Factorization'Class) return GPU_Tensor;
   function R (Object : GPU_QR_Factorization'Class) return GPU_Tensor;

   overriding
   function QR (Object : GPU_Tensor) return GPU_Tensor;

   overriding
   function QR (Object : GPU_Tensor; Mode : QR_Mode := Reduced) return QR_Factorization'Class;

   overriding
   function QR_For_Least_Squares (Object : GPU_Tensor) return QR_Factorization'Class;

   overriding
   function Least_Squares (Object : QR_Factorization'Class; B : GPU_Tensor) return GPU_Tensor;
--     with Pre'Class => GPU_QR_Factorization (Object).Q.Rows = B.Rows;
   --  Note: commented to avoid GNAT bug box

   overriding
   function Least_Squares (A, B : GPU_Tensor) return GPU_Tensor;

   overriding
   function Constrained_Least_Squares (A, B, C, D : GPU_Tensor) return GPU_Tensor;

   overriding
   function Cholesky (Object : GPU_Tensor; Form : Triangular_Form := Lower) return GPU_Tensor;

   overriding
   function Cholesky_Update
     (R, V : GPU_Tensor;
      Mode : Update_Mode) return GPU_Tensor;

   ----------------------------------------------------------------------------
   --                            Vector operations                           --
   ----------------------------------------------------------------------------

   overriding function Norm (Object : GPU_Tensor) return Element;

   overriding function Normalize (Object : GPU_Tensor) return GPU_Tensor;

   overriding function Standardize (Object : GPU_Tensor) return GPU_Tensor;

   overriding
   function Correlation_Coefficient (Left, Right : GPU_Tensor) return Correlation_Element;

   ----------------------------------------------------------------------------
   --                         Element-wise operations                        --
   ----------------------------------------------------------------------------

   overriding function "+" (Left, Right : GPU_Tensor) return GPU_Tensor;

   overriding function "-" (Left, Right : GPU_Tensor) return GPU_Tensor;

   overriding function "/" (Left, Right : GPU_Tensor) return GPU_Tensor;

   overriding function Divide_Or_Zero (Left, Right : GPU_Tensor) return GPU_Tensor;

   overriding function "**" (Left, Right : GPU_Tensor) return GPU_Tensor;

   overriding function "**" (Left : GPU_Tensor; Right : Element) return GPU_Tensor;
   overriding function "**" (Left : Element; Right : GPU_Tensor) return GPU_Tensor;

   overriding function "*" (Left : Element; Right : GPU_Tensor) return GPU_Tensor;
   overriding function "*" (Left : GPU_Tensor; Right : Element) return GPU_Tensor;

   overriding function "/" (Left : Element; Right : GPU_Tensor) return GPU_Tensor;
   overriding function "/" (Left : GPU_Tensor; Right : Element) return GPU_Tensor;

   overriding function "+" (Left : Element; Right : GPU_Tensor) return GPU_Tensor;
   overriding function "+" (Left : GPU_Tensor; Right : Element) return GPU_Tensor;

   overriding function "-" (Left : Element; Right : GPU_Tensor) return GPU_Tensor;
   overriding function "-" (Left : GPU_Tensor; Right : Element) return GPU_Tensor;

   overriding function "-" (Object : GPU_Tensor) return GPU_Tensor;

   overriding function "mod" (Left, Right : GPU_Tensor) return GPU_Tensor;
   overriding function "rem" (Left, Right : GPU_Tensor) return GPU_Tensor;

   overriding function "mod" (Left : GPU_Tensor; Right : Element) return GPU_Tensor;
   overriding function "rem" (Left : GPU_Tensor; Right : Element) return GPU_Tensor;

   overriding function "abs" (Object : GPU_Tensor) return GPU_Tensor;

   ----------------------------------------------------------------------------

   overriding function Add (Left, Right : GPU_Tensor) return GPU_Tensor renames "+";

   overriding function Subtract (Left, Right : GPU_Tensor) return GPU_Tensor renames "-";

   overriding function Multiply (Left, Right : GPU_Tensor) return GPU_Tensor;

   overriding function Power (Left : GPU_Tensor; Right : Integer) return GPU_Tensor;

   overriding function Divide (Left, Right : GPU_Tensor) return GPU_Tensor renames "/";

   ----------------------------------------------------------------------------

   overriding function Min (Left : Element; Right : GPU_Tensor) return GPU_Tensor;
   overriding function Min (Left : GPU_Tensor; Right : Element) return GPU_Tensor;

   overriding function Max (Left : Element; Right : GPU_Tensor) return GPU_Tensor;
   overriding function Max (Left : GPU_Tensor; Right : Element) return GPU_Tensor;

   overriding function Sqrt (Object : GPU_Tensor) return GPU_Tensor;

   overriding function Ceil  (Object : GPU_Tensor) return GPU_Tensor;
   overriding function Floor (Object : GPU_Tensor) return GPU_Tensor;
   overriding function Round (Object : GPU_Tensor) return GPU_Tensor;
   overriding function Truncate (Object : GPU_Tensor) return GPU_Tensor;

   overriding function Exp   (Object : GPU_Tensor) return GPU_Tensor;
   overriding function Log   (Object : GPU_Tensor) return GPU_Tensor;
   overriding function Log10 (Object : GPU_Tensor) return GPU_Tensor;
   overriding function Log2  (Object : GPU_Tensor) return GPU_Tensor;

   ----------------------------------------------------------------------------
   --                              Trigonometry                              --
   ----------------------------------------------------------------------------

   overriding function Sin (Object : GPU_Tensor) return GPU_Tensor;
   overriding function Cos (Object : GPU_Tensor) return GPU_Tensor;
   overriding function Tan (Object : GPU_Tensor) return GPU_Tensor;

   overriding function Arcsin (Object : GPU_Tensor) return GPU_Tensor;
   overriding function Arccos (Object : GPU_Tensor) return GPU_Tensor;
   overriding function Arctan (Left, Right : GPU_Tensor) return GPU_Tensor;

   overriding function Degrees (Object : GPU_Tensor) return GPU_Tensor;
   overriding function Radians (Object : GPU_Tensor) return GPU_Tensor;

   ----------------------------------------------------------------------------

   overriding
   function Reduce_Associative
     (Object  : GPU_Tensor;
      Subject : Expression'Class;
      Initial : Element) return Element;

   overriding
   function Reduce_Associative
     (Object  : GPU_Tensor;
      Subject : Expression'Class;
      Initial : Element;
      Axis    : Tensor_Axis) return GPU_Tensor;

   overriding
   function Reduce
     (Object  : GPU_Tensor;
      Subject : Expression'Class;
      Initial : Element) return Element;

   overriding
   function Reduce
     (Object  : GPU_Tensor;
      Subject : Expression'Class;
      Initial : Element;
      Axis    : Tensor_Axis) return GPU_Tensor;

   overriding function Sum (Object : GPU_Tensor) return Element;

   overriding function Product (Object : GPU_Tensor) return Element;

   overriding
   function Sum (Object : GPU_Tensor; Axis : Tensor_Axis) return GPU_Tensor;

   overriding
   function Product (Object : GPU_Tensor; Axis : Tensor_Axis) return GPU_Tensor;

   ----------------------------------------------------------------------------
   --                               Statistics                               --
   ----------------------------------------------------------------------------

   overriding function Min (Object : GPU_Tensor) return Element;
   overriding function Max (Object : GPU_Tensor) return Element;

   overriding function Quantile (Object : GPU_Tensor; P : Probability) return Element;
   overriding function Median (Object : GPU_Tensor) return Element;

   overriding function Mean (Object : GPU_Tensor) return Element;

   overriding
   function Variance (Object : GPU_Tensor; Offset : Natural := 0) return Element;

   overriding
   function Standard_Deviation (Object : GPU_Tensor; Offset : Natural := 0) return Element;

   overriding
   function Min (Left, Right : GPU_Tensor) return GPU_Tensor;

   overriding
   function Max (Left, Right : GPU_Tensor) return GPU_Tensor;

   overriding
   function Min (Object : GPU_Tensor; Axis : Tensor_Axis) return GPU_Tensor;

   overriding
   function Max (Object : GPU_Tensor; Axis : Tensor_Axis) return GPU_Tensor;

   overriding
   function Quantile
     (Object : GPU_Tensor;
      P      : Probability;
      Axis   : Tensor_Axis) return GPU_Tensor;

   overriding
   function Median (Object : GPU_Tensor; Axis : Tensor_Axis) return GPU_Tensor;

   overriding
   function Mean (Object : GPU_Tensor; Axis : Tensor_Axis) return GPU_Tensor;

   overriding
   function Variance
     (Object : GPU_Tensor;
      Axis   : Tensor_Axis;
      Offset : Natural := 0) return GPU_Tensor;

   overriding
   function Standard_Deviation
     (Object : GPU_Tensor;
      Axis   : Tensor_Axis;
      Offset : Natural := 0) return GPU_Tensor;

   ----------------------------------------------------------------------------
   --                                Logical                                 --
   ----------------------------------------------------------------------------

   overriding function And_Not (Left, Right : GPU_Tensor) return GPU_Tensor;

   overriding function "and" (Left, Right : GPU_Tensor) return GPU_Tensor;

   overriding function "and" (Left : Element; Right : GPU_Tensor) return GPU_Tensor;

   overriding function "or"  (Left, Right : GPU_Tensor) return GPU_Tensor;

   overriding function "xor" (Left, Right : GPU_Tensor) return GPU_Tensor;

   overriding function "not" (Object : GPU_Tensor) return GPU_Tensor;

   ----------------------------------------------------------------------------
   --                              Comparisons                               --
   ----------------------------------------------------------------------------

   overriding function "="  (Left : GPU_Tensor; Right : Element) return GPU_Tensor;
   overriding function "/=" (Left : GPU_Tensor; Right : Element) return GPU_Tensor;

   overriding function ">"  (Left : GPU_Tensor; Right : Element) return GPU_Tensor;
   overriding function "<"  (Left : GPU_Tensor; Right : Element) return GPU_Tensor;

   overriding function ">=" (Left : GPU_Tensor; Right : Element) return GPU_Tensor;
   overriding function "<=" (Left : GPU_Tensor; Right : Element) return GPU_Tensor;

   ----------------------------------------------------------------------------

   overriding function "="  (Left : Element; Right : GPU_Tensor) return GPU_Tensor;
   overriding function "/=" (Left : Element; Right : GPU_Tensor) return GPU_Tensor;

   overriding function ">"  (Left : Element; Right : GPU_Tensor) return GPU_Tensor;
   overriding function "<"  (Left : Element; Right : GPU_Tensor) return GPU_Tensor;

   overriding function ">=" (Left : Element; Right : GPU_Tensor) return GPU_Tensor;
   overriding function "<=" (Left : Element; Right : GPU_Tensor) return GPU_Tensor;

   ----------------------------------------------------------------------------

   overriding function "=" (Left, Right : GPU_Tensor) return Boolean;

   overriding function "="  (Left, Right : GPU_Tensor) return GPU_Tensor;
   overriding function "/=" (Left, Right : GPU_Tensor) return GPU_Tensor;

   overriding function ">"  (Left, Right : GPU_Tensor) return GPU_Tensor;
   overriding function "<"  (Left, Right : GPU_Tensor) return GPU_Tensor;

   overriding function ">=" (Left, Right : GPU_Tensor) return GPU_Tensor;
   overriding function "<=" (Left, Right : GPU_Tensor) return GPU_Tensor;

   ----------------------------------------------------------------------------

   overriding
   function All_Close
     (Left, Right        : GPU_Tensor;
      Relative_Tolerance : Element := 1.0e-05;
      Absolute_Tolerance : Element := Element_Type'Model_Epsilon) return Boolean;

   overriding
   function Any_True (Object : GPU_Tensor; Axis : Tensor_Axis) return GPU_Tensor;

   overriding
   function Any_True (Object : GPU_Tensor) return Boolean;

   overriding
   function All_True (Object : GPU_Tensor; Axis : Tensor_Axis) return GPU_Tensor;

   overriding
   function All_True (Object : GPU_Tensor) return Boolean;

   ----------------------------------------------------------------------------

   procedure Reset_Random (Seed : Duration);

   overriding function Random_Uniform (Shape : Tensor_Shape) return GPU_Tensor;
   --  Return a tensor with elements from a uniform distribution in [0.0, 1.0)

private

   type Buffer_Access is access all Orka.Rendering.Buffers.Buffer;

   type GPU_Tensor_Reference is record
      References   : Natural;
      Materialized : Boolean;
      Data         : Buffer_Access;
   end record
     with Dynamic_Predicate => (if Materialized then Data /= null);

   type GPU_Tensor_Reference_Access is access all GPU_Tensor_Reference;

   ----------------------------------------------------------------------------

   type Value_Kind is (Tensor_Value, Scalar_Value);

   package Tensor_Holders is new Ada.Containers.Indefinite_Holders (Tensor'Class);

   type Value_Type (Kind : Value_Kind := Scalar_Value) is record
      case Kind is
         when Tensor_Value =>
            Tensor : Tensor_Holders.Holder;
         when Scalar_Value =>
            Value : Element;
      end case;
   end record
     with Dynamic_Predicate => (if Kind = Tensor_Value then not Tensor.Is_Empty);

   ----------------------------------------------------------------------------

   type Binary_Operation_Kind is
     (Add, Subtract, Multiply, Divide,
      Equal, Not_Equal, Greater_Than, Greater_Equal, Less_Than, Less_Equal,
      Logical_And, Logical_Or, Logical_Xor,
      Divide_Or_Zero, Power, Modulus, Min, Max, Arctan);

   subtype Binary_Operator is Binary_Operation_Kind range Add .. Logical_Xor;
   subtype Binary_Function is Binary_Operation_Kind range Power .. Arctan;

   subtype Boolean_Operator is Binary_Operator range Equal .. Logical_Xor;

   type Unary_Operation_Kind is
     (Minus, Absolute, Sqrt, Ceil, Floor, Round, Truncate, Exp, Log, Log2,
      Sin, Cos, Tan, Arcsin, Arccos, Logical_Not, Reshape);

   type Constructor_Kind is (Identity, Fill, Linear_Space, Log_Space);

   type Constructor_Type (Kind : Constructor_Kind := Identity) is record
      case Kind is
         when Identity =>
            Offset : Integer;
         when Fill =>
            Value : Element;
         when Linear_Space | Log_Space =>
            Start, Step : Element;
            Base        : Element := 0.0;  --  Only used by Log_Space
      end case;
   end record;

   type Matrix_Operation_Kind is
     (Main_Diagonal, Diagonal, Transpose, Any_True, All_True, Matrix_Matrix, Random);

   type Matrix_Operation_Type (Kind : Matrix_Operation_Kind := Matrix_Matrix) is record
      case Kind is
         when Main_Diagonal | Diagonal | Transpose | Any_True | All_True =>
            Value  : Tensor_Holders.Holder;
            Offset : Integer := 0;  --  Not used by Transpose | Any_True | All_True
         when Matrix_Matrix =>
            Left, Right : Tensor_Holders.Holder;
         when Random =>
            null;
      end case;
   end record;

   ----------------------------------------------------------------------------

   type Operation_Kind is
     (None, Constructor_Operation, Binary_Operation, Unary_Operation, Matrix_Operation);

   type Operation_Type (Kind : Operation_Kind := None) is record
      case Kind is
         when Binary_Operation =>
            Binary_Operator : Binary_Operation_Kind;
            Left, Right     : Value_Type;
         when Unary_Operation =>
            Unary_Operator  : Unary_Operation_Kind;
            Value           : Value_Type;
         when Constructor_Operation =>
            Constructor : Constructor_Type;
         when Matrix_Operation =>
            Matrix_Operation : Matrix_Operation_Type;
         when None =>
            null;
      end case;
   end record;

   type GPU_Tensor
     (Axes : Tensor_Axis;
      Kind : Data_Type)
   is new Ada.Finalization.Controlled and Tensor with record
      Reference : GPU_Tensor_Reference_Access;
      Shape     : Tensor_Shape (1 .. Axes);
      Operation : Operation_Type;
   end record;

   overriding procedure Adjust   (Object : in out GPU_Tensor);
   overriding procedure Finalize (Object : in out GPU_Tensor);

   ----------------------------------------------------------------------------

   type GPU_QR_Factorization is new QR_Factorization with record
      Q, R         : Tensor_Holders.Holder;
      Determinancy : Matrix_Determinancy;
   end record;

   function Q (Object : GPU_QR_Factorization'Class) return GPU_Tensor is
     (GPU_Tensor (Object.Q.Element));
   function R (Object : GPU_QR_Factorization'Class) return GPU_Tensor is
     (GPU_Tensor (Object.R.Element));

   overriding
   function Determinancy (Object : GPU_QR_Factorization) return Matrix_Determinancy is
     (Object.Determinancy);

end Orka.Numerics.Tensors.CS_GPU;
