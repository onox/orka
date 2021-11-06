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

private with Ada.Containers.Indefinite_Holders;

generic
   type Integer_Type is range <>;
   type Integer_Vector_Type is array (Index_Homogeneous) of Integer_Type;

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

   type Vector_Type is array (Index_Homogeneous) of Element_Type;

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
package Orka.Numerics.Tensors.SIMD_CPU is
   pragma Preelaborate;

   type CPU_Tensor (<>) is new Tensor with private;

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

   overriding
   function Image (Object : CPU_Tensor) return String;

   overriding
   function Shape (Object : CPU_Tensor) return Tensor_Shape;

   overriding
   function Elements (Object : CPU_Tensor) return Natural;

   overriding
   function Dimensions (Object : CPU_Tensor) return Tensor_Dimension;

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
   function To_Tensor (Elements : Element_Array) return CPU_Tensor;

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
      Dimension   : Tensor_Dimension) return CPU_Tensor;

   overriding
   function "&" (Left, Right : CPU_Tensor) return CPU_Tensor;

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
   function Reduce
     (Object    : CPU_Tensor;
      Subject   : Expression'Class;
      Initial   : Element) return Element;

   overriding
   function Reduce
     (Object    : CPU_Tensor;
      Subject   : Expression'Class;
      Initial   : Element;
      Dimension : Tensor_Dimension) return CPU_Tensor;

   overriding function Sum (Object : CPU_Tensor) return Element;

   overriding function Product (Object : CPU_Tensor) return Element;

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
   function Min (Object : CPU_Tensor; Dimension : Tensor_Dimension) return CPU_Tensor;

   overriding
   function Max (Object : CPU_Tensor; Dimension : Tensor_Dimension) return CPU_Tensor;

   overriding
   function Quantile
     (Object    : CPU_Tensor;
      P         : Probability;
      Dimension : Tensor_Dimension) return CPU_Tensor;

   overriding
   function Median (Object : CPU_Tensor; Dimension : Tensor_Dimension) return CPU_Tensor;

   overriding
   function Mean (Object : CPU_Tensor; Dimension : Tensor_Dimension) return CPU_Tensor;

   overriding
   function Variance (Object : CPU_Tensor; Dimension : Tensor_Dimension) return CPU_Tensor;

   overriding
   function Standard_Deviation
     (Object    : CPU_Tensor;
      Dimension : Tensor_Dimension) return CPU_Tensor;

   ----------------------------------------------------------------------------
   --                                Logical                                 --
   ----------------------------------------------------------------------------

   overriding function And_Not (Left, Right : CPU_Tensor) return CPU_Tensor;

   overriding function "and" (Left, Right : CPU_Tensor) return CPU_Tensor;

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
   function Any_True (Object : CPU_Tensor; Dimension : Tensor_Dimension) return CPU_Tensor;

   overriding
   function Any_True (Object : CPU_Tensor) return Boolean;

   overriding
   function All_True (Object : CPU_Tensor; Dimension : Tensor_Dimension) return CPU_Tensor;

   overriding
   function All_True (Object : CPU_Tensor) return Boolean;

   ----------------------------------------------------------------------------

   type CPU_Expression (<>) is new Expression with private;

   overriding function "+" (Left, Right : CPU_Expression) return CPU_Expression;
   overriding function "-" (Left, Right : CPU_Expression) return CPU_Expression;
   overriding function "*" (Left, Right : CPU_Expression) return CPU_Expression;
   overriding function "/" (Left, Right : CPU_Expression) return CPU_Expression;

   overriding function Min (Left, Right : CPU_Expression) return CPU_Expression;
   overriding function Max (Left, Right : CPU_Expression) return CPU_Expression;

   overriding function "+" (Left : Element; Right : CPU_Expression) return CPU_Expression;
   overriding function "+" (Left : CPU_Expression; Right : Element) return CPU_Expression;

   overriding function "-" (Left : Element; Right : CPU_Expression) return CPU_Expression;
   overriding function "-" (Left : CPU_Expression; Right : Element) return CPU_Expression;

   overriding function "*" (Left : Element; Right : CPU_Expression) return CPU_Expression;
   overriding function "*" (Left : CPU_Expression; Right : Element) return CPU_Expression;

   overriding function "/" (Left : Element; Right : CPU_Expression) return CPU_Expression;
   overriding function "/" (Left : CPU_Expression; Right : Element) return CPU_Expression;

   overriding function "-" (Value : CPU_Expression) return CPU_Expression;

   overriding function "abs" (Value : CPU_Expression) return CPU_Expression;

   overriding function Sqrt (Value : CPU_Expression) return CPU_Expression;

   overriding function Min (Left : Element; Right : CPU_Expression) return CPU_Expression;
   overriding function Min (Left : CPU_Expression; Right : Element) return CPU_Expression;

   overriding function Max (Left : Element; Right : CPU_Expression) return CPU_Expression;
   overriding function Max (Left : CPU_Expression; Right : Element) return CPU_Expression;

   overriding function X return CPU_Expression;
   overriding function Y return CPU_Expression;

private

   type Vector_Array is array (Index_Type range <>) of Vector_Type;

   type CPU_Tensor (Dimensions : Tensor_Dimension; Size : Natural; Kind : Data_Type)
     is new Tensor with
   record
      Shape : Tensor_Shape (1 .. Dimensions);
      Data  : Vector_Array (1 .. Size);
   end record;

   ----------------------------------------------------------------------------

   type Argument_Kind is (X, Y);

   type Binary_Operation_Kind is (Add, Subtract, Multiply, Divide, Min, Max);

   type Unary_Operation_Kind is (Minus, Absolute, Sqrt);

   type CPU_Expression_Kind is (Argument, Number, Binary_Operation, Unary_Operation);

   package Expression_Holders is new Ada.Containers.Indefinite_Holders (Expression'Class);

   type CPU_Expression (Kind : CPU_Expression_Kind) is new Expression with record
      case Kind is
         when Argument =>
            Argument : Argument_Kind;
         when Number =>
            Number : Element;
         when Binary_Operation =>
            Operator : Binary_Operation_Kind;
            Left, Right : Expression_Holders.Holder;
         when Unary_Operation =>
            Unary_Operator : Unary_Operation_Kind;
            Expression : Expression_Holders.Holder;
      end case;
   end record;

   function Apply
     (Object      : CPU_Expression;
      Left, Right : Vector_Type) return Vector_Type;

   function Apply
     (Object      : CPU_Expression;
      Left, Right : Element) return Element;

end Orka.Numerics.Tensors.SIMD_CPU;
