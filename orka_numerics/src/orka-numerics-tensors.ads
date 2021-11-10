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
   type Element_Type is digits <>;
package Orka.Numerics.Tensors is
   pragma Pure;

   subtype Element is Element_Type;

   type Element_Array is array (Positive range <>) of Element_Type;

   type Boolean_Array is array (Positive range <>) of Boolean;

   type Data_Type is (Float_Type, Int_Type, Bool_Type);

   type Tensor_Dimension is range 1 .. 2;

   type Tensor_Shape is array (Tensor_Dimension range <>) of Natural
     with Default_Component_Value => 0;
   --  1 = rows
   --  2 = columns

   function Elements (Shape : Tensor_Shape) return Natural;

   function Image (Shape : Tensor_Shape) return String;

   function Trim (Value : Natural) return String;

   subtype Index_Type is Positive;

   type Tensor_Index is array (Tensor_Dimension) of Index_Type
     with Default_Component_Value => Index_Type'First;
   --  TODO Support negative indices in function Get?

   type Range_Type is record
      Start, Stop : Index_Type := Index_Type'First;
   end record
     with Dynamic_Predicate => Range_Type.Start <= Range_Type.Stop
       or else raise Constraint_Error with
         "Range start (" & Trim (Range_Type.Start) & ") > stop (" & Trim (Range_Type.Stop) & ")";

   type Tensor_Range is array (Tensor_Dimension) of Range_Type;

   ----------------------------------------------------------------------------

   type Tensor is interface
     with Constant_Indexing => Get;

   function Kind (Object : Tensor) return Data_Type is abstract;

   function Get (Object : Tensor; Index : Index_Type) return Element is abstract
     with Pre'Class => Object.Kind = Float_Type and Object.Dimensions = 1;
   --  Return the value of a vector

   function Get (Object : Tensor; Index : Index_Type) return Boolean is abstract
     with Pre'Class => Object.Kind = Bool_Type and Object.Dimensions = 1;
   --  Return the value of a boolean vector

   function Get (Object : Tensor; Index : Index_Type) return Tensor is abstract
     with Pre'Class  => Object.Dimensions = 2,
          Post'Class => Get'Result.Dimensions = 1 and Get'Result.Shape (1) = Object.Shape (2);
   --  Return the row of a matrix as a vector

   function Get (Object : Tensor; Index : Tensor_Index) return Element is abstract
     with Pre'Class => Object.Kind = Float_Type and Object.Dimensions = 2;
   --  Return the value of a matrix

   function Get (Object : Tensor; Index : Tensor_Index) return Boolean is abstract
     with Pre'Class => Object.Kind = Bool_Type and Object.Dimensions = 2;
   --  Return the value of a boolean matrix

   function Get (Object : Tensor; Index : Range_Type) return Tensor is abstract
     with Post'Class => Object.Dimensions = Get'Result.Dimensions;

   function Get (Object : Tensor; Index : Tensor_Range) return Tensor is abstract
     with Post'Class => Object.Dimensions = Get'Result.Dimensions;
   --  TODO Add Pre'Class => Dimensions (Index) <= Object.Dimensions

   function Get (Object : Tensor; Index : Tensor) return Tensor is abstract
     with Pre'Class => Index.Kind = Bool_Type and Index.Dimensions in 1 .. Object.Dimensions;

   ----------------------------------------------------------------------------

   function Image (Object : Tensor) return String is abstract;

   function Shape (Object : Tensor) return Tensor_Shape is abstract;

   function Elements (Object : Tensor) return Natural is abstract;

   function Dimensions (Object : Tensor) return Tensor_Dimension is abstract;

   function Fill (Shape : Tensor_Shape; Value : Element) return Tensor is abstract;

   function Zeros (Elements : Positive) return Tensor is abstract;
   function Zeros (Shape : Tensor_Shape) return Tensor is abstract;
   --  Return a tensor filled with zeros

   function Ones (Elements : Positive) return Tensor is abstract;
   function Ones (Shape : Tensor_Shape) return Tensor is abstract;
   --  Return a tensor filled with ones

   function To_Tensor (Elements : Element_Array) return Tensor is abstract
     with Post'Class => To_Tensor'Result.Kind = Float_Type;

   function To_Boolean_Tensor (Booleans : Boolean_Array) return Tensor is abstract
     with Post'Class => To_Boolean_Tensor'Result.Kind = Bool_Type;

   ----------------------------------------------------------------------------

   function Array_Range (Stop : Element) return Tensor is abstract
     with Post'Class => Array_Range'Result.Dimensions = 1;

   function Array_Range (Start, Stop : Element; Step : Element := 1.0) return Tensor is abstract
     with Pre'Class  => Start < Stop and Step > 0.0,
          Post'Class => Array_Range'Result.Dimensions = 1;

   type Interval_Kind is (Closed, Half_Open);

   function Linear_Space
     (Start, Stop : Element;
      Count       : Positive;
      Interval    : Interval_Kind := Closed) return Tensor is abstract
   with Post'Class => Linear_Space'Result.Dimensions = 1
                        and Linear_Space'Result.Elements = Count;
   --  Return a 1D tensor containing numbers in a linear scale in the
   --  interval [start, stop] when interval is closed or [start, stop) when half open.

   function Log_Space
     (Start, Stop : Element;
      Count       : Positive;
      Interval    : Interval_Kind := Closed;
      Base        : Element := 10.0) return Tensor is abstract
   with Post'Class => Log_Space'Result.Dimensions = 1
                        and Log_Space'Result.Elements = Count;
   --  Return a 1D tensor containing numbers in a logarithmic scale in
   --  the interval [base**start, base**stop] when interval is closed
   --  or [base**start, base**stop) when half open.

   function Geometric_Space
     (Start, Stop : Element;
      Count       : Positive;
      Interval    : Interval_Kind := Closed;
      Base        : Element := 10.0) return Tensor is abstract
   with Post'Class => Geometric_Space'Result.Dimensions = 1
                        and Geometric_Space'Result.Elements = Count;
   --  Return a 1D tensor containing numbers in a logarithmic scale in
   --  the interval [base**start, base**stop] when interval is closed
   --  or [base**start, base**stop) when half open.

   ----------------------------------------------------------------------------

   function Identity (Size : Positive; Offset : Integer := 0) return Tensor is abstract
     with Post'Class => Identity'Result.Dimensions = 2
                          and Identity'Result.Shape (1) = Size
                          and Identity'Result.Shape (2) = Size;
   --  Return a tensor with ones on the diagonal (main when K = 0) and zeros everywhere else

   function Identity (Rows, Columns : Positive; Offset : Integer := 0) return Tensor is abstract
     with Post'Class => Identity'Result.Dimensions = 2
                          and Identity'Result.Shape (1) = Rows
                          and Identity'Result.Shape (2) = Columns;
   --  Return a tensor with ones on the diagonal (main when K = 0) and zeros everywhere else

   function Main_Diagonal (Object : Tensor; Offset : Integer := 0) return Tensor is abstract
     with Pre'Class  => Object.Dimensions = 2,
          Post'Class => Main_Diagonal'Result.Dimensions = 1;
   --  Return a 1D tensor filled with the elements of the diagonal (main when K = 0)
   --  of the given tensor

   function Diagonal (Elements : Element_Array; Offset : Integer := 0) return Tensor is abstract
     with Post'Class => Diagonal'Result.Dimensions = 2
                          and Diagonal'Result.Elements = Elements'Length ** 2
                          and Diagonal'Result.Shape (1) = Diagonal'Result.Shape (2);
   --  Return a 2D tensor filled with the given elements on the diagonal (main when K = 0)
   --  and zeros everywhere else

   function Diagonal (Elements : Tensor; Offset : Integer := 0) return Tensor is abstract
     with Pre'Class  => Elements.Dimensions = 1,
          Post'Class => Diagonal'Result.Dimensions = 2
                          and Diagonal'Result.Elements = Elements.Elements ** 2
                          and Diagonal'Result.Shape (1) = Diagonal'Result.Shape (2);
   --  Return a 2D tensor filled with the given elements on the diagonal (main when K = 0)
   --  and zeros everywhere else

   function Trace (Object : Tensor; Offset : Integer := 0) return Element is abstract
     with Pre'Class => Object.Dimensions = 2;
   --  Return the trace of a 2D tensor
   --
   --  The trace is a linear mapping:
   --
   --    tr(A + B) = tr(A) + tr(B)
   --    tr(c * A) = c * tr(A) where c is a scalar (tr(A*B) /= tr(A) * tr(B))
   --
   --  And invariant under cyclic permutation:
   --
   --    tr(A * B * C) = tr(C * A * B)

   function Reshape (Object : Tensor; Shape : Tensor_Shape) return Tensor is abstract
     with Pre'Class  => Object.Elements = Elements (Shape),
          Post'Class => Reshape'Result.Shape = Shape;

   function Reshape (Object : Tensor; Elements : Positive) return Tensor is abstract
     with Pre'Class  => Object.Elements = Elements,
          Post'Class => Reshape'Result.Dimensions = 1;

   function Flatten (Object : Tensor) return Tensor is abstract
     with Pre'Class  => Object.Dimensions = 2,
          Post'Class => Flatten'Result.Dimensions = 1;

   function Concatenate
     (Left, Right : Tensor;
      Dimension   : Tensor_Dimension) return Tensor is abstract
   with Pre'Class  => Left.Dimensions = Right.Dimensions and
                      Left.Kind = Right.Kind and
                      Dimension <= Left.Dimensions and
                      (for all D in Left.Shape'Range =>
                         (if D /= Dimension
                            and D in Left.Shape'Range
                            and D in Right.Shape'Range
                          then
                            Left.Shape (D) = Right.Shape (D))),
        Post'Class => Left.Dimensions = Concatenate'Result.Dimensions;
   --  Return the concatenation of the two tensors in the given dimension

   function "&" (Left, Right : Tensor) return Tensor is abstract;
   --  Return the concatenation of the two tensors in the first dimension

   ----------------------------------------------------------------------------

   function "*" (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class => (if Left.Dimensions > 1 then
                          Left.Shape (2) = Right.Shape (1)
                        else
                          (Right.Dimensions > 1 and Left.Shape (1) = Right.Shape (1)));
                                                    --  Left is a row vector

   function "*" (Left, Right : Tensor) return Element is abstract
     with Pre'Class => (Left.Dimensions = 1 and Right.Dimensions = 1 and
                       Left.Shape = Right.Shape) or else
                         raise Constraint_Error with
                           "Tensors must be vectors with same shape" &
                           " (left = " & Image (Left.Shape) & " and " &
                           "right = " & Image (Right.Shape) & ")";

   function "**" (Left : Tensor; Right : Integer) return Tensor is abstract
     with Pre'Class => Left.Dimensions = 2 and Left.Shape (1) = Left.Shape (2);

   function Outer (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class  => Left.Dimensions = 1 and Right.Dimensions = 1,
          Post'Class => Outer'Result.Dimensions = 2
                          and Outer'Result.Shape = (Left.Elements, Right.Elements);

   function Inverse (Object : Tensor) return Tensor is abstract
     with Pre'Class  => Object.Dimensions = 2,
          Post'Class => Inverse'Result.Dimensions = 2;

   function Transpose (Object : Tensor) return Tensor is abstract
     with Pre'Class  => Object.Dimensions = 2,
          Post'Class => Transpose'Result.Dimensions = 2;

   --  TODO Add LU, QR, SVD, Cholesky, Solve

   ----------------------------------------------------------------------------
   --                         Element-wise operations                        --
   ----------------------------------------------------------------------------

   function "+" (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class => Left.Shape = Right.Shape;

   function "-" (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class => Left.Shape = Right.Shape;

   function "/" (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class => Left.Shape = Right.Shape;

   function Divide_Or_Zero (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class => Left.Shape = Right.Shape;

   function "**" (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class => Left.Shape = Right.Shape;

   function "**" (Left : Tensor; Right : Element) return Tensor is abstract;
   function "**" (Left : Element; Right : Tensor) return Tensor is abstract;

   function "*" (Left : Element; Right : Tensor) return Tensor is abstract;
   function "*" (Left : Tensor; Right : Element) return Tensor is abstract;

   function "/" (Left : Element; Right : Tensor) return Tensor is abstract;
   function "/" (Left : Tensor; Right : Element) return Tensor is abstract;

   function "+" (Left : Element; Right : Tensor) return Tensor is abstract;
   function "+" (Left : Tensor; Right : Element) return Tensor is abstract;

   function "-" (Left : Element; Right : Tensor) return Tensor is abstract;
   function "-" (Left : Tensor; Right : Element) return Tensor is abstract;

   function "-" (Object : Tensor) return Tensor is abstract;

   function "mod" (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class => Left.Shape = Right.Shape;
   --  Values in Right must not be equal to 0.0

   function "rem" (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class => Left.Shape = Right.Shape;
   --  Values in Right must not be equal to 0.0

   function "mod" (Left : Tensor; Right : Element) return Tensor is abstract
     with Pre'Class => Right /= 0.0;

   function "rem" (Left : Tensor; Right : Element) return Tensor is abstract
     with Pre'Class => Right /= 0.0;

   function "abs" (Object : Tensor) return Tensor is abstract;

   ----------------------------------------------------------------------------

   function Add (Left, Right : Tensor) return Tensor renames "+";

   function Subtract (Left, Right : Tensor) return Tensor renames "-";

   function Multiply (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class => Left.Shape = Right.Shape;
   --  Element-wise multiplication

   function Power (Left : Tensor; Right : Integer) return Tensor is abstract;
   --  Element-wise exponentiation

   function Divide (Left, Right : Tensor) return Tensor renames "/";

   ----------------------------------------------------------------------------

   function Sqrt (Object : Tensor) return Tensor is abstract;

   function Ceil (Object : Tensor) return Tensor is abstract;
   function Floor (Object : Tensor) return Tensor is abstract;
   function Round (Object : Tensor) return Tensor is abstract;
   function Truncate (Object : Tensor) return Tensor is abstract;

   function Exp (Object : Tensor) return Tensor is abstract;

   --  Values must be > 0.0
   function Log (Object : Tensor) return Tensor is abstract;
   function Log10 (Object : Tensor) return Tensor is abstract;
   function Log2 (Object : Tensor) return Tensor is abstract;

   ----------------------------------------------------------------------------
   --                              Trigonometry                              --
   ----------------------------------------------------------------------------

   function Sin (Object : Tensor) return Tensor is abstract;
   function Cos (Object : Tensor) return Tensor is abstract;
   function Tan (Object : Tensor) return Tensor is abstract;

   --  Values must be in -1.0 .. 1.0
   function Arcsin (Object : Tensor) return Tensor is abstract;

   --  Values must be in -1.0 .. 1.0
   function Arccos (Object : Tensor) return Tensor is abstract;

   --  Values in Left > 0.0 and Right > 0.0
   function Arctan (Left, Right : Tensor) return Tensor is abstract;

   function Degrees (Object : Tensor) return Tensor is abstract;
   --  Return a tensor with all elements converted from radians to degrees

   function Radians (Object : Tensor) return Tensor is abstract;
   --  Return a tensor with all elements converted from degrees to radians

   ----------------------------------------------------------------------------
   --                              Expressions                               --
   ----------------------------------------------------------------------------

   type Expression is interface;

   function "+" (Left, Right : Expression) return Expression is abstract;
   function "-" (Left, Right : Expression) return Expression is abstract;
   function "*" (Left, Right : Expression) return Expression is abstract;
   function "/" (Left, Right : Expression) return Expression is abstract;

   function Min (Left, Right : Expression) return Expression is abstract;
   function Max (Left, Right : Expression) return Expression is abstract;

   function "+" (Left : Element; Right : Expression) return Expression is abstract;
   function "+" (Left : Expression; Right : Element) return Expression is abstract;

   function "-" (Left : Element; Right : Expression) return Expression is abstract;
   function "-" (Left : Expression; Right : Element) return Expression is abstract;

   function "*" (Left : Element; Right : Expression) return Expression is abstract;
   function "*" (Left : Expression; Right : Element) return Expression is abstract;

   function "/" (Left : Element; Right : Expression) return Expression is abstract;
   function "/" (Left : Expression; Right : Element) return Expression is abstract;

   function "-" (Value : Expression) return Expression is abstract;

   function "abs" (Value : Expression) return Expression is abstract;

   function Sqrt (Value : Expression) return Expression is abstract;

   function Min (Left : Element; Right : Expression) return Expression is abstract;
   function Min (Left : Expression; Right : Element) return Expression is abstract;

   function Max (Left : Element; Right : Expression) return Expression is abstract;
   function Max (Left : Expression; Right : Element) return Expression is abstract;

   function X return Expression is abstract;
   function Y return Expression is abstract;

   --  TODO Add function Cumulative

   function Reduce
     (Object    : Tensor;
      Subject   : Expression'Class;
      Initial   : Element) return Element is abstract;

   function Reduce
     (Object    : Tensor;
      Subject   : Expression'Class;
      Initial   : Element;
      Dimension : Tensor_Dimension) return Tensor is abstract
   with Pre'Class => Dimension <= Object.Dimensions;

   function Sum (Object : Tensor) return Element is abstract;

   function Product (Object : Tensor) return Element is abstract;

   ----------------------------------------------------------------------------
   --                               Statistics                               --
   ----------------------------------------------------------------------------

   type Probability is new Element range 0.0 .. 1.0;

   function Min (Object : Tensor) return Element is abstract;
   function Max (Object : Tensor) return Element is abstract;

   function Quantile (Object : Tensor; P : Probability) return Element is abstract;
   function Median (Object : Tensor) return Element is abstract;

   function Mean (Object : Tensor) return Element is abstract;

   function Variance (Object : Tensor; Offset : Natural := 0) return Element is abstract;
   --  Return the variance (sample variance if Offset = 1)
   --
   --  The returned value is unbiased if Offset = 1 and biased if Offset = 0.

   function Standard_Deviation (Object : Tensor; Offset : Natural := 0) return Element is abstract;
   --  Return the standard deviation
   --
   --  The returned value is biased because of the square root, even when Offset = 1.

   function Min (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class => Left.Shape = Right.Shape;

   function Max (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class => Left.Shape = Right.Shape;

   function Min (Object : Tensor; Dimension : Tensor_Dimension) return Tensor is abstract
     with Pre'Class => Dimension <= Object.Dimensions;

   function Max (Object : Tensor; Dimension : Tensor_Dimension) return Tensor is abstract
     with Pre'Class => Dimension <= Object.Dimensions;

   function Quantile
     (Object    : Tensor;
      P         : Probability;
      Dimension : Tensor_Dimension) return Tensor is abstract
   with Pre'Class => Dimension <= Object.Dimensions;

   function Median (Object : Tensor; Dimension : Tensor_Dimension) return Tensor is abstract
     with Pre'Class => Dimension <= Object.Dimensions;

   function Mean (Object : Tensor; Dimension : Tensor_Dimension) return Tensor is abstract
     with Pre'Class => Dimension <= Object.Dimensions;

   function Variance (Object : Tensor; Dimension : Tensor_Dimension) return Tensor is abstract
     with Pre'Class => Dimension <= Object.Dimensions;

   function Standard_Deviation
     (Object    : Tensor;
      Dimension : Tensor_Dimension) return Tensor is abstract
   with Pre'Class => Dimension <= Object.Dimensions;

   ----------------------------------------------------------------------------
   --                                Logical                                 --
   ----------------------------------------------------------------------------

   function And_Not (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class  => Left.Kind = Bool_Type and Right.Kind = Bool_Type
                          and Left.Shape = Right.Shape,
          Post'Class => And_Not'Result.Kind = Bool_Type;
   --  Return a tensor equal to (not Left) and Right

   function "and" (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class  => Left.Kind = Bool_Type and Right.Kind = Bool_Type
                          and Left.Shape = Right.Shape,
          Post'Class => "and"'Result.Kind = Bool_Type;

   function "or" (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class  => Left.Kind = Bool_Type and Right.Kind = Bool_Type
                          and Left.Shape = Right.Shape,
          Post'Class => "or"'Result.Kind = Bool_Type;

   function "xor" (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class  => Left.Kind = Bool_Type and Right.Kind = Bool_Type
                          and Left.Shape = Right.Shape,
          Post'Class => "xor"'Result.Kind = Bool_Type;

   function "not" (Object : Tensor) return Tensor is abstract
     with Pre'Class  => Object.Kind = Bool_Type,
          Post'Class => "not"'Result.Kind = Bool_Type;
   --  Return a tensor where each boolean element is inverted

   ----------------------------------------------------------------------------
   --                              Comparisons                               --
   ----------------------------------------------------------------------------

   function "=" (Left : Tensor; Right : Element) return Tensor is abstract
     with Post'Class => "="'Result.Kind = Bool_Type;
   function "/=" (Left : Tensor; Right : Element) return Tensor is abstract
     with Post'Class => "/="'Result.Kind = Bool_Type;

   function ">" (Left : Tensor; Right : Element) return Tensor is abstract
     with Pre'Class  => Left.Kind /= Bool_Type,
          Post'Class => ">"'Result.Kind = Bool_Type;
   function "<" (Left : Tensor; Right : Element) return Tensor is abstract
     with Post'Class => "<"'Result.Kind = Bool_Type;

   function ">=" (Left : Tensor; Right : Element) return Tensor is abstract
     with Pre'Class  => Left.Kind /= Bool_Type,
          Post'Class => ">="'Result.Kind = Bool_Type;
   function "<=" (Left : Tensor; Right : Element) return Tensor is abstract
     with Pre'Class  => Left.Kind /= Bool_Type,
          Post'Class => "<="'Result.Kind = Bool_Type;

   ----------------------------------------------------------------------------

   function "=" (Left : Element; Right : Tensor) return Tensor is abstract
     with Post'Class => "="'Result.Kind = Bool_Type;
   function "/=" (Left : Element; Right : Tensor) return Tensor is abstract
     with Post'Class => "/="'Result.Kind = Bool_Type;

   function ">" (Left : Element; Right : Tensor) return Tensor is abstract
     with Pre'Class  => Right.Kind /= Bool_Type,
          Post'Class => ">"'Result.Kind = Bool_Type;
   function "<" (Left : Element; Right : Tensor) return Tensor is abstract
     with Pre'Class  => Right.Kind /= Bool_Type,
          Post'Class => "<"'Result.Kind = Bool_Type;

   function ">=" (Left : Element; Right : Tensor) return Tensor is abstract
     with Pre'Class  => Right.Kind /= Bool_Type,
          Post'Class => ">="'Result.Kind = Bool_Type;
   function "<=" (Left : Element; Right : Tensor) return Tensor is abstract
     with Pre'Class  => Right.Kind /= Bool_Type,
          Post'Class => "<="'Result.Kind = Bool_Type;

   ----------------------------------------------------------------------------

   overriding
   function "=" (Left, Right : Tensor) return Boolean is abstract
     with Pre'Class => Left.Shape = Right.Shape and Left.Kind = Right.Kind;

   function "=" (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class  => Left.Shape = Right.Shape and Left.Kind = Right.Kind,
          Post'Class => "="'Result.Kind = Bool_Type;
   function "/=" (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class  => Left.Shape = Right.Shape and Left.Kind = Right.Kind,
          Post'Class => "/="'Result.Kind = Bool_Type;

   function ">" (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class  => Left.Shape = Right.Shape,
          Post'Class => ">"'Result.Kind = Bool_Type;
   function "<" (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class  => Left.Shape = Right.Shape,
          Post'Class => "<"'Result.Kind = Bool_Type;

   function ">=" (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class  => Left.Shape = Right.Shape,
          Post'Class => ">="'Result.Kind = Bool_Type;
   function "<=" (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class  => Left.Shape = Right.Shape,
          Post'Class => "<="'Result.Kind = Bool_Type;

   ----------------------------------------------------------------------------

   function Any_True (Object : Tensor; Dimension : Tensor_Dimension) return Tensor is abstract
     with Pre'Class => Object.Kind = Bool_Type
                         and Dimension <= Object.Dimensions
                         and Object.Dimensions > 1;

   function Any_True (Object : Tensor) return Boolean is abstract
     with Pre'Class => Object.Kind = Bool_Type;

   function All_True (Object : Tensor; Dimension : Tensor_Dimension) return Tensor is abstract
     with Pre'Class => Object.Kind = Bool_Type
                         and Dimension <= Object.Dimensions
                         and Object.Dimensions > 1;

   function All_True (Object : Tensor) return Boolean is abstract
     with Pre'Class => Object.Kind = Bool_Type;

private

   function Add (Left, Right : Tensor_Shape; Dimension : Tensor_Dimension) return Tensor_Shape;

end Orka.Numerics.Tensors;