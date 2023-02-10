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
private with Ada.Numerics.Generic_Elementary_Functions;

with Ada.Assertions;
with Ada.Numerics;

generic
   type Element_Type is digits <>;
package Orka.Numerics.Tensors is
   pragma Preelaborate;

   subtype Element is Element_Type;

   function Square_Root (Value : Element) return Element;

   type Element_Array is array (Positive range <>) of Element_Type;

   type Boolean_Array is array (Positive range <>) of Boolean;

   type Data_Type is (Float_Type, Int_Type, Bool_Type);

   type Tensor_Axis is range 1 .. 4;
   --  TODO Fix pre/post-conditions of matrix operations for tensors with 3 or 4 axes

   type Tensor_Shape is array (Tensor_Axis range <>) of Natural
     with Default_Component_Value => 0;
   --  The shape of a tensor gives the number of dimensions on each axis
   --
   --  Axes:
   --  1:               (rows)
   --  2:               (rows, columns)
   --  3:        (depth, rows, columns)
   --  4: (other, depth, rows, columns)

   function Elements (Shape : Tensor_Shape) return Natural;

   function Image (Shape : Tensor_Shape) return String;

   function Trim (Value : Natural) return String;

   function Is_Equal
     (Left, Right : Tensor_Shape;
      Except      : Tensor_Axis) return Boolean
   is (for all D in Left'Range => D = Except or Left (D) = Right (D));

   subtype Index_Type is Positive;

   type Tensor_Index is array (Tensor_Axis range <>) of Index_Type
     with Default_Component_Value => Index_Type'First;

   function Image (Index : Tensor_Index) return String;

   type Range_Type is record
      Start, Stop : Index_Type := Index_Type'First;
   end record
     with Dynamic_Predicate => Range_Type.Start <= Range_Type.Stop
       or else raise Constraint_Error with
         "Range start (" & Trim (Range_Type.Start) & ") > stop (" & Trim (Range_Type.Stop) & ")";

   type Tensor_Range is array (Tensor_Axis range <>) of Range_Type;

   function Shape (Index : Tensor_Range) return Tensor_Shape
     with Post => Index'Length = Shape'Result'Length
                    and (for all D in Index'Range =>
                           Index (D).Stop - Index (D).Start + 1 = Shape'Result (D));

   ----------------------------------------------------------------------------

   type Tensor is interface
     with Constant_Indexing => Get;

   function Is_Materialized (Object : Tensor) return Boolean is abstract;

   procedure Materialize (Object : in out Tensor) is abstract
     with Post'Class => Object.Is_Materialized;

   function Kind (Object : Tensor) return Data_Type is abstract;

   function Get (Object : Tensor; Index : Index_Type) return Element is abstract
     with Pre'Class => Object.Kind = Float_Type and Object.Axes = 1;
   --  Return the value of a vector

   function Get (Object : Tensor; Index : Index_Type) return Boolean is abstract
     with Pre'Class => Object.Kind = Bool_Type and Object.Axes = 1;
   --  Return the value of a boolean vector

   function Get (Object : Tensor; Index : Index_Type) return Tensor is abstract
     with Pre'Class  => Object.Axes = 2,
          Post'Class => Get'Result.Axes = 1 and Get'Result.Rows = Object.Columns;
   --  Return the row of a matrix as a vector

   function Get (Object : Tensor; Index : Tensor_Index) return Element is abstract
     with Pre'Class => Object.Kind = Float_Type and Object.Axes = Index'Length;
   --  Return the value of a matrix

   function Get (Object : Tensor; Index : Tensor_Index) return Boolean is abstract
     with Pre'Class => Object.Kind = Bool_Type and Object.Axes = Index'Length;
   --  Return the value of a boolean matrix

   function Get (Object : Tensor; Index : Range_Type) return Tensor is abstract
     with Post'Class => Object.Axes = Get'Result.Axes;

   function Get (Object : Tensor; Index : Tensor_Range) return Tensor is abstract
     with Pre'Class  => Index'Length <= Object.Axes,
          Post'Class => Get'Result.Axes in 1 | Index'Length;

   function Get (Object : Tensor; Index : Tensor) return Tensor is abstract
     with Pre'Class  => Index.Kind = Bool_Type and Index.Axes in 1 .. Object.Axes,
          Post'Class => Get'Result.Axes = 1 and Get'Result.Kind = Object.Kind;

   ----------------------------------------------------------------------------

   procedure Set (Object : in out Tensor; Index : Index_Type; Value : Element) is abstract
     with Pre'Class => Object.Axes = 1 and Object.Kind = Float_Type;

   procedure Set (Object : in out Tensor; Index : Index_Type; Value : Boolean) is abstract
     with Pre'Class => Object.Axes = 1 and Object.Kind = Bool_Type;

   procedure Set (Object : in out Tensor; Index : Tensor_Index; Value : Element) is abstract
     with Pre'Class => Object.Kind = Float_Type;

   procedure Set (Object : in out Tensor; Index : Tensor_Index; Value : Boolean) is abstract
     with Pre'Class => Object.Kind = Bool_Type;

   procedure Set
     (Object : in out Tensor;
      Index  : Index_Type;
      Value  : Tensor) is abstract
   with Pre'Class =>
     (case Object.Axes is
        when 1 => raise Ada.Assertions.Assertion_Error,
        when 2 => Value.Axes = 1 and then Value.Rows = Object.Columns,
        when 3 => Value.Axes = 2 and then Value.Shape = Object.Shape (2 .. 3),
        when 4 => Value.Axes = 3 and then Value.Shape = Object.Shape (2 .. 4));

   procedure Set
     (Object : in out Tensor;
      Index  : Range_Type;
      Value  : Tensor) is abstract
   with Pre'Class => Object.Axes = Value.Axes and then Is_Equal (Object.Shape, Value.Shape, 1);

   procedure Set
     (Object : in out Tensor;
      Index  : Tensor_Range;
      Value  : Tensor) is abstract
   with Pre'Class => Index'Length <= Object.Axes
                       and Index'Length = Value.Axes;

   ----------------------------------------------------------------------------

   function Image (Object : Tensor) return String is abstract;

   function Shape (Object : Tensor) return Tensor_Shape is abstract;

   function Axes (Object : Tensor) return Tensor_Axis is abstract;

   function Rows    (Object : Tensor'Class) return Natural is
     (Object.Shape (if Object.Axes = 1 then 1 else Object.Axes - 1));

   function Columns (Object : Tensor'Class) return Natural is (Object.Shape (Object.Axes))
     with Pre => Object.Axes >= 2;

   function Depth   (Object : Tensor'Class) return Natural is (Object.Shape (Object.Axes - 2))
     with Pre => Object.Axes >= 3;

   function Elements (Object : Tensor) return Natural is abstract;

   function Is_Square (Object : Tensor'Class) return Boolean is
     (Object.Axes = 2 and then Object.Rows = Object.Columns);

   ----------------------------------------------------------------------------

   function Same_Shape (Left, Right : Tensor'Class) return Boolean is
     (Left.Shape = Right.Shape or else raise Ada.Assertions.Assertion_Error with
        "Shape " & Image (Left.Shape) & " /= " & Image (Right.Shape));

   function Same_Kind (Left, Right : Tensor'Class) return Boolean is
     (Left.Kind = Right.Kind or else raise Ada.Assertions.Assertion_Error with
        "Kind " & Left.Kind'Image & " /= " & Right.Kind'Image);

   ----------------------------------------------------------------------------
   --                              Constructors                              --
   ----------------------------------------------------------------------------

   function Empty (Shape : Tensor_Shape) return Tensor is abstract;
   --  Return a tensor of the given shape without initialized elements

   function Fill (Shape : Tensor_Shape; Value : Element) return Tensor is abstract
     with Post'Class => Fill'Result.Kind = Float_Type and
                        Fill'Result.Shape = Shape;

   function Zeros (Elements : Positive) return Tensor is abstract
     with Post'Class => Zeros'Result.Kind = Float_Type and
                        Zeros'Result.Axes = 1 and
                        Zeros'Result.Elements = Elements;
   --  Return a tensor filled with zeros

   function Zeros (Shape : Tensor_Shape) return Tensor is abstract
     with Post'Class => Zeros'Result.Kind = Float_Type and
                        Zeros'Result.Shape = Shape;
   --  Return a tensor filled with zeros

   function Ones (Elements : Positive) return Tensor is abstract
     with Post'Class => Ones'Result.Kind = Float_Type and
                        Ones'Result.Axes = 1 and
                        Ones'Result.Elements = Elements;
   --  Return a tensor filled with ones

   function Ones (Shape : Tensor_Shape) return Tensor is abstract
     with Post'Class => Ones'Result.Kind = Float_Type and
                        Ones'Result.Shape = Shape;
   --  Return a tensor filled with ones

   function To_Tensor (Elements : Element_Array; Shape : Tensor_Shape) return Tensor is abstract
     with Pre'Class  => Elements'Length = Tensors.Elements (Shape),
          Post'Class => To_Tensor'Result.Kind = Float_Type and
                        To_Tensor'Result.Shape = Shape and
                        To_Tensor'Result.Elements = Elements'Length;

   function To_Tensor (Elements : Element_Array) return Tensor is abstract
     with Post'Class => To_Tensor'Result.Kind = Float_Type and
                        To_Tensor'Result.Axes = 1 and
                        To_Tensor'Result.Elements = Elements'Length;

   function To_Boolean_Tensor
     (Booleans : Boolean_Array;
      Shape    : Tensor_Shape) return Tensor is abstract
   with Pre'Class  => Booleans'Length = Elements (Shape),
        Post'Class => To_Boolean_Tensor'Result.Kind = Bool_Type and
                      To_Boolean_Tensor'Result.Shape = Shape;

   function To_Boolean_Tensor (Booleans : Boolean_Array) return Tensor is abstract
     with Post'Class => To_Boolean_Tensor'Result.Kind = Bool_Type;

   ----------------------------------------------------------------------------

   function Array_Range (Stop : Element) return Tensor is abstract
     with Post'Class => Array_Range'Result.Kind = Float_Type and
                        Array_Range'Result.Axes = 1;

   function Array_Range (Start, Stop : Element; Step : Element := 1.0) return Tensor is abstract
     with Pre'Class  => Start < Stop and Step > 0.0,
          Post'Class => Array_Range'Result.Kind = Float_Type and
                        Array_Range'Result.Axes = 1;

   type Interval_Kind is (Closed, Half_Open);

   function Linear_Space
     (Start, Stop : Element;
      Count       : Positive;
      Interval    : Interval_Kind := Closed) return Tensor is abstract
   with Post'Class => Linear_Space'Result.Axes = 1
                        and Linear_Space'Result.Kind = Float_Type
                        and Linear_Space'Result.Elements = Count;
   --  Return a 1D tensor containing numbers in a linear scale in the
   --  interval [start, stop] when interval is closed or [start, stop) when half open.

   function Log_Space
     (Start, Stop : Element;
      Count       : Positive;
      Interval    : Interval_Kind := Closed;
      Base        : Element := 10.0) return Tensor is abstract
   with Post'Class => Log_Space'Result.Axes = 1
                        and Log_Space'Result.Kind = Float_Type
                        and Log_Space'Result.Elements = Count;
   --  Return a 1D tensor containing numbers in a logarithmic scale in
   --  the interval [base**start, base**stop] when interval is closed
   --  or [base**start, base**stop) when half open.

   function Geometric_Space
     (Start, Stop : Element;
      Count       : Positive;
      Interval    : Interval_Kind := Closed;
      Base        : Element := 10.0) return Tensor is abstract
   with Post'Class => Geometric_Space'Result.Axes = 1
                        and Geometric_Space'Result.Kind = Float_Type
                        and Geometric_Space'Result.Elements = Count;
   --  Return a 1D tensor containing numbers in a logarithmic scale in
   --  the interval [start, stop] when interval is closed
   --  or [start, stop) when half open.

   ----------------------------------------------------------------------------

   function Identity (Size : Positive; Offset : Integer := 0) return Tensor is abstract
     with Post'Class => Identity'Result.Axes = 2
                          and Identity'Result.Kind = Float_Type
                          and Identity'Result.Rows = Size
                          and Identity'Result.Columns = Size;
   --  Return a tensor with ones on the diagonal (main when Offset = 0)
   --  and zeros everywhere else

   function Identity (Rows, Columns : Positive; Offset : Integer := 0) return Tensor is abstract
     with Post'Class => Identity'Result.Axes = 2
                          and Identity'Result.Kind = Float_Type
                          and Identity'Result.Rows = Rows
                          and Identity'Result.Columns = Columns;
   --  Return a tensor with ones on the diagonal (main when Offset = 0)
   --  and zeros everywhere else

   function Upper_Triangular (Object : Tensor; Offset : Integer := 0) return Tensor is abstract
     with Pre'Class  => Object.Axes = 2,
          Post'Class => Upper_Triangular'Result.Kind = Object.Kind and
                        Upper_Triangular'Result.Axes = 2;
   --  Return the upper triangular part of the matrix with zeros in the
   --  lower triangular part
   --
   --  Offset specifies the diagonal (main diagonal when Offset = 0) that acts
   --  as the boundary between the lower and upper triangular parts, and is not
   --  zeroes.
   --
   --  Offset < 0 moves this diagonal downward and Offset > 0 moves it upward.
   --  Thus the main diagonal will be zeroes when Offset > 0.

   function Main_Diagonal (Object : Tensor; Offset : Integer := 0) return Tensor is abstract
     with Pre'Class  => Object.Axes = 2,
          Post'Class => Main_Diagonal'Result.Kind = Object.Kind and
                        Main_Diagonal'Result.Axes = 1;
   --  Return a 1D tensor filled with the elements of the diagonal (main
   --  when Offset = 0) of the given tensor

   function Diagonal (Elements : Element_Array; Offset : Integer := 0) return Tensor is abstract
     with Post'Class => Is_Square (Diagonal'Result)
                          and Diagonal'Result.Kind = Float_Type
                          and Diagonal'Result.Elements = Elements'Length ** 2;
   --  Return a 2D tensor filled with the given elements on the diagonal
   --  (main when Offset = 0) and zeros everywhere else

   function Diagonal (Elements : Tensor; Offset : Integer := 0) return Tensor is abstract
     with Pre'Class  => Elements.Axes = 1,
          Post'Class => Is_Square (Diagonal'Result)
                          and Diagonal'Result.Kind = Elements.Kind
                          and Diagonal'Result.Elements = Elements.Elements ** 2;
   --  Return a 2D tensor filled with the given elements on the diagonal
   --  (main when Offset = 0) and zeros everywhere else

   function Trace (Object : Tensor; Offset : Integer := 0) return Element is abstract
     with Pre'Class => Object.Axes = 2;
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
          Post'Class => Reshape'Result.Kind = Object.Kind and Reshape'Result.Shape = Shape;

   function Reshape (Object : Tensor; Elements : Positive) return Tensor is abstract
     with Pre'Class  => Object.Elements = Elements,
          Post'Class => Reshape'Result.Kind = Object.Kind and Reshape'Result.Axes = 1;

   function Flatten (Object : Tensor) return Tensor is abstract
     with Post'Class => Flatten'Result.Kind = Object.Kind and Flatten'Result.Axes = 1;

   function Concatenate
     (Left, Right : Tensor;
      Axis   : Tensor_Axis) return Tensor is abstract
   with Pre'Class  => Left.Axes = Right.Axes and
                      Left.Kind = Right.Kind and
                      Axis <= Left.Axes and
                      Is_Equal (Left.Shape, Right.Shape, Axis) and
                      (Left.Elements > 0 or Right.Elements > 0),
        Post'Class => Concatenate'Result.Axes = Left.Axes and then
                      Concatenate'Result.Shape (Axis) = Left.Shape (Axis) + Right.Shape (Axis);
   --  Return the concatenation of the two tensors in the given Axis

   function "&" (Left, Right : Tensor) return Tensor is abstract;
   --  Return the concatenation of the two tensors in the first Axis

   ----------------------------------------------------------------------------
   --                            Matrix operations                           --
   ----------------------------------------------------------------------------

   function "*" (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class => (if Left.Axes > 1 then
                          Left.Columns = Right.Rows
                        else
                          (Right.Axes > 1 and Left.Rows = Right.Rows))
                         or else raise Constraint_Error with
                           "Cannot multiply matrices" &
                           " (left = " & Image (Left.Shape) & " and " &
                           "right = " & Image (Right.Shape) & ")",
          Post'Class => "*"'Result.Axes = Right.Axes;
   --  Perform matrix multiplication on two matrices (the right matrix
   --  can be a column vector) or a row vector and a matrix
   --
   --  Left is a row vector if it is 1-D.

   function "*" (Left, Right : Tensor) return Element is abstract
     with Pre'Class => (Left.Axes = 1 and Right.Axes = 1 and
                       Left.Shape = Right.Shape) or else
                         raise Constraint_Error with
                           "Tensors must be vectors with same shape" &
                           " (left = " & Image (Left.Shape) & " and " &
                           "right = " & Image (Right.Shape) & ")";
   --  Return the inner or dot product of two vectors (1-D tensors)

   function "**" (Left : Tensor; Right : Integer) return Tensor is abstract
     with Pre'Class => Is_Square (Left);

   function Outer (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class  => Left.Axes = 1 and Right.Axes = 1,
          Post'Class => Outer'Result.Axes = 2
                          and Outer'Result.Shape = (Left.Elements, Right.Elements);

   function Inverse (Object : Tensor) return Tensor is abstract
     with Pre'Class  => Is_Square (Object),
          Post'Class => Inverse'Result.Axes = 2;
   --  Return the inverse of a nonsingular matrix
   --
   --  Raises a Singular_Matrix exception if the matrix is singular / noninvertible.

   function Transpose (Object : Tensor) return Tensor is abstract
     with Pre'Class  => Object.Axes = 2,
          Post'Class => Transpose'Result.Axes = 2
                          and Transpose'Result.Rows = Object.Columns
                          and Transpose'Result.Columns = Object.Rows;

   type Solution_Kind is (None, Unique, Infinite);

   function Solve (A, B : Tensor; Solution : out Solution_Kind) return Tensor is abstract
     with Pre'Class  => A.Axes = 2 and A.Rows = B.Rows,
          Post'Class => Solve'Result.Shape = B.Shape;
   --  Solve Ax = b for x for each column b in B by applying
   --  Gauss-Jordan elimination to convert A to its reduced row echelon form
   --
   --  B can be a vector or a matrix.

   type Triangular_Form is (Upper, Lower);

   function Solve (A, B : Tensor; Form : Triangular_Form) return Tensor is abstract
     with Pre'Class  => Is_Square (A) and A.Rows = B.Rows,
          Post'Class => Solve'Result.Shape = B.Shape;
   --  Solve Ax = b for x for each column b in B by applying
   --  Gauss-Jordan elimination to convert A to its reduced row echelon form
   --  using either only back-substitution or forward-substitution
   --
   --  A must be either lower or upper triangular. If A does not
   --  have this form, then either the function Solve with the parameter
   --  Solution or the function Least_Squares must be called.

   function Divide_By (B, A : Tensor) return Tensor is abstract
     with Pre'Class  => A.Axes = 2 and A.Rows >= A.Columns and
                        B.Axes = 2 and A.Columns = B.Columns,
          Post'Class => Divide_By'Result.Shape = (B.Rows, A.Rows);
   --  Solve xA = B for x and return x = B / A

   function Divide_By (B, A : Tensor; Form : Triangular_Form) return Tensor is abstract
     with Pre'Class  => Is_Square (A) and
                        B.Axes = 2 and A.Columns = B.Columns,
          Post'Class => Divide_By'Result.Shape = (B.Rows, A.Rows);

   --  TODO Verify behavior of Divide_By when A is underdetermined

   type QR_Factorization is interface;
   --  Q is orthogonal (Q^T * Q = I) and R is upper triangular

   type QR_Mode is (Complete, Reduced);

   type Matrix_Determinancy is (Overdetermined, Underdetermined, Unknown);

   function Determinancy (Object : QR_Factorization) return Matrix_Determinancy is abstract;

   function QR (Object : Tensor) return Tensor is abstract
     with Pre'Class  => Object.Axes = 2;
   --  Return the reduced upper triangular matrix R of the QR decomposition (A = Q * R)

   function QR
     (Object : Tensor;
      Mode   : QR_Mode := Reduced) return QR_Factorization'Class is abstract
   with Pre'Class  => Object.Axes = 2,
        Post'Class => QR'Result.Determinancy = Unknown;
   --  Return the Q and R matrices of the QR decomposition (A = Q * R)
   --
   --  Q is orthogonal (Q^T * Q = I) and R is upper triangular.

   function QR_For_Least_Squares (Object : Tensor) return QR_Factorization'Class is abstract
     with Pre'Class => Object.Axes = 2,
          Post'Class => QR_For_Least_Squares'Result.Determinancy /= Unknown;
   --  Return the QR decomposition of A if A is overdetermined (rows >= columns)
   --  or the decomposition of A^T if A is underdetermined (rows < columns)

   function Least_Squares (Object : QR_Factorization'Class; B : Tensor) return Tensor is abstract
     with Pre'Class  => Object.Determinancy /= Unknown,
          Post'Class => Is_Equal (Least_Squares'Result.Shape, B.Shape, 1);
   --  Solve Ax' = b' for x' for each b' that is the orthogonal projection of
   --  a corresponding column b in B by computing the least-squares solution x
   --  using the given QR decomposition of A (if A is overdetermined) or A^T
   --  (if A is underdetermined)

   function Least_Squares (A, B : Tensor) return Tensor is abstract
     with Pre'Class  => A.Axes = 2 and A.Rows = B.Rows,
          Post'Class => A.Columns = Least_Squares'Result.Rows
                          and Is_Equal (Least_Squares'Result.Shape, B.Shape, 1);
   --  Solve Ax' = b' for x' for each b' that is the orthogonal projection of
   --  a corresponding column b in B by computing the QR decomposition of A
   --  and then the least-squares solution x

   function Constrained_Least_Squares (A, B, C, D : Tensor) return Tensor is abstract
     with Pre'Class => (A.Axes = 2 and C.Axes = 2) and then
                         (A.Columns = C.Columns and
                          A.Rows = B.Rows and
                          C.Rows = D.Rows and
                          Is_Equal (B.Shape, D.Shape, 1)),
          Post'Class => A.Columns = Constrained_Least_Squares'Result.Rows
                          and Is_Equal (Constrained_Least_Squares'Result.Shape, B.Shape, 1);
   --  Solve Ax' = b' subject to Cx' = d for the least-squares solution x'
   --  for each b' that is the orthogonal projection of a corresponding
   --  column b in B
   --
   --  If A = I and b = 0 then the function returns the smallest x' for which Cx' = d.

   function Cholesky (Object : Tensor; Form : Triangular_Form := Lower) return Tensor is abstract
     with Pre'Class  => Is_Square (Object),
          Post'Class => Is_Square (Cholesky'Result);
   --  Return the lower triangular matrix L of the Cholesky decomposition
   --  of A (= L * L^T) or the upper triangular matrix U of A (= U^T * U)
   --  if A is symmetric positive definite
   --
   --  Positive definite means that for all x /= 0: x^T * A * x > 0.
   --
   --  Raises a Not_Positive_Definite_Matrix exception if A is not positive definite.

   type Update_Mode is (Update, Downdate);

   function Cholesky_Update
     (R, V : Tensor;
      Mode : Update_Mode) return Tensor is abstract
   with Pre'Class  => Is_Square (R) and V.Axes = 1,
        Post'Class => Is_Square (Cholesky_Update'Result);
   --  Return the rank 1 update or downdate of the given upper triangular matrix R
   --
   --  That is, the result D is equal to D^T*D = R^T*R +/- V*V^T:
   --
   --     A ----------------> R = Cholesky (A, Upper)
   --     |                         |
   --     v                         v
   --  A' = A +/- V*V^T ----> D = Cholesky(A', Upper) or Cholesky_Update (R, V, Update/Downdate)
   --
   --  It is much faster (O(n^2)) to compute D from R and V using function
   --  Cholesky_Update than to compute it using function Cholesky (O(n^3))
   --  and it makes sense when A is updated repeatedly.

   --  TODO Add Schur, SVD

   ----------------------------------------------------------------------------
   --                            Vector operations                           --
   ----------------------------------------------------------------------------

   function Norm (Object : Tensor) return Element is abstract
     with Pre'Class  => Object.Axes = 1,
          Post'Class => Norm'Result >= 0.0;
   --  Return the norm or magnitude of the vector

   function Normalize (Object : Tensor) return Tensor is abstract
     with Pre'Class  => Object.Axes = 1,
          Post'Class => Normalize'Result.Axes = 1;
   --  Return the normalized vector
   --
   --  The magnitude or norm of a unit vector is 1.0.

   function Standardize (Object : Tensor) return Tensor is abstract
     with Pre'Class  => Object.Axes = 1,
          Post'Class => Standardize'Result.Axes = 1;
   --  Return the standardized version of the vector
   --
   --  A standardized vector has a mean of 0.0 and a standard deviation
   --  of 1.0. If all elements of the tensor are equal, then its standard
   --  deviation is 0.0, which means the returned vector is the zero vector.

   subtype Correlation_Element is Element range -1.0 .. 1.0;

   function Correlation_Coefficient (Left, Right : Tensor) return Correlation_Element is abstract
     with Pre'Class  => Left.Axes = 1 and Right.Axes = 1
                          and Left.Shape = Right.Shape;
   --  Return the correlation coefficient of two vectors
   --
   --  The coefficient is 0.0 if the two vectors are uncorrelated
   --  or if one or both vectors has all equal elements. 1.0 is returned
   --  if the two vectors are aligned, and -1.0 if negatively aligned.

   ----------------------------------------------------------------------------
   --                         Element-wise operations                        --
   ----------------------------------------------------------------------------

   --  TODO Add Pre'Class => Left.Kind /= Bool_Type and Left.Kind = Right.Kind

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
   --  If x = 0 and y = 0 then result is 1, else undefined if (x = 0 and y < 0) or x < 0

   function "**" (Left : Tensor; Right : Element) return Tensor is abstract;
   --  Return a tensor containing the elements in the left tensor raised to
   --  the right element
   --
   --  Following rules are applied:
   --
   --  x^0 = 1 (for any x)
   --  x^1 = x (for any x)
   --  0^0 = 1

   function "**" (Left : Element; Right : Tensor) return Tensor is abstract;
   --  Return a tensor containing the left element raised to the power of
   --  the elements in the right tensor
   --
   --  Following rules are applied:
   --
   --  1^y = 1 (for any y)
   --  0^0 = 1

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

   function Min (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class => Left.Shape = Right.Shape;

   function Max (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class => Left.Shape = Right.Shape;

   function Min (Left : Element; Right : Tensor) return Tensor is abstract;
   function Min (Left : Tensor; Right : Element) return Tensor is abstract;

   function Max (Left : Element; Right : Tensor) return Tensor is abstract;
   function Max (Left : Tensor; Right : Element) return Tensor is abstract;

   function Sqrt (Object : Tensor) return Tensor is abstract;
   --  Return a tensor containing the square roots of the given tensor
   --
   --  An element is undefined if x < 0.

   function Ceil (Object : Tensor) return Tensor is abstract;
   function Floor (Object : Tensor) return Tensor is abstract;
   function Round (Object : Tensor) return Tensor is abstract;
   function Truncate (Object : Tensor) return Tensor is abstract;

   function Exp (Object : Tensor) return Tensor is abstract;

   function Log (Object : Tensor) return Tensor is abstract;
   function Log10 (Object : Tensor) return Tensor is abstract;
   function Log2 (Object : Tensor) return Tensor is abstract;
   --  Values must be > 0.0

   ----------------------------------------------------------------------------
   --                              Trigonometry                              --
   ----------------------------------------------------------------------------

   function Sin (Object : Tensor) return Tensor is abstract;
   function Cos (Object : Tensor) return Tensor is abstract;
   function Tan (Object : Tensor) return Tensor is abstract;

   function Arcsin (Object : Tensor) return Tensor is abstract;
   --  Values must be in -1.0 .. 1.0

   function Arccos (Object : Tensor) return Tensor is abstract;
   --  Values must be in -1.0 .. 1.0

   function Arctan (Left, Right : Tensor) return Tensor is abstract;
   --  Values in Left > 0.0 or Right > 0.0

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

   function Number (Value : Element) return Expression is abstract;

   --  TODO Add function Cumulative

   function Reduce_Associative
     (Object    : Tensor;
      Subject   : Expression'Class;
      Initial   : Element) return Element is abstract
   with Pre'Class => Object.Kind /= Bool_Type;

   function Reduce_Associative
     (Object  : Tensor;
      Subject : Expression'Class;
      Initial : Element;
      Axis    : Tensor_Axis) return Tensor is abstract
   with Pre'Class  => Object.Kind /= Bool_Type and Axis <= Object.Axes,
        Post'Class => Reduce_Associative'Result.Axes = Object.Axes - 1;

   function Reduce
     (Object    : Tensor;
      Subject   : Expression'Class;
      Initial   : Element) return Element is abstract
   with Pre'Class => Object.Kind /= Bool_Type;

   function Reduce
     (Object  : Tensor;
      Subject : Expression'Class;
      Initial : Element;
      Axis    : Tensor_Axis) return Tensor is abstract
   with Pre'Class  => Object.Kind /= Bool_Type and Axis <= Object.Axes,
        Post'Class => Reduce'Result.Axes = Object.Axes - 1;

   function Sum (Object : Tensor) return Element is abstract;

   function Sum (Object : Tensor; Axis : Tensor_Axis) return Tensor is abstract;

   function Product (Object : Tensor) return Element is abstract;

   function Product (Object : Tensor; Axis : Tensor_Axis) return Tensor is abstract;

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

   function Min (Object : Tensor; Axis : Tensor_Axis) return Tensor is abstract
     with Pre'Class  => Axis <= Object.Axes,
          Post'Class => Min'Result.Axes = Object.Axes - 1;

   function Max (Object : Tensor; Axis : Tensor_Axis) return Tensor is abstract
     with Pre'Class  => Axis <= Object.Axes,
          Post'Class => Max'Result.Axes = Object.Axes - 1;

   function Quantile
     (Object : Tensor;
      P      : Probability;
      Axis   : Tensor_Axis) return Tensor is abstract
   with Pre'Class  => Axis <= Object.Axes,
        Post'Class => Quantile'Result.Axes = Object.Axes - 1;

   function Median (Object : Tensor; Axis : Tensor_Axis) return Tensor is abstract
     with Pre'Class  => Axis <= Object.Axes,
          Post'Class => Median'Result.Axes = Object.Axes - 1;

   function Mean (Object : Tensor; Axis : Tensor_Axis) return Tensor is abstract
     with Pre'Class  => Axis <= Object.Axes,
          Post'Class => Mean'Result.Axes = Object.Axes - 1;

   function Variance
     (Object : Tensor;
      Axis   : Tensor_Axis;
      Offset : Natural := 0) return Tensor is abstract
   with Pre'Class  => Axis <= Object.Axes,
        Post'Class => Variance'Result.Axes = Object.Axes - 1;

   function Standard_Deviation
     (Object : Tensor;
      Axis   : Tensor_Axis;
      Offset : Natural := 0) return Tensor is abstract
   with Pre'Class  => Axis <= Object.Axes,
        Post'Class => Standard_Deviation'Result.Axes = Object.Axes - 1;

   ----------------------------------------------------------------------------
   --                                Logical                                 --
   ----------------------------------------------------------------------------

   function And_Not (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class  => Left.Kind = Bool_Type and Right.Kind = Bool_Type
                          and Left.Shape = Right.Shape,
          Post'Class => And_Not'Result.Kind = Bool_Type;
   --  Return a tensor equal to (not Left) and Right

   function "and" (Left, Right : Tensor) return Tensor is abstract
     with Pre'Class  => Right.Kind = Bool_Type
                          and Left.Shape = Right.Shape,
          Post'Class => "and"'Result.Kind = Left.Kind;

   function "and" (Left : Element; Right : Tensor) return Tensor is abstract
     with Pre'Class  => Right.Kind = Bool_Type,
          Post'Class => "and"'Result.Kind = Float_Type;
   --  Return a tensor where each position is the given element
   --  if the corresponding boolean from the boolean tensor is True
   --  and 0.0 if False

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
--     with Pre'Class  => Left.Shape = Right.Shape and Left.Kind = Right.Kind,
     with Pre'Class  => Same_Shape (Left, Right) and Same_Kind (Left, Right),
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

   function All_Close
     (Left, Right        : Tensor;
      Relative_Tolerance : Element := 1.0e-05;
      Absolute_Tolerance : Element := Element_Type'Model_Epsilon) return Boolean is abstract
   with Pre'Class => Left.Shape = Right.Shape and Left.Kind = Right.Kind;

   function Any_True (Object : Tensor; Axis : Tensor_Axis) return Tensor is abstract
     with Pre'Class  => Object.Kind = Bool_Type
                          and Axis <= Object.Axes
                          and Object.Axes > 1,
          Post'Class => Any_True'Result.Axes = Object.Axes - 1;

   function Any_True (Object : Tensor) return Boolean is abstract
     with Pre'Class => Object.Kind = Bool_Type;

   function All_True (Object : Tensor; Axis : Tensor_Axis) return Tensor is abstract
     with Pre'Class  => Object.Kind = Bool_Type
                          and Axis <= Object.Axes
                          and Object.Axes > 1,
          Post'Class => All_True'Result.Axes = Object.Axes - 1;

   function All_True (Object : Tensor) return Boolean is abstract
     with Pre'Class => Object.Kind = Bool_Type;

   ----------------------------------------------------------------------------

   function Random_Uniform (Shape : Tensor_Shape) return Tensor is abstract;

   generic
      type Random_Tensor (<>) is new Tensor with private;
   package Generic_Random is

      function Uniform (Shape : Tensor_Shape) return Random_Tensor renames Random_Uniform;
      --  Return a tensor with elements from a uniform distribution in [0.0, 1.0)
      --
      --  Mean is 0.5 * (A + B) and variance is 1.0 / 12.0 * (B - A)^2.
      --
      --  Use A + Uniform (Shape) * (B - A) for the uniform distribution A .. B.

      function Normal (Shape : Tensor_Shape) return Random_Tensor;
      --  Return a tensor with elements from the standard normal distribution
      --
      --  Mean is 0.0 and variance is 1.0.
      --
      --  Use Mu + Normal (Shape) * Sigma for the distribution N (Mu, Sigma^2)
      --  where Mu is the mean and Sigma is the standard deviation (Sigma^2 is
      --  the variance).

      function Binomial (Shape : Tensor_Shape; N : Positive; P : Probability) return Random_Tensor;
      --  Return a tensor where each element is the number of successful
      --  trials (0 .. N) with each trial having a probability of success of P
      --
      --  Mean is N * P and variance is N * P * (1.0 - P)

      function Geometric (Shape : Tensor_Shape; P : Probability) return Random_Tensor
        with Pre => P > 0.0;
      --  Return a tensor with a geometric distribution, modeling the
      --  number of failures
      --
      --  Mean is (1.0 - P) / P and variance is (1.0 - P) / P**2.

      function Exponential (Shape : Tensor_Shape; Lambda : Element) return Random_Tensor is
        ((-1.0 / Lambda) * Log (Uniform (Shape) + Element'Model_Small))
      with Pre => Lambda > 0.0;
      --  Return a tensor with an exponential distribution
      --
      --  Mean is 1.0 / Lambda and variance is 1.0 / Lambda**2.

      function Pareto (Shape : Tensor_Shape; Xm, Alpha : Element) return Random_Tensor is
        (Xm / ((1.0 - Uniform (Shape)) ** (1.0 / Alpha)))
      with Pre => Xm > 0.0 and Alpha > 0.0;
      --  Return a tensor with a Pareto distribution
      --
      --  Mean is (Alpha * Xm) / (Alpha - 1.0) for Alpha > 1.0 and infinite for Alpha <= 1.0
      --  and variance is (Xm**2 * Alpha) / ((Alpha - 1.0)**2 * (Alpha - 2.0)) for Alpha > 2.0
      --  and infinite for Alpha < 2.0.
      --
      --  If X ~ Exp (Alpha) then Y = Xm * e^X ~ Pareto(Xm, Alpha):
      --
      --    Pareto'Result = (Xm * Ada.Numerics.e ** Exponential (Shape, Alpha))

      function Laplace (Shape : Tensor_Shape; Mean, B : Element) return Random_Tensor is
        (Mean + (Exponential (Shape, 1.0 / B) - Exponential (Shape, 1.0 / B)))
      with Pre => B > 0.0;
      --  Return a tensor that has a Laplace distribution
      --
      --  Mean is the given mean and variance is 2.0 * B^2.

      function Rayleigh (Shape : Tensor_Shape; Sigma : Element) return Random_Tensor is
        (Sigma * Sqrt (-2.0 * Log (Uniform (Shape) + Element'Model_Small)))
      with Pre => Sigma > 0.0;
      --  Return a tensor that has a Rayleigh distribution
      --
      --  Mean is Sigma * Sqrt (Pi / 2.0) and variance is (4.0 - Pi) / 2.0 * Sigma**2.
      --
      --  If X ~ Exp (Lambda) then Y = Sqrt (Exp (Lambda)) ~ Rayleigh(1 / Sqrt(2 * Lambda)):
      --
      --    Rayleigh'Result = (Sqrt (Exponential (Shape, 0.5 * (1.0 / Sigma)**2)))

      function Weibull (Shape : Tensor_Shape; K, Lambda : Element) return Random_Tensor is
        (Lambda * (-Log (Uniform (Shape) + Element'Model_Small))**(1.0 / K))
      with Pre => K > 0.0 and Lambda > 0.0;
      --  Return a tensor that has a Weibull distribution
      --
      --  Mean is Lambda * Gamma(1.0 + 1.0 / K) and variance is
      --  Lambda^2 * [Gamma(1.0 + 2.0 / K) - Gamma(1.0 + 1.0 / K)^2].

      function Poisson (Shape : Tensor_Shape; Lambda : Element) return Random_Tensor
        with Pre => Lambda > 0.0;
      --  Return a tensor that has the Poisson distribution
      --
      --  Mean and variance are both Lambda.

      function Gamma (Shape : Tensor_Shape; K, Theta : Element) return Random_Tensor
        with Pre => K >= 1.0 and Theta > 0.0;
      --  Return a tensor that has the gamma distribution
      --
      --  Mean is K * Theta and variance is K * Theta^2.
      --
      --  Note: 0.0 < K < 1.0 is currently not supported.

      function Beta (Shape : Tensor_Shape; Alpha, Beta : Element) return Random_Tensor
        with Pre => Alpha > 0.0 and Beta > 0.0;
      --  Return a tensor with a beta distribution
      --
      --  Mean is Alpha / (Alpha + Beta) and variance is
      --  (Alpha * Beta) / ((Alpha + Beta)^2 * (Alpha + Beta + 1.0)).

      function Chi_Squared (Shape : Tensor_Shape; K : Positive) return Random_Tensor is
        (Gamma (Shape, K => Element (K) / 2.0, Theta => 2.0))
      with Pre => K >= 2;
      --  Return a tensor that has the chi^2 distribution
      --
      --  Mean is K and variance is 2 * K.
      --
      --  Note: K = 1 is not supported because of a limitation of function Gamma.

      function Student_T (Shape : Tensor_Shape; V : Positive) return Random_Tensor is
        (Multiply (Normal (Shape),
                   Sqrt (Element (V) / (Chi_Squared (Shape, K => V) + Element'Model_Small))))
      with Pre => V >= 2;
      --  Return a tensor that has the Student's t-distribution
      --
      --  Mean is 0 and variance is V / (V - 2.0) for V > 2.0 or infinite if V = 2.0.
      --
      --  Note: V = 1 is not supported because of a limitation of function Chi_Squared.

      function Test_Statistic_T_Test (Data : Random_Tensor; True_Mean : Element) return Element;
      --  Return the test statistic of the one-sample t-test for the null
      --  hypothesis sample mean = True_Mean
      --
      --  A t-value near zero is evidence for the null hypothesis, while a large
      --  positive or negative value away from zero is evidence against it.
      --
      --  t-value >> 0: mean > True_Mean
      --  t-value << 0: mean < True_Mean.
      --
      --  The test statistic can be used to compute the probability of having a
      --  type I error (rejecting the null hypothesis when it is actually true):
      --
      --    Tensor      : Tensor'Class := Random.Student_T ((1 => Trials), V => Data.Elements - 1);
      --    Probability : Element      := Sum (1.0 and (Tensor >= abs T)) / Element (Trials);
      --
      --  If the probability is greater than some significance level (for
      --  example, 0.05) then there is a good chance of incorrectly rejecting
      --  the null hypothesis. Therefore, the null hypothesis should *not* be
      --  rejected. If the probability is less than the level, then it is
      --  unlikely to have a type I error and therefore you can safely reject
      --  the null hypothesis.

      function Threshold_T_Test
        (Data      : Random_Tensor;
         Level     : Probability) return Element
      with Pre => 0.0 < Level and Level <= 0.5;
      --  Return the threshold relative to a true mean for a given significance level
      --
      --  Add and subtract the result from some true mean to get the interval for
      --  which the null hypothesis (sample mean = true mean) is accepted.
      --
      --  A significance level of 0.1 corresponds with a confidence of 90 %
      --  and 0.05 corresponds with 95 %. A lower significance level (and thus
      --  higher confidence) will give a wider interval.
      --
      --  For a sample mean further away from the true mean, the null hypothesis
      --  is correctly rejected or incorrectly (type I error), but the type I error
      --  occurs only with a probability equal to the given significance level.
      --
      --  See https://en.wikipedia.org/wiki/Student%27s_t-distribution#Table_of_selected_values

   end Generic_Random;

   ----------------------------------------------------------------------------

   type Expression_Type (<>) is new Expression with private;

   overriding function "+" (Left, Right : Expression_Type) return Expression_Type;
   overriding function "-" (Left, Right : Expression_Type) return Expression_Type;
   overriding function "*" (Left, Right : Expression_Type) return Expression_Type;
   overriding function "/" (Left, Right : Expression_Type) return Expression_Type;

   overriding function Min (Left, Right : Expression_Type) return Expression_Type;
   overriding function Max (Left, Right : Expression_Type) return Expression_Type;

   overriding function "+" (Left : Element; Right : Expression_Type) return Expression_Type;
   overriding function "+" (Left : Expression_Type; Right : Element) return Expression_Type;

   overriding function "-" (Left : Element; Right : Expression_Type) return Expression_Type;
   overriding function "-" (Left : Expression_Type; Right : Element) return Expression_Type;

   overriding function "*" (Left : Element; Right : Expression_Type) return Expression_Type;
   overriding function "*" (Left : Expression_Type; Right : Element) return Expression_Type;

   overriding function "/" (Left : Element; Right : Expression_Type) return Expression_Type;
   overriding function "/" (Left : Expression_Type; Right : Element) return Expression_Type;

   overriding function "-" (Value : Expression_Type) return Expression_Type;

   overriding function "abs" (Value : Expression_Type) return Expression_Type;

   overriding function Sqrt (Value : Expression_Type) return Expression_Type;

   overriding function Min (Left : Element; Right : Expression_Type) return Expression_Type;
   overriding function Min (Left : Expression_Type; Right : Element) return Expression_Type;

   overriding function Max (Left : Element; Right : Expression_Type) return Expression_Type;
   overriding function Max (Left : Expression_Type; Right : Element) return Expression_Type;

   overriding function X return Expression_Type;
   overriding function Y return Expression_Type;

   overriding function Number (Value : Element) return Expression_Type;

private

   function Add (Left, Right : Tensor_Shape; Axis : Tensor_Axis) return Tensor_Shape;

   function To_Index (Index : Tensor_Index; Shape : Tensor_Shape) return Index_Type
     with Pre => Index'Length = Shape'Length
                   and then (for all D in Index'Range => Index (D) <= Shape (D));

   function Full_Range (Shape : Tensor_Shape; Index : Tensor_Range) return Tensor_Range;
   --  Return a copy of the given index with the missing Axes added
   --  from the given shape
   --
   --  For example, if Shape is 2-D and Index is 1-D, then 1 .. Shape (2)
   --  is used for the second Axis in the result.

   type Alignment is (Left, Right);

   function Full_Shape
     (Axes : Tensor_Axis;
      Shape      : Tensor_Shape;
      Justify    : Alignment) return Tensor_Shape;
   --  Return a shape padded at the beginning or end with 1's for missing Axes
   --
   --  For example, if Axes = 3 and Alignment = Right and Shape has 2 Axes,
   --  then the first Axis of the result is 1 and the last two Axes
   --  equal to the given shape.

   package EF is new Ada.Numerics.Generic_Elementary_Functions (Element_Type);

   function Square_Root (Value : Element) return Element renames EF.Sqrt;

   ----------------------------------------------------------------------------

   type Argument_Kind is (X, Y);

   type Binary_Operation_Kind is (Add, Subtract, Multiply, Divide, Min, Max);

   type Unary_Operation_Kind is (Minus, Absolute, Sqrt);

   type Expression_Type_Kind is (Argument, Number, Binary_Operation, Unary_Operation);

   package Expression_Holders is new Ada.Containers.Indefinite_Holders (Expression'Class);

   type Expression_Type (Kind : Expression_Type_Kind) is new Expression with record
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

   generic
      type Data_Type is private;

      with function Identity (Value : Element) return Data_Type;

      with function "+" (Left, Right : Data_Type) return Data_Type is <>;
      with function "-" (Left, Right : Data_Type) return Data_Type is <>;
      with function "*" (Left, Right : Data_Type) return Data_Type is <>;
      with function "/" (Left, Right : Data_Type) return Data_Type is <>;
      with function Min (Left, Right : Data_Type) return Data_Type is <>;
      with function Max (Left, Right : Data_Type) return Data_Type is <>;

      with function "-"   (Value : Data_Type) return Data_Type is <>;
      with function "abs" (Value : Data_Type) return Data_Type is <>;
      with function Sqrt  (Value : Data_Type) return Data_Type is <>;
   function Generic_Apply
     (Object      : Expression_Type;
      Left, Right : Data_Type) return Data_Type;

   Not_Implemented_Yet : exception;

end Orka.Numerics.Tensors;
