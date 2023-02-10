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

package body Orka.Numerics.Tensors.Operations is

   overriding function Get (Object : Tensor_Type; Index : Index_Type) return Element is
     (Object.Get ((1 => Index)));

   overriding function Get (Object : Tensor_Type; Index : Index_Type) return Boolean is
     (Object.Get ((1 => Index)));

   overriding function Get (Object : Tensor_Type; Index : Range_Type) return Tensor_Type is
     (case Object.Axes is
        when 1 => Object.Get (Tensor_Range'(1 => Index)),
        when 2 => Object.Get (Tensor_Range'(Index, (1, Object.Columns))),
        when 3 => Object.Get (Tensor_Range'(Index, (1, Object.Rows), (1, Object.Columns))),
        when 4 => Object.Get (Tensor_Range'(
                                Index, (1, Object.Depth), (1, Object.Rows), (1, Object.Columns)
                              )));

   overriding procedure Set
     (Object : in out Tensor_Type;
      Index  : Index_Type;
      Value  : Tensor_Type) is
   begin
      case Object.Axes is
         when 1 =>
            raise Program_Error;
         when 2 | 3 | 4 =>
            Object.Set (Tensor_Range'(1 => (Index, Index)), Value);
      end case;
   end Set;

   overriding procedure Set
     (Object : in out Tensor_Type;
      Index  : Range_Type;
      Value  : Tensor_Type) is
   begin
      case Object.Axes is
         when 1 =>
            Object.Set (Tensor_Range'(1 => Index), Value);
         when 2 =>
            Object.Set (Tensor_Range'(Index, (1, Object.Columns)), Value);
         when 3 =>
            Object.Set (Tensor_Range'(Index, (1, Object.Rows), (1, Object.Columns)), Value);
         when 4 =>
            Object.Set (Tensor_Range'(
                          Index, (1, Object.Depth), (1, Object.Rows), (1, Object.Columns)
                        ), Value);
      end case;
   end Set;

   overriding procedure Set (Object : in out Tensor_Type; Index : Index_Type; Value : Element) is
   begin
      Object.Set ((1 => Index), Value);
   end Set;

   overriding procedure Set (Object : in out Tensor_Type; Index : Index_Type; Value : Boolean) is
   begin
      Object.Set ((1 => Index), Value);
   end Set;

   ----------------------------------------------------------------------------
   --                              Constructors                              --
   ----------------------------------------------------------------------------

   overriding
   function Zeros (Shape : Tensor_Shape) return Tensor_Type is (Fill (Shape, 0.0));

   overriding
   function Zeros (Elements : Positive) return Tensor_Type is (Zeros ((1 => Elements)));

   overriding
   function Ones (Shape : Tensor_Shape) return Tensor_Type is (Fill (Shape, 1.0));

   overriding
   function Ones (Elements : Positive) return Tensor_Type is (Ones ((1 => Elements)));

   overriding
   function To_Tensor (Elements : Element_Array) return Tensor_Type is
     (To_Tensor (Elements, (1 => Elements'Length)));

   overriding
   function To_Boolean_Tensor (Booleans : Boolean_Array) return Tensor_Type is
     (To_Boolean_Tensor (Booleans, (1 => Booleans'Length)));

   overriding
   function Geometric_Space
     (Start, Stop : Element;
      Count       : Positive;
      Interval    : Interval_Kind := Closed;
      Base        : Element := 10.0) return Tensor_Type
   is (Log_Space (Start    => EF.Log (Start) / EF.Log (Base),
                  Stop     => EF.Log (Stop) / EF.Log (Base),
                  Count    => Count,
                  Interval => Interval,
                  Base     => Base));

   overriding
   function Array_Range (Start, Stop : Element; Step : Element := 1.0) return Tensor_Type is
     (Linear_Space (Start => Start,
                    Stop  => Stop,
                    Count => Positive'Max (1, Integer (Element'Ceiling ((Stop - Start) / Step))),
                    Interval => Half_Open));

   overriding
   function Array_Range (Stop : Element) return Tensor_Type is
     (Array_Range (Start => 0.0, Stop => Stop));

   overriding
   function Identity (Size : Positive; Offset : Integer := 0) return Tensor_Type is
     (Identity (Rows => Size, Columns => Size, Offset => Offset));

   overriding
   function Upper_Triangular (Object : Tensor_Type; Offset : Integer := 0) return Tensor_Type is
   begin
      return Result : Tensor_Type := Object do
         Make_Upper_Triangular (Result, Offset => Offset);
      end return;
   end Upper_Triangular;

   overriding
   function Trace (Object : Tensor_Type; Offset : Integer := 0) return Element is
     (Object.Main_Diagonal (Offset => Offset).Sum);

   overriding
   function Flatten (Object : Tensor_Type) return Tensor_Type is
     (Object.Reshape (Object.Elements));

   overriding
   function Reshape (Object : Tensor_Type; Elements : Positive) return Tensor_Type is
     (Object.Reshape ((1 => Elements)));

   overriding
   function "&" (Left, Right : Tensor_Type) return Tensor_Type is (Concatenate (Left, Right, 1));

   ----------------------------------------------------------------------------
   --                            Matrix operations                           --
   ----------------------------------------------------------------------------

   overriding function "**" (Left : Tensor_Type; Right : Integer) return Tensor_Type is
      function Matrix_Power (Left : Tensor_Type; Right : Natural) return Tensor_Type is
         Result : Tensor_Type := Identity (Size => Left.Rows);

         Log_2 : constant Element := EF.Log (2.0);

         Remaining : Natural := Right;
      begin
         while Remaining > 0 loop
            declare
               Doubling : Tensor_Type := Left;
               Count : constant Integer :=
                 Integer (Element'Floor (EF.Log (Element (Remaining)) / Log_2));
            begin
               for I in 1 .. Count loop
                  Doubling := Doubling * Doubling;
               end loop;
               Result    := Result * Doubling;
               Remaining := Remaining - 2 ** Count;
            end;
         end loop;

         return Result;
      end Matrix_Power;
   begin
      case Right is
         when 2 .. Integer'Last =>
            return Matrix_Power (Left, Right);
         when 1 =>
            return Left;
         when 0 =>
            return Identity (Size => Left.Rows);
         when -1 =>
            return Left.Inverse;
         when Integer'First .. -2 =>
            return Matrix_Power (Left.Inverse, abs Right);
      end case;
   end "**";

   overriding
   function Inverse (Object : Tensor_Type) return Tensor_Type is
      Size : constant Natural := Object.Rows;

      Solution : Solution_Kind := None;
   begin
      return Result : constant Tensor_Type := Solve (Object, Identity (Size), Solution) do
         if Solution /= Unique then
            raise Singular_Matrix;
         end if;
      end return;
   end Inverse;

   function Matrix_Solve (A, B : Tensor_Type; Solution : out Solution_Kind) return Tensor_Type is
      Ab : Tensor_Type := Concatenate (A, B, Axis => 2);

      Rows    : constant Natural := A.Rows;
      Columns : constant Natural := A.Columns;

      Columns_Ab : constant Natural := Ab.Columns;

      function Find_Largest_Pivot (Row_Index, Column_Index : Index_Type) return Index_Type is
         Pivot_Value : Element    := abs Ab.Get ((Row_Index, Column_Index));
         Pivot_Index : Index_Type := Row_Index;
      begin
         for Index in Row_Index .. Rows loop
            declare
               Value : constant Element := abs Ab.Get ((Index, Column_Index));
            begin
               if Value > Pivot_Value then
                  Pivot_Value := Value;
                  Pivot_Index := Index;
               end if;
            end;
         end loop;

         return Pivot_Index;
      end Find_Largest_Pivot;

      Pivots : array (0 .. Rows) of Natural := (others => 0);
   begin
      --  Forward phase: row reduce augmented matrix to echelon form

      --  Iterate over the columns and rows and find the row with the
      --  largest absolute pivot
      for Index in 1 .. Rows loop
         --  The pivot of the previous row is in the column to the left
         Pivots (Index) := Pivots (Index - 1) + 1;
         pragma Assert (Pivots (Index) <= Columns);

         declare
            Pivot_Index : Index_Type renames Pivots (Index);
            Row_Index   : Positive := Find_Largest_Pivot (Index, Pivot_Index);
         begin
            while Pivot_Index < Columns and then Ab.Get ((Row_Index, Pivot_Index)) = 0.0 loop
               Pivot_Index := Pivot_Index + 1;
               Row_Index   := Find_Largest_Pivot (Index, Pivot_Index);
            end loop;

            if Ab.Get ((Row_Index, Pivot_Index)) = 0.0 then
               Pivot_Index := 0;
               exit;
            end if;

            Swap_Rows (Ab, Index, Row_Index);

            Forward_Substitute (Ab, Index, Pivot_Index);

            --  Current pivot position is in the last column, all rows below it must be zero
            exit when Pivot_Index = Columns;
         end;
      end loop;

      --  Backward phase: row reduce augmented matrix to reduced echelon form

      for Index in reverse 1 .. Rows loop
         declare
            Pivot_Index : Index_Type renames Pivots (Index);
         begin
            if Pivot_Index > 0 then
               Back_Substitute (Ab, Index, Pivot_Index);
            else
               --  Row contains only zeros; no pivot
               null;
            end if;
         end;
      end loop;

      if (for some I in 1 .. Rows => Pivots (I) = 0
        and Any_True (Tensor_Type'(B.Get (I) /= 0.0)))
      then
         Solution := None;
      elsif Columns > Rows or else (for some I in 1 .. Columns => Pivots (I) /= I) then
         Solution := Infinite;
      else
         Solution := Unique;
      end if;

      return Ab.Get (Tensor_Range'((1, Rows), (Columns + 1, Columns_Ab)));
   end Matrix_Solve;

   overriding
   function Solve (A, B : Tensor_Type; Solution : out Solution_Kind) return Tensor_Type is
   begin
      case B.Axes is
         when 1 =>
            return Matrix_Solve (A, B.Reshape ((B.Elements, 1)), Solution).Flatten;
         when 2 =>
            return Matrix_Solve (A, B, Solution);
         when others =>
            raise Not_Implemented_Yet;  --  FIXME
      end case;
   end Solve;

   function Householder_Matrix
     (R, I  : Tensor_Type;
      Index : Index_Type;
      Size  : Positive) return Tensor_Type
   is
      U : Tensor_Type :=
        R.Get (Tensor_Range'((Index, Size), (Index, Index))).Reshape (Size - Index + 1);
      U1 : constant Element := U.Get (1);

      --  Alpha must have the opposite sign of R ((Index, Index)) (or U (1))
      Alpha : constant Element := -Element'Copy_Sign (1.0, U1) * U.Norm;
   begin
      U.Set (Tensor_Index'(1 => 1), U1 - Alpha);

      declare
         --  V = Normalize (X - Alpha * E) where X is column vector R (Index)
         --        and X (1 .. Index - 1) is set to 0.0
         --        and Alpha is X.Norm with opposite sign of X (Index)
         --        and E = [0 ... 0 1 0 ... 0]^T with 1 at Index
         V : constant Tensor_Type := Zeros ((1 => Index - 1)) & U.Normalize;
      begin
         return I - 2.0 * Outer (V, V);
      end;
   end Householder_Matrix;

   overriding
   function QR (Object : Tensor_Type) return Tensor_Type is
      Rows    : constant Positive := Object.Rows;
      Columns : constant Positive := Object.Columns;

      K : constant Positive := Positive'Min (Rows, Columns);

      Size : Positive renames Rows;
      I : constant Tensor_Type := Identity (Size => Size);

      R : Tensor_Type := Object;
   begin
      --  QR decomposition using householder reflections

      for Index in 1 .. Natural'Min (Rows - 1, Columns) loop
         --  Compute householder matrix using column R (Index)
         R := Householder_Matrix (R, I, Index, Size) * R;
      end loop;

      Make_Upper_Triangular (R);

      return R.Get (Tensor_Range'((1, K), (1, Columns)));
   end QR;

   function QR
     (Object       : Tensor_Type;
      Determinancy : Matrix_Determinancy;
      Mode         : QR_Mode) return QR_Factorization'Class
   is
      Rows    : constant Positive := Object.Rows;
      Columns : constant Positive := Object.Columns;

      Size : Positive renames Rows;
      I : constant Tensor_Type := Identity (Size => Size);

      Q : Tensor_Type := I;
      R : Tensor_Type := Object;
   begin
      --  QR decomposition using householder reflections

      for Index in 1 .. Natural'Min (Rows - 1, Columns) loop
         --  Compute householder matrix using column R (Index)
         declare
            Householder : constant Tensor_Type := Householder_Matrix (R, I, Index, Size);
         begin
            Q := Q * Householder;
            R := Householder * R;
         end;
      end loop;

      Make_Upper_Triangular (R);

      case Mode is
         when Complete =>
            return Create_QR_Factorization (Q, R, Determinancy);
         when Reduced =>
            declare
               K : constant Natural := Natural'Min (Rows, Columns);

               Q1 : constant Tensor_Type := Q.Get (Tensor_Range'((1, Rows), (1, K)));
               R1 : constant Tensor_Type := R.Get (Tensor_Range'((1, K), (1, Columns)));
            begin
               return Create_QR_Factorization (Q1, R1, Determinancy);
            end;
      end case;
   end QR;

   overriding
   function QR (Object : Tensor_Type; Mode : QR_Mode := Reduced) return QR_Factorization'Class is
     (QR (Object, Unknown, Mode));

   overriding
   function QR_For_Least_Squares (Object : Tensor_Type) return QR_Factorization'Class is
      Rows    : constant Positive := Object.Rows;
      Columns : constant Positive := Object.Columns;
   begin
      if Rows >= Columns then
         return QR (Object, Overdetermined, Reduced);
      else
         return QR (Object.Transpose, Underdetermined, Reduced);
      end if;
   end QR_For_Least_Squares;

   function QR_Solve (R, Y : Tensor_Type; Determinancy : Matrix_Determinancy) return Tensor_Type
     with Post => Is_Equal (QR_Solve'Result.Shape, Y.Shape, 1);

   function QR_Solve
     (R, Y         : Tensor_Type;
      Determinancy : Matrix_Determinancy) return Tensor_Type
   is
      Ry : Tensor_Type := Concatenate (R, Y, Axis => 2);

      Columns : constant Positive := R.Columns;
      Size    : constant Positive := Natural'Min (R.Rows, R.Columns);
      --  Use the smallest Axis of R for the size of the reduced (square) version R1
      --  without needing to extract it
      --
      --  R = [R1] (A is overdetermined) or R = [R1 0] (A is underdetermined)
      --      [ 0]

      Columns_Ry : constant Positive := Ry.Columns;
   begin
      case Determinancy is
         when Overdetermined =>
            --  Backward phase: row reduce augmented matrix of R * x = (Q^T * b = y) to
            --  reduced echelon form by performing back-substitution on Ry
            --  (since R is upper triangular no forward phase is needed)
            for Index in reverse 1 .. Size loop
               Back_Substitute (Ry, Index, Index);
            end loop;
         when Underdetermined =>
            --  Forward phase: row reduce augmented matrix of R^T * y = b
            --  to reduced echelon form by performing forward-substitution on Ry
            --  (R is actually R^T) (reduced because R^T is lower triangular)
            for Index in 1 .. Size loop
               Scale_Row (Ry, Index, 1.0 / Ry.Get ((Index, Index)));
               Forward_Substitute (Ry, Index, Index);
            end loop;
         when Unknown => raise Program_Error;
      end case;

      return Ry.Get (Tensor_Range'((1, Size), (Columns + 1, Columns_Ry)));
   end QR_Solve;

   overriding
   function Least_Squares (Object : QR_Factorization'Class; B : Tensor_Type) return Tensor_Type is
      QR : QR_Factorization_Type renames QR_Factorization_Type (Object);

      --  The least-squares solution is computed differently based on
      --  whether the QR decomposition was based on A or A^T:
      --
      --  Let A.Shape = (m, n), then:
      --
      --  if m >= n (overdetermined):
      --    - Solve R * x = Q^T * b for x with back-substitution
      --  if m < n (underdetermined):
      --    - Assume QR was solved for A^T = Q * R
      --    - Compute x = Q * y where R^T * y = b is solved for y with forward-substitution

      D : constant Matrix_Determinancy := Object.Determinancy;
   begin
      case D is
         when Overdetermined =>
            declare
               Y : constant Tensor_Type := QR.Q.Transpose * B;
            begin
               case Y.Axes is
                  when 1 =>
                     return QR_Solve (QR.R, Y.Reshape ((Y.Elements, 1)), D).Flatten;
                  when 2 =>
                     return QR_Solve (QR.R, Y, D);
                  when others =>
                     raise Not_Implemented_Yet;  --  FIXME
               end case;
            end;
         when Underdetermined =>
            case B.Axes is
               when 1 =>
                  return QR.Q * QR_Solve (QR.R.Transpose, B.Reshape ((B.Elements, 1)), D).Flatten;
               when 2 =>
                  return QR.Q * QR_Solve (QR.R.Transpose, B, D);
               when others =>
                  raise Not_Implemented_Yet;  --  FIXME
            end case;
         when Unknown => raise Program_Error;
      end case;
   end Least_Squares;

   overriding
   function Least_Squares (A, B : Tensor_Type) return Tensor_Type is
     (Least_Squares (QR_For_Least_Squares (A), B));

   overriding
   function Constrained_Least_Squares (A, B, C, D : Tensor_Type) return Tensor_Type is
      AC : constant Tensor_Type := Concatenate (A, C, Axis => 1);

      QR_AC : constant QR_Factorization_Type := QR_Factorization_Type (QR_For_Least_Squares (AC));

      QR_AC_Q1_T : constant Tensor_Type := QR_AC.Q.Get (Range_Type'(1, A.Rows)).Transpose;
      QR_AC_Q2 : constant Tensor_Type := QR_AC.Q.Get (Range_Type'(A.Rows + 1, AC.Rows));

      QR_Q2 : constant QR_Factorization_Type :=
        QR_Factorization_Type (QR_For_Least_Squares (QR_AC_Q2));
      pragma Assert (QR_Q2.Determinancy = Underdetermined);

      RQ2_IT_D : constant Tensor_Type :=
        (case D.Axes is
            when 1 =>
               QR_Solve (QR_Q2.R.Transpose, D.Reshape ((D.Elements, 1)), Underdetermined),
            when 2 =>
               QR_Solve (QR_Q2.R.Transpose, D, Underdetermined),
            when others =>
               raise Not_Implemented_Yet);  --  FIXME

      B_Matrix : constant Tensor_Type :=
        (case B.Axes is
            when 1 => B.Reshape ((B.Elements, 1)),
            when 2 => B,
            when others => raise Not_Implemented_Yet);  --  FIXME

      Rw : constant Tensor_Type :=
        Tensor_Type'(Tensor_Type'(2.0 * QR_Q2.Q.Transpose * QR_AC_Q1_T) * B_Matrix)
          - 2.0 * RQ2_IT_D;

      W : constant Tensor_Type := QR_Solve (QR_Q2.R, Rw, Overdetermined);

      Rx : constant Tensor_Type :=
        Tensor_Type'(QR_AC_Q1_T * B_Matrix) - Tensor_Type'(0.5 * QR_AC_Q2.Transpose * W);

      Result : constant Tensor_Type := QR_Solve (QR_AC.R, Rx, Overdetermined);
   begin
      return (if B.Axes = 1 then Result.Flatten else Result);
   end Constrained_Least_Squares;

   function Cholesky_Lower (Object : Tensor_Type) return Tensor_Type is
      Rows  : constant Natural      := Object.Rows;
      Shape : constant Tensor_Shape := (1 .. 2 => Rows);

      Empty : constant Tensor_Type := Zeros ((1 => 0));
   begin
      return Result : Tensor_Type := Zeros (Shape) do
         for J in 1 .. Rows loop
            declare
               Row_J_Before_J : constant Tensor_Type :=
                  (if J = 1 then Empty else Result.Get (Tensor_Range'((J, J), (1, J - 1))));

               Ljj_Squared : constant Element :=
                 Object.Get ((J, J)) - Power (Row_J_Before_J, 2).Sum;
            begin
               --  If = 0.0 then matrix is positive semi-definite and singular
               --  If < 0.0 then matrix is negative semi-definite or indefinite
               if Ljj_Squared <= 0.0 then
                  raise Not_Positive_Definite_Matrix with
                    Ljj_Squared'Image & " at row " & J'Image;
               end if;

               declare
                  Ljj : constant Element := EF.Sqrt (Ljj_Squared);
               begin
                  --  Compute the jth value on the diagonal
                  Result.Set ((J, J), Ljj);

                  --  Compute the values below the jth value on the diagonal
                  for I in J + 1 .. Rows loop
                     declare
                        Row_I_Before_J : constant Tensor_Type :=
                          (if J = 1 then Empty
                           else Result.Get (Tensor_Range'((I, I), (1, J - 1))));

                        Lij : constant Element :=
                          (Object.Get ((I, J)) - Multiply (Row_I_Before_J, Row_J_Before_J).Sum)
                            / Ljj;
                     begin
                        Result.Set (Tensor_Index'(I, J), Lij);
                     end;
                  end loop;
               end;
            end;
         end loop;
      end return;
   end Cholesky_Lower;

   overriding
   function Cholesky (Object : Tensor_Type; Form : Triangular_Form := Lower) return Tensor_Type is
     (case Form is
        when Lower => Cholesky_Lower (Object),
        when Upper => Cholesky_Lower (Object.Transpose).Transpose);

   overriding
   function Cholesky_Update
     (R, V : Tensor_Type;
      Mode : Update_Mode) return Tensor_Type
   is
      Rows : constant Natural := R.Rows;
   begin
      case Mode is
         when Update =>
            raise Program_Error with "Not implemented yet";
         when Downdate =>
            --  Implementation of Algorithm B from [1]
            --
            --  [1] "A modification to the LINPACK downdating algorithm", Pan C.T.,
            --      BIT Numerical Mathematics, 1990, 30.4: 707-722, DOI:10.1007/BF01933218
            return Result : Tensor_Type := Zeros (R.Shape) do
               declare
                  Alpha : Element := 1.0;
                  Beta  : Element := 1.0;

                  Z : Tensor_Type := V;
               begin
                  for I in 1 .. Rows loop
                     declare
                        Rii : constant Element := R.Get ((I, I));

                        A : constant Element := Element'(Z.Get (I)) / Rii;
                     begin
                        Alpha := Alpha - A**2;

                        --  If = 0.0 then matrix is positive semi-definite and singular
                        --  If < 0.0 then matrix is negative semi-definite or indefinite
                        if Alpha <= 0.0 then
                           raise Not_Positive_Definite_Matrix with
                             Alpha'Image & " at row " & I'Image;
                        end if;

                        declare
                           Previous_Beta : constant Element := Beta;
                        begin
                           Beta := EF.Sqrt (Alpha);

                           --  Compute the jth value on the diagonal
                           Result.Set ((I, I), Beta / Previous_Beta * Rii);

                           exit when I = Rows;

                           declare
                              C1 : constant Element := Beta / Previous_Beta;
                              C2 : constant Element := A / (Previous_Beta * Beta);

                              Column_Indices : constant Range_Type   := (I + 1, Rows);
                              Row_Indices    : constant Tensor_Range := ((I, I), Column_Indices);

                              Rik_Old : constant Tensor_Type := R.Get (Row_Indices);
                              Zk_Old  : constant Tensor_Type := Z.Get (Column_Indices);
                              Zk_New  : constant Tensor_Type := Zk_Old - A * Rik_Old;
                              Rik_New : constant Tensor_Type := C1 * Rik_Old - C2 * Zk_New;
                           begin
                              Z.Set (Column_Indices, Zk_New);
                              Result.Set (Row_Indices, Rik_New.Reshape ((1, Rik_New.Elements)));
                              --  Reshape only needed because of Pre condition on Set
                           end;
                        end;
                     end;
                  end loop;
               end;
            end return;
      end case;
   end Cholesky_Update;

   overriding
   function Solve (A, B : Tensor_Type; Form : Triangular_Form) return Tensor_Type is
      Determinancy : constant Matrix_Determinancy :=
        (case Form is
           when Upper => Overdetermined,
           when Lower => Underdetermined);
   begin
      case B.Axes is
         when 1 =>
            return QR_Solve (A, B.Reshape ((B.Elements, 1)), Determinancy).Flatten;
         when 2 =>
            return QR_Solve (A, B, Determinancy);
         when others =>
            raise Not_Implemented_Yet;  --  FIXME
      end case;
   end Solve;

   overriding
   function Divide_By (B, A : Tensor_Type) return Tensor_Type is
      QR_A : constant QR_Factorization_Type := QR_Factorization_Type (QR_For_Least_Squares (A));
   begin
      --  Let A = QR:
      --              x * (Q * R) = B
      --  Multiply sides with inverses of QR and B:
      --                B^-1 * x  = R^-1 * Q^T
      --           R * (B^-1 * x) = Q^T
      --
      --  Solve R * y = Q^T for y (= B^-1 * x) and then multiply the result with B to get x

      return B * Solve (QR_A.R, QR_A.Q.Transpose, Upper);
   end Divide_By;

   overriding
   function Divide_By (B, A : Tensor_Type; Form : Triangular_Form) return Tensor_Type is
     (B * Solve (A, Identity (Size => A.Rows), Form));

   ----------------------------------------------------------------------------
   --                            Vector operations                           --
   ----------------------------------------------------------------------------

   overriding
   function Norm (Object : Tensor_Type) return Element is (EF.Sqrt (Object * Object));

   overriding
   function Normalize (Object : Tensor_Type) return Tensor_Type is (Object / Object.Norm);

   overriding
   function Standardize (Object : Tensor_Type) return Tensor_Type is
      Std_Dev : constant Element := Object.Standard_Deviation;
   begin
      return (Object - Object.Mean) / (if Std_Dev /= 0.0 then Std_Dev else 1.0);
   end Standardize;

   overriding
   function Correlation_Coefficient (Left, Right : Tensor_Type) return Correlation_Element is
     (Element'(Left.Standardize * Right.Standardize) / Element (Left.Elements));

   ----------------------------------------------------------------------------
   --                         Element-wise operations                        --
   ----------------------------------------------------------------------------

   overriding function "*" (Left : Element; Right : Tensor_Type) return Tensor_Type is
     (Right * Left);

   overriding function "+" (Left : Element; Right : Tensor_Type) return Tensor_Type is
     (Right + Left);

   overriding function "-" (Left : Tensor_Type; Right : Element) return Tensor_Type is
     (Left + (-Right));

   overriding function "mod" (Left, Right : Tensor_Type) return Tensor_Type is
     (((Left rem Right) + Right) rem Right);
   --  TODO Replace with (Left - Multiply (Floor (Left / Right), Right))?

   overriding function "rem" (Left, Right : Tensor_Type) return Tensor_Type is
     (Left - Multiply (Truncate (Left / Right), Right));

   overriding function "mod" (Left : Tensor_Type; Right : Element) return Tensor_Type is
     (((Left rem Right) + Right) rem Right);
   --  TODO Replace with (Left - Floor (Left / Right) * Right)?

   overriding function "rem" (Left : Tensor_Type; Right : Element) return Tensor_Type is
     (Left - Truncate (Left / Right) * Right);

   overriding function Power (Left : Tensor_Type; Right : Integer) return Tensor_Type is
      function Vector_Power (Left : Tensor_Type; Right : Natural) return Tensor_Type is
         Result : Tensor_Type := Ones (Left.Shape);

         Log_2 : constant Element := EF.Log (2.0);

         Remaining : Natural := Right;
      begin
         while Remaining > 0 loop
            declare
               Doubling : Tensor_Type := Left;
               Count : constant Integer :=
                 Integer (Element'Floor (EF.Log (Element (Remaining)) / Log_2));
            begin
               for I in 1 .. Count loop
                  Doubling := Multiply (Doubling, Doubling);
               end loop;
               Result    := Multiply (Result, Doubling);
               Remaining := Remaining - 2 ** Count;
            end;
         end loop;

         return Result;
      end Vector_Power;
   begin
      if Right = 2 then
         return Multiply (Left, Left);
      elsif Right > 0 then
         return Vector_Power (Left, Right);
      elsif Right < 0 then
         return 1.0 / Vector_Power (Left, abs Right);
      else
         return Ones (Elements => Left.Rows);
      end if;
   end Power;

   overriding function Min (Left : Element; Right : Tensor_Type) return Tensor_Type is
     (Min (Right, Left));

   overriding function Max (Left : Element; Right : Tensor_Type) return Tensor_Type is
     (Max (Right, Left));

   overriding function Log10 (Object : Tensor_Type) return Tensor_Type is
     (Log (Object) / EF.Log (10.0));

   overriding function Log2 (Object : Tensor_Type) return Tensor_Type is
     (Log (Object) / EF.Log (2.0));

   overriding function Degrees (Object : Tensor_Type) return Tensor_Type is
     (Object / Ada.Numerics.Pi * 180.0);

   overriding function Radians (Object : Tensor_Type) return Tensor_Type is
     (Object / 180.0 * Ada.Numerics.Pi);

   overriding function Sum (Object : Tensor_Type) return Element is
      Expression_Sum : constant Expression_Type := X + Y;
   begin
      return Object.Reduce_Associative (Expression_Sum, 0.0);
   end Sum;

   overriding function Product (Object : Tensor_Type) return Element is
      Expression_Product : constant Expression_Type := X * Y;
   begin
      return Object.Reduce_Associative (Expression_Product, 1.0);
   end Product;

   overriding
   function Sum (Object : Tensor_Type; Axis : Tensor_Axis) return Tensor_Type is
      Expression_Sum : constant Expression_Type := X + Y;
   begin
      return Object.Reduce_Associative (Expression_Sum, 0.0, Axis);
   end Sum;

   overriding
   function Product (Object : Tensor_Type; Axis : Tensor_Axis) return Tensor_Type is
      Expression_Product : constant Expression_Type := X * Y;
   begin
      return Object.Reduce_Associative (Expression_Product, 1.0, Axis);
   end Product;

   ----------------------------------------------------------------------------
   --                               Statistics                               --
   ----------------------------------------------------------------------------

   overriding function Min (Object : Tensor_Type) return Element is
      Expression_Min : constant Expression_Type := Min (X, Y);
   begin
      return Object.Reduce_Associative (Expression_Min, Element'Last);
   end Min;

   overriding function Max (Object : Tensor_Type) return Element is
      Expression_Max : constant Expression_Type := Max (X, Y);
   begin
      return Object.Reduce_Associative (Expression_Max, Element'First);
   end Max;

   overriding
   function Min (Object : Tensor_Type; Axis : Tensor_Axis) return Tensor_Type is
      Expression_Min : constant Expression_Type := Min (X, Y);
   begin
      return Object.Reduce_Associative (Expression_Min, Element'Last, Axis);
   end Min;

   overriding
   function Max (Object : Tensor_Type; Axis : Tensor_Axis) return Tensor_Type is
      Expression_Max : constant Expression_Type := Max (X, Y);
   begin
      return Object.Reduce_Associative (Expression_Max, Element'First, Axis);
   end Max;

   function Median_Of_Three (A, B, C : Element) return Element is
   begin
      if A in B .. C | C .. B then
         return A;
      elsif B in A .. C | C .. A then
         return B;
      else
         return C;
      end if;
   end Median_Of_Three;

   function Quick_Select (Object : Tensor_Type; K : Positive) return Element
     with Pre => K <= Object.Elements
   is
      N : constant Positive := Object.Elements;

      Splitter : constant Element :=
        (if N > 1 then
           Median_Of_Three (Object.Get (1), Object.Get (N / 2), Object.Get (N))
         else
           Object.Get (1));

      Less : constant Tensor_Type := Object.Get (Object < Splitter);
      L    : constant Natural     := Less.Elements;
   begin
      if L >= K then
         --  Kth element in Less
         return Quick_Select (Less, K);
      else
         declare
            More : constant Tensor_Type := Object.Get (Object > Splitter);
            M    : constant Natural    := More.Elements;

            pragma Assert (L + M < N);
         begin
            if L < K and K <= N - M then
               --  Kth element not in Less or More
               return Splitter;
            else
               --  Kth element in More
               return Quick_Select (More, K - (N - M));
            end if;
         end;
      end if;
   end Quick_Select;

   overriding function Quantile (Object : Tensor_Type; P : Probability) return Element is
      function Internal_Quantile (Object : Tensor_Type; P : Probability) return Element is
         Last_Index : constant Element := Element (Object.Elements - 1);

         K_Lower : constant Positive := Natural (Element'Floor (Last_Index * Element (P))) + 1;
         K_Upper : constant Positive := Positive'Min (Object.Elements, K_Lower + 1);

         P_Lower : constant Probability := Probability (Element (K_Lower - 1) / Last_Index);
         --  Convert K_Lower back to a probability so that the difference
         --  with P can be computed (needed for interpolation when P would
         --  map to an element that is between two indices)

         Element_Lower : constant Element := Quick_Select (Object, K_Lower);
         Element_Upper : constant Element :=
           (if P_Lower < P then Quick_Select (Object, K_Upper) else Element_Lower);

         Probability_Per_Index : constant Probability := Probability (1.0 / Last_Index);
         Probability_Ratio     : constant Probability := (P - P_Lower) / Probability_Per_Index;
      begin
         return (Element_Upper - Element_Lower) * Element (Probability_Ratio) + Element_Lower;
      end Internal_Quantile;
   begin
      case Object.Elements is
         when 0      => raise Constraint_Error with "Tensor is empty";
         when 1      => return Object.Get (1);
         when others => return Internal_Quantile (Object, P);
      end case;
   end Quantile;

   overriding function Median (Object : Tensor_Type) return Element is
     (Object.Quantile (0.5));

   overriding function Mean (Object : Tensor_Type) return Element is
     (Object.Sum / Element (Object.Elements));

   overriding
   function Variance (Object : Tensor_Type; Offset : Natural := 0) return Element is
     (Sum (Power (Object - Object.Mean, 2)) / (Element (Object.Elements - Offset)));

   overriding
   function Standard_Deviation (Object : Tensor_Type; Offset : Natural := 0) return Element is
     (EF.Sqrt (Object.Variance (Offset)));

   ----------------------------------------------------------------------------

   overriding
   function Mean (Object : Tensor_Type; Axis : Tensor_Axis) return Tensor_Type is
     (Object.Sum (Axis) / Element (Object.Shape (Axis)));

   overriding
   function Variance
     (Object : Tensor_Type;
      Axis   : Tensor_Axis;
      Offset : Natural := 0) return Tensor_Type
   is
      Repeat : constant Positive := Object.Shape (Axis);
      Mean : constant Tensor_Type := Object.Mean (Axis);
      Repeated_Mean : Tensor_Type := Mean;
   begin
      for Index in 1 .. Repeat - 1 loop
         Repeated_Mean := Concatenate (Repeated_Mean, Mean, Axis);
      end loop;

      return Sum (Power (Object - Mean, 2), Axis)
               / (Element (Object.Shape (Axis) - Offset));
   end Variance;

   overriding
   function Median (Object : Tensor_Type; Axis : Tensor_Axis) return Tensor_Type is
     (Object.Quantile (0.5, Axis));

   overriding
   function Standard_Deviation
     (Object : Tensor_Type;
      Axis   : Tensor_Axis;
      Offset : Natural := 0) return Tensor_Type
   is (Sqrt (Object.Variance (Axis, Offset)));

   ----------------------------------------------------------------------------
   --                              Comparisons                               --
   ----------------------------------------------------------------------------

   overriding function "="  (Left : Tensor_Type; Right : Element) return Tensor_Type is
   begin
      case Left.Kind is
         when Float_Type =>
            return (abs (Left - Right) <= Element_Type'Model_Epsilon);
         when Int_Type | Bool_Type =>
            raise Program_Error;
      end case;
   end "=";

   overriding function "/=" (Left : Tensor_Type; Right : Element) return Tensor_Type is
   begin
      case Left.Kind is
         when Float_Type =>
            return (abs (Left - Right) > Element_Type'Model_Epsilon);
         when Int_Type | Bool_Type =>
            raise Program_Error;
      end case;
   end "/=";

   ----------------------------------------------------------------------------

   overriding function "="  (Left : Element; Right : Tensor_Type) return Tensor_Type is
     (Right = Left);

   overriding function "/=" (Left : Element; Right : Tensor_Type) return Tensor_Type is
     (Right /= Left);

   overriding function ">"  (Left : Element; Right : Tensor_Type) return Tensor_Type is
     (Right < Left);

   overriding function "<"  (Left : Element; Right : Tensor_Type) return Tensor_Type is
     (Right > Left);

   overriding function ">=" (Left : Element; Right : Tensor_Type) return Tensor_Type is
     (Right <= Left);

   overriding function "<=" (Left : Element; Right : Tensor_Type) return Tensor_Type is
     (Right >= Left);

   ----------------------------------------------------------------------------

   overriding function And_Not (Left, Right : Tensor_Type) return Tensor_Type is
     ((not Left) and Right);

   overriding function "=" (Left, Right : Tensor_Type) return Boolean is
     (All_True (Left = Right));

   overriding
   function All_Close
     (Left, Right        : Tensor_Type;
      Relative_Tolerance : Element := 1.0e-05;
      Absolute_Tolerance : Element := Element_Type'Model_Epsilon) return Boolean
   is (All_True (abs (Left - Right) <= Absolute_Tolerance + Relative_Tolerance * abs Right));

end Orka.Numerics.Tensors.Operations;
