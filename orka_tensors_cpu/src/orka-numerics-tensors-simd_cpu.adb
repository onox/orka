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

with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

with Orka.Numerics.Tensors.Operations;

package body Orka.Numerics.Tensors.SIMD_CPU is

   procedure Swap_Rows (Ab : in out CPU_Tensor; I, J : Index_Type) is
   begin
      if I /= J then
         declare
            Row_I : constant CPU_Tensor := Ab (I);
            Old_J : constant CPU_Tensor := Ab (J);
         begin
            Set (Ab, J, Row_I);
            Set (Ab, I, Old_J);
         end;
      end if;
   end Swap_Rows;

   procedure Scale_Row (Ab : in out CPU_Tensor; I : Index_Type; Scale : Element) is
      Row_I : constant CPU_Tensor := Ab (I);
   begin
      if Scale /= 1.0 then
         Set (Ab, I, Scale * Row_I);
      end if;
   end Scale_Row;

   procedure Replace_Row (Ab : in out CPU_Tensor; Scale : Element; I, J : Index_Type) is
      Row_I : constant CPU_Tensor := Ab (I);
      Row_J : constant CPU_Tensor := Ab (J);
   begin
      if Scale /= 0.0 then
         Set (Ab, J, Row_J - Scale * Row_I);
      end if;
   end Replace_Row;

   procedure Forward_Substitute (Ab : in out CPU_Tensor; Index, Pivot_Index : Index_Type) is
      Rows        : constant Natural := Ab.Rows;
      Pivot_Value : constant Element := Ab ((Index, Pivot_Index));
   begin
      --  Create zeros below the pivot position
      for Row_Index in Index + 1 .. Rows loop
         Replace_Row (Ab, Ab ((Row_Index, Pivot_Index)) / Pivot_Value, Index, Row_Index);
      end loop;
   end Forward_Substitute;

   procedure Back_Substitute (Ab : in out CPU_Tensor; Index, Pivot_Index : Index_Type) is
   begin
      Scale_Row (Ab, Index, 1.0 / Ab ((Index, Pivot_Index)));

      --  Create zeros above the pivot position
      for Row_Index in 1 .. Index - 1 loop
         Replace_Row (Ab, Ab ((Row_Index, Pivot_Index)), Index, Row_Index);
      end loop;
   end Back_Substitute;

   function Create_QR
     (Q, R         : CPU_Tensor;
      Determinancy : Matrix_Determinancy) return CPU_QR_Factorization
   is (Q_Size       => Q.Size,
       R_Size       => R.Size,
       Q            => Q,
       R            => R,
       Determinancy => Determinancy);

   procedure Make_Upper_Triangular (Object : in out CPU_Tensor; Offset : Integer := 0) is
      Rows    : constant Natural := Object.Rows;
      Columns : constant Natural := Object.Columns;
   begin
      if Offset >= -(Rows - 2) then
         --  Make matrix upper triangular by zeroing out the elements in the
         --  lower triangular part
         for Row_Index in Index_Type'First + 1 - Integer'Min (1, Offset) .. Rows loop
            for Column_Index in 1 .. Natural'Min (Row_Index - 1 + Offset, Columns) loop
               Object.Set ((Row_Index, Column_Index), 0.0);
            end loop;
         end loop;
      end if;
   end Make_Upper_Triangular;

   package Operations is new Orka.Numerics.Tensors.Operations
     (CPU_Tensor, Make_Upper_Triangular, Scale_Row, Swap_Rows, Forward_Substitute, Back_Substitute,
      Expression_Type, CPU_QR_Factorization, Create_QR, Q, R);

   ----------------------------------------------------------------------------

   function Convert is new Ada.Unchecked_Conversion (Vector_Type, Integer_Vector_Type);
   function Convert is new Ada.Unchecked_Conversion (Integer_Vector_Type, Vector_Type);

   function From_Last (Offset : Natural) return Vector_Index_Type is
     (Vector_Index_Type'Val (Vector_Index_Type'Pos (Vector_Index_Type'Last) - Offset));

   function Data_Vectors (Index : Natural) return Natural is
     ((Index + (Vector_Type'Length - 1)) / Vector_Type'Length);

   function Data_Vectors (Shape : Tensor_Shape) return Natural is
     (Data_Vectors (Elements (Shape)));

   function Data_Offset (Index : Positive) return Vector_Index_Type is
     (Vector_Index_Type'Val ((Index - 1) mod Vector_Type'Length
        + Vector_Index_Type'Pos (Vector_Index_Type'First)));

   function Data_Padding (Size, Count : Natural) return Natural is
     (Size * Vector_Type'Length - Count);

   function To_Index (Index : Tensor_Index; Columns : Positive) return Index_Type is
     (To_Index (Index, (Index (1), Columns)))
   with Pre => Index'Length = 2;
   --  Index (1) isn't used for 2-D Index except in Pre condition of To_Index in parent package

   function Reset_Padding
     (Object  : CPU_Tensor;
      Padding : Natural;
      Value   : Element_Type) return Vector_Type
   is
      Last_Vector : Vector_Type := Object.Data (Object.Data'Last);
   begin
      if Padding /= 0 then
         Last_Vector (From_Last (Padding - 1) .. Last_Vector'Last) := (others => Value);
      end if;

      return Last_Vector;
   end Reset_Padding;

   function Disable_Padding (Object : CPU_Tensor) return Vector_Type is
      Padding : constant Natural :=
        Data_Padding (Size => Object.Size, Count => Object.Elements);
   begin
      return Reset_Padding (Object, Padding, 0.0);
   end Disable_Padding;

   function Without_Data
     (Object : CPU_Tensor;
      Kind   : Data_Type := Float_Type) return CPU_Tensor;
   --  This silly definition is needed to avoid "length check failed" in GNAT FSF 11.1

   function Without_Data
     (Object : CPU_Tensor;
      Kind   : Data_Type := Float_Type) return CPU_Tensor
   is
     ((Axes  => Object.Axes,
       Size  => Object.Size,
       Kind  => Kind,
       Shape => Object.Shape,
       Data  => <>));

   function Without_Data
     (Shape : Tensor_Shape;
      Kind  : Data_Type := Float_Type) return CPU_Tensor
   is
     ((Axes  => Shape'Length,
       Size  => Data_Vectors (Shape),
       Kind  => Kind,
       Shape => Shape,
       Data  => <>));

   ----------------------------------------------------------------------------
   --                              Expressions                               --
   ----------------------------------------------------------------------------

   function Identity_Vector_Type (Value : Element) return Vector_Type is (others => Value);
   function Identity_Element     (Value : Element) return Element     is (Value);

   function Apply is new Generic_Apply (Vector_Type, Identity_Vector_Type);

   function Apply is new Generic_Apply (Element, Identity_Element,
     Min => Element'Min, Max => Element'Max, Sqrt => EF.Sqrt);

   ----------------------------------------------------------------------------

   overriding function Kind (Object : CPU_Tensor) return Data_Type is (Object.Kind);

   overriding
   function Get (Object : CPU_Tensor; Index : Index_Type) return Element renames Operations.Get;

   overriding
   function Get (Object : CPU_Tensor; Index : Index_Type) return Boolean renames Operations.Get;

   overriding function Get (Object : CPU_Tensor; Index : Index_Type) return CPU_Tensor is
      Count : constant Positive := Object.Columns;
      Shape : constant Tensor_Shape := (1 => Count);
   begin
      if Index > Object.Rows then
         raise Constraint_Error with
           "Stop index (" & Trim (Index) & ") out of bounds (1 .. " & Trim (Object.Rows) & ")";
      end if;

      --  Returning the row of a 2D tensor as a vector instead of a (1, n) 2D tensor
      return Result : CPU_Tensor := Without_Data (Shape, Object.Kind) do
         declare
            Result_Data : Element_Array (1 .. Count)
              with Import, Convention => Ada, Address => Result.Data'Address;

            Object_Data : Element_Array (1 .. Object.Elements)
              with Import, Convention => Ada, Address => Object.Data'Address;

            Base_Index : constant Natural := (Index - 1) * Count;
         begin
            Result_Data (1 .. Count) := Object_Data (Base_Index + 1 .. Base_Index + Count);
         end;
      end return;
   end Get;

   overriding procedure Set
     (Object : in out CPU_Tensor;
      Index  : Index_Type;
      Value  : CPU_Tensor) renames Operations.Set;

   overriding procedure Set
     (Object : in out CPU_Tensor;
      Index  : Range_Type;
      Value  : CPU_Tensor) renames Operations.Set;

   overriding
   procedure Set (Object : in out CPU_Tensor; Index : Tensor_Range; Value : CPU_Tensor) is
      Full_Index : constant Tensor_Range := Full_Range (Object.Shape, Index);
      Full_Value : constant Tensor_Shape := Full_Shape (Object.Axes, Value.Shape, Right);

      pragma Assert (Full_Value = Shape (Full_Index));
   begin
      --  If the value (and shape of index) has the full depth/height/width except
      --  for the first axis, then the memory to which the data will be written
      --  is contiguous, which means it has no gaps.
      --
      --  For example, if shape of Value is (2, 3) and you have the following
      --  object and index (in brackets):
      --
      --  1 [ 2  3  4]  5
      --  6 [ 7  8  9] 10
      --  11 12 13 14  15
      --
      --  then there is a gap (positions 5 and 6). Howerver, if the shape
      --  of Value is (2, 5) (with a matching Index) then there are no gaps.
      --
      --  Another case in which there are are no gaps is when all but the last
      --  axis have a shape equal to 1. For example if the index is
      --  ((2, 2), (7, 9)), which has the shape (1, 3).
      if Is_Equal (Object.Shape, Full_Value, 1)
        or else (for all D in Full_Value'First .. Full_Value'Last - 1 => Full_Value (D) = 1)
      then
         declare
            Start_Index : Tensor_Index (Full_Index'Range);
            Stop_Index  : Tensor_Index (Full_Index'Range);
         begin
            for Axis in Full_Index'Range loop
               Start_Index (Axis) := Full_Index (Axis).Start;
               Stop_Index (Axis)  := Full_Index (Axis).Stop;
            end loop;

            declare
               Start_Index_Flattened : constant Index_Type := To_Index (Start_Index, Object.Shape);
               Stop_Index_Flattened  : constant Index_Type := To_Index (Stop_Index, Object.Shape);

               Count : constant Natural := Value.Elements;
               pragma Assert (Stop_Index_Flattened - Start_Index_Flattened + 1 = Count);

               Row_Data : Element_Array (1 .. Count)
                 with Import, Convention => Ada, Address => Value.Data'Address;

               Object_Data : Element_Array (1 .. Object.Elements)
                 with Import, Convention => Ada, Address => Object.Data'Address;
            begin
               Object_Data (Start_Index_Flattened .. Stop_Index_Flattened) := Row_Data;
            end;
         end;
      else
         raise Program_Error with "Not implemented yet";  --  FIXME
      end if;
   end Set;

   function Flattened_Index (Object : CPU_Tensor; Index : Tensor_Index) return Index_Type is
   begin
      for Axis in Index'Range loop
         declare
            Index_Dim : constant Natural := Index (Axis);
            Shape_Dim : constant Natural := Object.Shape (Axis);
         begin
            if Index_Dim > Shape_Dim then
               raise Constraint_Error with
                 "Index (" & Trim (Index_Dim) & ") out of bounds (1 .. " & Trim (Shape_Dim) & ")";
            end if;
         end;
      end loop;

      return To_Index (Index, Object.Shape);
   end Flattened_Index;

   overriding procedure Set
     (Object : in out CPU_Tensor;
      Index  : Index_Type;
      Value  : Element) renames Operations.Set;

   overriding procedure Set
     (Object : in out CPU_Tensor;
      Index  : Index_Type;
      Value  : Boolean) renames Operations.Set;

   overriding procedure Set (Object : in out CPU_Tensor; Index : Tensor_Index; Value : Element) is
      Index_Flattened : constant Index_Type := Flattened_Index (Object, Index);
   begin
      Object.Data (Data_Vectors (Index_Flattened)) (Data_Offset (Index_Flattened)) := Value;
   end Set;

   overriding procedure Set (Object : in out CPU_Tensor; Index : Tensor_Index; Value : Boolean) is
      Index_Flattened : constant Index_Type := Flattened_Index (Object, Index);

      Zero_Vector : constant Vector_Type := (others => 0.0);

      Mask : constant Integer_Vector_Type := Convert (Zero_Vector = Zero_Vector);

      Booleans : Integer_Vector_Type := Convert (Object.Data (Data_Vectors (Index_Flattened)));
   begin
      Booleans (Data_Offset (Index_Flattened)) := (if Value then Mask (Mask'First) else 0);
      Object.Data (Data_Vectors (Index_Flattened)) := Convert (Booleans);
   end Set;

   overriding function Get (Object : CPU_Tensor; Index : Tensor_Index) return Element is
      Object_Data : Element_Array (1 .. Object.Elements)
        with Import, Convention => Ada, Address => Object.Data'Address;
   begin
      return Object_Data (Flattened_Index (Object, Index));
   end Get;

   overriding function Get (Object : CPU_Tensor; Index : Tensor_Index) return Boolean is
      Index_Flattened : constant Index_Type := Flattened_Index (Object, Index);

      One_Vector : constant Integer_Vector_Type := (others => 1);

      Mask       : constant Integer_Vector_Type :=
        Convert (Object.Data (Data_Vectors (Index_Flattened)));
      Ones_Zeros : constant Integer_Vector_Type := Mask and One_Vector;
   begin
      return Ones_Zeros (Data_Offset (Index_Flattened)) = 1;
   end Get;

   overriding
   function Get (Object : CPU_Tensor; Index : Range_Type) return CPU_Tensor renames Operations.Get;

   overriding function Get (Object : CPU_Tensor; Index : Tensor_Range) return CPU_Tensor is
      Rows : constant Natural := Object.Rows;

      Row_Start : constant Index_Type := Index (1).Start;
      Row_Stop  : constant Index_Type := Index (1).Stop;

      Result_Rows : constant Positive := Row_Stop - Row_Start + 1;
   begin
      case Object.Axes is
         when 1 =>
            declare
               Count : constant Positive := Result_Rows;
               Size  : constant Positive := Data_Vectors (Count);
               Shape : constant Tensor_Shape := (1 => Count);
            begin
               if Row_Stop > Rows then
                  raise Constraint_Error with
                    "Stop index (" & Trim (Row_Stop) & ") out of bounds (1 .. " &
                    Trim (Rows) & ")";
               end if;

               return Result : CPU_Tensor := Without_Data (Shape, Object.Kind) do
                  if Data_Offset (Row_Start) = Vector_Index_Type'First then
                     Result.Data (1 .. Size) :=
                       Object.Data (Data_Vectors (Row_Start) .. Data_Vectors (Row_Stop));
                  else
                     declare
                        Result_Data : Element_Array (1 .. Count)
                          with Import, Convention => Ada, Address => Result.Data'Address;

                        Object_Data : Element_Array (1 .. Object.Elements)
                          with Import, Convention => Ada, Address => Object.Data'Address;
                     begin
                        Result_Data (1 .. Count) := Object_Data (Row_Start .. Row_Stop);
                     end;
                  end if;
               end return;
            end;
         when 2 =>
            declare
               Columns : constant Natural :=
                 (if 2 in Object.Shape'Range then Object.Columns else 1);

               Index_Shape : constant Tensor_Shape := Shape (Index);
               Result_Columns : constant Positive :=
                 (if 2 in Index_Shape'Range then Index_Shape (2) else Columns);

               Shape : constant Tensor_Shape :=
                 (if Result_Rows = 1 then
                    (1 => Result_Columns)
                  else
                    (1 => Result_Rows, 2 => Result_Columns));

               Column_Start : constant Index_Type :=
                 (if 2 in Index'Range then Index (2).Start else 1);
               Column_Stop  : constant Index_Type :=
                 (if 2 in Index'Range then Index (2).Stop else Columns);
            begin
               if Row_Stop > Rows then
                  raise Constraint_Error with
                    "Stop index (" & Trim (Row_Stop) & ") out of bounds (1 .. " &
                    Trim (Rows) & ")";
               end if;

               if Column_Stop > Columns then
                  raise Constraint_Error with
                    "Stop index (" & Trim (Column_Stop) & ") out of bounds (1 .. " &
                    Trim (Columns) & ")";
               end if;

               return Result : CPU_Tensor := Without_Data (Shape, Object.Kind) do
                  declare
                     Result_Data : Element_Array (1 .. Result.Elements)
                       with Import, Convention => Ada, Address => Result.Data'Address;

                     Object_Data : Element_Array (1 .. Object.Elements)
                       with Import, Convention => Ada, Address => Object.Data'Address;
                  begin
                     for I in 1 .. Result_Rows loop
                        declare
                           Result_Index : constant Natural := (I - 1) * Result_Columns;

                           Current_Row : constant Natural := Row_Start - 1 + I;
                           Base_Index  : constant Natural := (Current_Row - 1) * Columns;
                        begin
                           Result_Data (Result_Index + 1 .. Result_Index + Result_Columns) :=
                             Object_Data (Base_Index + Column_Start .. Base_Index + Column_Stop);
                        end;
                     end loop;
                  end;
               end return;
            end;
         when others =>
            raise Not_Implemented_Yet;  --  FIXME
      end case;
   end Get;

   overriding function Get (Object : CPU_Tensor; Index : CPU_Tensor) return CPU_Tensor is
      type Integer_Vector_Array is array (Index_Type range <>) of Integer_Vector_Type;

      One_Vector : constant Integer_Vector_Type := (others => 1);

      Indices : Integer_Vector_Array (1 .. Index.Size);

      Offset : Integer_Vector_Type := (others => 0);
   begin
      for Index_Vector in Index.Data'Range loop
         declare
            --  1. Create Integer_Vector of 1s and 0s
            Mask : constant Integer_Vector_Type := Convert (Index.Data (Index_Vector));

            PS  : Integer_Vector_Type := Mask and One_Vector;
            Sum : Integer_Vector_Type := Offset + PS;
         begin
            --  2. Compute prefix sum
            for I in 2 .. Vector_Type'Length loop
               PS  := Shift_Elements_Left (PS);
               Sum := Sum + PS;
            end loop;

            --  Index is > 0 if valid according to mask
            Indices (Index_Vector) := Mask and Sum;

            --  The last number (sum of all the elements in the vector)
            --  is the offset for the next vector
            Offset := (others => Sum (Sum'Last));
         end;
      end loop;

      --  3. Last number of last vector is number of elements in Result tensor
      return Result : CPU_Tensor :=
        Without_Data (Tensor_Shape'(1 => Natural (Offset (Offset'Last))))
      do
         if Result.Elements > 0 then
            declare
               Result_Data : Element_Array (1 .. Result.Elements)
                 with Import, Convention => Ada, Address => Result.Data'Address;
            begin
               --  4. Iterate over the prefix sum and assign the value from Object.Data
               --  to Result.Data at the index found in the prefix sum
               for Index_Vector in Index.Data'Range loop
                  declare
                     Indices_Vector : Integer_Vector_Type renames Indices (Index_Vector);
                  begin
                     for I in Indices_Vector'Range loop
                        if Indices_Vector (I) > 0 then
                           Result_Data (Natural (Indices_Vector (I))) :=
                             Object.Data (Index_Vector) (I);
                        end if;
                     end loop;
                  end;
               end loop;
            end;
         end if;
      end return;
   end Get;

   ----------------------------------------------------------------------------

   overriding
   function Image (Object : CPU_Tensor) return String is
      package L1 renames Ada.Characters.Latin_1;
      package SU renames Ada.Strings.Unbounded;

      Row_Count : constant := 5;
      Count     : constant Natural := Object.Elements;

      Result : SU.Unbounded_String;
   begin
      SU.Append (Result, "tensor([");
      case Object.Axes is
         when 1 =>
            for I in 1 .. Count loop
               declare
                  First_Element_Of_Row : constant Boolean := (I - 1) mod Row_Count = 0;
                  Last_Element_Of_Row  : constant Boolean := (I - 0) mod Row_Count = 0;

                  Value : Element_Type renames Object.Data (Data_Vectors (I)) (Data_Offset (I));
               begin
                  if First_Element_Of_Row then
                     SU.Append (Result, (if I = 1 then "" else "        "));
                  end if;
                  case Object.Kind is
                     when Float_Type | Int_Type =>
                        SU.Append (Result, (if Value'Valid then Value'Image else "     invalid"));
                     when Bool_Type =>
                        SU.Append (Result, "       " &
                          (if not Value'Valid or else Value /= 0.0 then " True" else "False"));
                  end case;
                  if I < Count then
                     SU.Append (Result, ",");
                     if Last_Element_Of_Row then
                        SU.Append (Result, L1.LF);
                     end if;
                  end if;
               end;
            end loop;
         when 2 =>
            declare
               Rows    : constant Natural := Object.Rows;
               Columns : constant Natural := Object.Columns;

               Object_Data : Element_Array (1 .. Count)
                 with Import, Convention => Ada, Address => Object.Data'Address;
            begin
               for I in 1 .. Rows loop
                  SU.Append (Result, (if I = 1 then "" else "        "));
                  SU.Append (Result, "[");
                  for J in 1 .. Columns loop
                     declare
                        Value : Element_Type renames Object_Data ((I - 1) * Columns + J);
                     begin
                        case Object.Kind is
                           when Float_Type | Int_Type =>
                              SU.Append (Result,
                                (if Value'Valid then Value'Image else "     invalid"));
                           when Bool_Type =>
                              SU.Append (Result, "       " &
                                (if not Value'Valid or else Value /= 0.0 then
                                   " True"
                                 else
                                   "False"));
                        end case;
                     end;
                     if J < Columns then
                        SU.Append (Result, ",");
                     end if;
                  end loop;
                  SU.Append (Result, "]");
                  if I < Rows then
                     SU.Append (Result, ",");
                     SU.Append (Result, L1.LF);
                  end if;
               end loop;
            end;
         when others =>
            raise Not_Implemented_Yet;  --  FIXME
      end case;
      SU.Append (Result, "])");

      return SU.To_String (Result);
   end Image;

   overriding
   function Shape (Object : CPU_Tensor) return Tensor_Shape is (Object.Shape);

   overriding
   function Elements (Object : CPU_Tensor) return Natural is (Elements (Object.Shape));

   overriding
   function Axes (Object : CPU_Tensor) return Tensor_Axis is (Object.Axes);

   overriding
   function Empty (Shape : Tensor_Shape) return CPU_Tensor is (Without_Data (Shape));

   overriding
   function Fill (Shape : Tensor_Shape; Value : Element) return CPU_Tensor is
      Vector : constant Vector_Type := (others => Value);
   begin
      return Result : CPU_Tensor := Without_Data (Shape) do
         Result.Data := (others => Vector);
      end return;
   end Fill;

   overriding function Zeros (Shape : Tensor_Shape) return CPU_Tensor renames Operations.Zeros;
   overriding function Zeros (Elements : Positive)  return CPU_Tensor renames Operations.Zeros;

   overriding function Ones (Shape : Tensor_Shape) return CPU_Tensor renames Operations.Ones;
   overriding function Ones (Elements : Positive)  return CPU_Tensor renames Operations.Ones;

   overriding
   function To_Tensor (Elements : Element_Array; Shape : Tensor_Shape) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Shape) do
         declare
            Result_Data : Element_Array (1 .. Result.Elements)
              with Import, Convention => Ada, Address => Result.Data'Address;
         begin
            Result_Data := Elements;
         end;
      end return;
   end To_Tensor;

   overriding
   function To_Tensor (Elements : Element_Array) return CPU_Tensor renames Operations.To_Tensor;

   overriding
   function To_Boolean_Tensor
     (Booleans : Boolean_Array;
      Shape    : Tensor_Shape) return CPU_Tensor
   is
      Elements : Element_Array (Booleans'Range);
   begin
      for Index in Elements'Range loop
         Elements (Index) := (if Booleans (Index) then 1.0 else 0.0);
      end loop;

      return To_Tensor (Elements, Shape) > 0.0;
   end To_Boolean_Tensor;

   overriding
   function To_Boolean_Tensor (Booleans : Boolean_Array) return CPU_Tensor
     renames Operations.To_Boolean_Tensor;

   overriding
   function Linear_Space
     (Start, Stop : Element;
      Count       : Positive;
      Interval    : Interval_Kind := Closed) return CPU_Tensor
   is
      Shape : constant Tensor_Shape := (1 => Count);

      Step : constant Element :=
        (if Count > 1 then (Stop - Start) / Element (Count - (case Interval is
                                                                when Closed    => 1,
                                                                when Half_Open => 0))
                      else 0.0);
   begin
      return Result : CPU_Tensor := Without_Data (Shape) do
         declare
            Result_Data : Element_Array (1 .. Elements (Shape))
              with Import, Convention => Ada, Address => Result.Data'Address;
         begin
            for Index in 1 .. Count loop
               Result_Data (Index) := Start + Step * Element (Index - 1);
            end loop;
         end;
      end return;
   end Linear_Space;

   overriding
   function Log_Space
     (Start, Stop : Element;
      Count       : Positive;
      Interval    : Interval_Kind := Closed;
      Base        : Element := 10.0) return CPU_Tensor
   is
      Shape : constant Tensor_Shape := (1 => Count);

      Step : constant Element :=
        (if Count > 1 then (Stop - Start) / Element (Count - (case Interval is
                                                                when Closed    => 1,
                                                                when Half_Open => 0))
                      else 0.0);
   begin
      return Result : CPU_Tensor := Without_Data (Shape) do
         declare
            Result_Data : Element_Array (1 .. Elements (Shape))
              with Import, Convention => Ada, Address => Result.Data'Address;

            use EF;
         begin
            for Index in 1 .. Count loop
               Result_Data (Index) := Base ** (Start + Step * Element (Index - 1));
            end loop;
         end;
      end return;
   end Log_Space;

   overriding
   function Geometric_Space
     (Start, Stop : Element;
      Count       : Positive;
      Interval    : Interval_Kind := Closed;
      Base        : Element := 10.0) return CPU_Tensor renames Operations.Geometric_Space;

   overriding
   function Array_Range (Start, Stop : Element; Step : Element := 1.0) return CPU_Tensor
     renames Operations.Array_Range;

   overriding
   function Array_Range (Stop : Element) return CPU_Tensor renames Operations.Array_Range;

   overriding
   function Identity (Size : Positive; Offset : Integer := 0) return CPU_Tensor
     renames Operations.Identity;

   overriding
   function Identity (Rows, Columns : Positive; Offset : Integer := 0) return CPU_Tensor is
      Shape : constant Tensor_Shape := (1 => Rows, 2 => Columns);

      Max_Size : constant Positive := Positive'Max (Rows, Columns);

      Zero_Vector : constant Vector_Type := (others => 0.0);
   begin
      return Result : CPU_Tensor :=
        (Axes => Shape'Length,
         Size       => Data_Vectors (Shape),
         Kind       => Float_Type,
         Shape      => Shape,
         Data       => (others => Zero_Vector))
      do
         if Offset in -(Max_Size - 1) .. Max_Size - 1 then
            declare
               Result_Data : Element_Array (1 .. Elements (Shape))
                 with Import, Convention => Ada, Address => Result.Data'Address;

               Index : Integer := Offset + 1;
            begin
               for I in 1 .. Rows loop
                  if Index in Result_Data'Range then
                     Result_Data (Index) := 1.0;
                  end if;
                  Index := Index + Columns + 1;
               end loop;
            end;
         end if;
      end return;
   end Identity;

   overriding
   function Upper_Triangular (Object : CPU_Tensor; Offset : Integer := 0) return CPU_Tensor
     renames Operations.Upper_Triangular;

   overriding
   function Main_Diagonal (Object : CPU_Tensor; Offset : Integer := 0) return CPU_Tensor is
      Rows    : constant Positive := Object.Rows;
      Columns : constant Positive := Object.Columns;

      Shape : constant Tensor_Shape := (1 => Positive'Min (Rows, Columns));
   begin
      return Result : CPU_Tensor := Without_Data (Shape) do
         declare
            Result_Data : Element_Array (1 .. Elements (Shape))
              with Import, Convention => Ada, Address => Result.Data'Address;

            Object_Data : Element_Array (1 .. Object.Elements)
              with Import, Convention => Ada, Address => Object.Data'Address;

            Index : Integer := Offset + 1;
         begin
            for I in Result_Data'Range loop
               if Index in Object_Data'Range then
                  Result_Data (I) := Object_Data (Index);
               else
                  Result_Data (I) := 0.0;
               end if;
               Index := Index + Columns + 1;
            end loop;
         end;
      end return;
   end Main_Diagonal;

   overriding
   function Diagonal (Elements : Element_Array; Offset : Integer := 0) return CPU_Tensor is
      Size : constant Positive := Elements'Length;

      Shape : constant Tensor_Shape := (1 .. 2 => Size);

      Zero_Vector : constant Vector_Type := (others => 0.0);
   begin
      return Result : CPU_Tensor :=
        (Axes => Shape'Length,
         Size       => Data_Vectors (Shape),
         Kind       => Float_Type,
         Shape      => Shape,
         Data       => (others => Zero_Vector))
      do
         if Offset in -(Size - 1) .. Size - 1 then
            declare
               Result_Data : Element_Array (1 .. Size * Size)
                 with Import, Convention => Ada, Address => Result.Data'Address;

               Index : Integer := Offset + 1;
            begin
               for I in Elements'Range loop
                  if Index in Result_Data'Range then
                     Result_Data (Index) := Elements (I);
                  end if;
                  Index := Index + Size + 1;
               end loop;
            end;
         end if;
      end return;
   end Diagonal;

   overriding
   function Diagonal (Elements : CPU_Tensor; Offset : Integer := 0) return CPU_Tensor is
      Object_Data : Element_Array (1 .. Elements.Elements)
        with Import, Convention => Ada, Address => Elements.Data'Address;
   begin
      return Diagonal (Object_Data, Offset);
   end Diagonal;

   overriding
   function Trace (Object : CPU_Tensor; Offset : Integer := 0) return Element
     renames Operations.Trace;

   overriding
   function Reshape (Object : CPU_Tensor; Shape : Tensor_Shape) return CPU_Tensor is
     (Axes => Shape'Length,
      Size       => Object.Size,
      Kind       => Object.Kind,
      Shape      => Shape,
      Data       => Object.Data);

   overriding
   function Reshape (Object : CPU_Tensor; Elements : Positive) return CPU_Tensor
     renames Operations.Reshape;

   overriding
   function Flatten (Object : CPU_Tensor) return CPU_Tensor renames Operations.Flatten;

   overriding
   function Concatenate
     (Left, Right : CPU_Tensor;
      Axis   : Tensor_Axis) return CPU_Tensor
   is
      Shape : constant Tensor_Shape := Add (Left.Shape, Right.Shape, Axis);
      pragma Assert (Elements (Shape) = Left.Elements + Right.Elements);
   begin
      return Result : CPU_Tensor := Without_Data (Shape, Left.Kind) do
         declare
            Result_Data : Element_Array (1 .. Result.Elements)
              with Import, Convention => Ada, Address => Result.Data'Address;

            Left_Data : Element_Array (1 .. Left.Elements)
              with Import, Convention => Ada, Address => Left.Data'Address;

            Right_Data : Element_Array (1 .. Right.Elements)
              with Import, Convention => Ada, Address => Right.Data'Address;
         begin
            case Axis is
               when 1 =>
                  Result_Data (1 .. Left.Elements) := Left_Data;
                  Result_Data (Left.Elements + 1 .. Result_Data'Last) := Right_Data;
               when 2 =>
                  declare
                     Rows : constant Positive := Left.Rows;

                     Columns_Left  : constant Positive := Left.Columns;
                     Columns_Right : constant Positive := Right.Columns;
                  begin
                     for Index in 1 .. Rows loop
                        declare
                           Base_Left_Index : constant Natural :=
                             (Index - 1) * (Columns_Left + Columns_Right);
                           Base_Right_Index : constant Natural := Base_Left_Index + Columns_Left;
                           Left_Index  : constant Natural := (Index - 1) * Columns_Left;
                           Right_Index : constant Natural := (Index - 1) * Columns_Right;
                        begin
                           Result_Data (Base_Left_Index + 1 .. Base_Left_Index + Columns_Left)
                             := Left_Data (Left_Index + 1 .. Left_Index + Columns_Left);
                           Result_Data (Base_Right_Index + 1 .. Base_Right_Index + Columns_Right)
                             := Right_Data (Right_Index + 1 .. Right_Index + Columns_Right);
                        end;
                     end loop;
                  end;
               when others =>
                  raise Not_Implemented_Yet;  --  FIXME
            end case;
         end;
      end return;
   end Concatenate;

   overriding
   function "&" (Left, Right : CPU_Tensor) return CPU_Tensor renames Operations."&";

   ----------------------------------------------------------------------------
   --                            Matrix operations                           --
   ----------------------------------------------------------------------------

   procedure Multiply_Add
     (Result : in out Element_Array;
      Left   : Element;
      Right  : Element_Array)
   is
      Left_Vector : constant Vector_Type := (others => Left);

      subtype Vector_Elements is Element_Array (1 .. Vector_Type'Length);

      function Convert is new Ada.Unchecked_Conversion (Vector_Elements, Vector_Type);
      function Convert is new Ada.Unchecked_Conversion (Vector_Type, Vector_Elements);

      Last_Vector_Index : constant Positive := Data_Vectors (Right'Length);

      Padding : constant Natural :=
        Data_Padding (Last_Vector_Index, Right'Length);
   begin
      for Index in 1 .. Last_Vector_Index - (if Padding > 0 then 1 else 0) loop
         declare
            Vector_Index : constant Integer := (Index - 1) * Vector_Type'Length - 1;

            Result_Index : constant Natural := Vector_Index + Result'First;
            Right_Index  : constant Natural := Vector_Index + Right'First;

            Result_Vector : constant Vector_Type :=
              Convert (Result (Result_Index + 1 .. Result_Index + Vector_Type'Length));
            Right_Vector : constant Vector_Type :=
              Convert (Right (Right_Index + 1 .. Right_Index + Vector_Type'Length));
         begin
            Result (Result_Index + 1 .. Result_Index + Vector_Type'Length) :=
              Convert (Result_Vector + Left_Vector * Right_Vector);
         end;
      end loop;

      if Padding > 0 then
         declare
            Vector_Index : constant Integer := (Last_Vector_Index - 1) * Vector_Type'Length - 1;

            Result_Index : constant Natural := Vector_Index + Result'First;
            Right_Index  : constant Natural := Vector_Index + Right'First;

            Last_Count : constant Positive := Vector_Type'Length - Padding;
         begin
            for I in 1 .. Last_Count loop
               Result (Result_Index + I) :=
                 Result (Result_Index + I) + Left * Right (Right_Index + I);
            end loop;
         end;
      end if;
   end Multiply_Add;

   overriding
   function "*" (Left, Right : CPU_Tensor) return CPU_Tensor is
      --  m x n * n x p
      --      ^   ^
      --      |___|
      --     (Count)
      Left_Rows     : constant Natural := (if Left.Axes = 2 then Left.Rows else 1);
      Count         : constant Natural := Right.Rows;
      Right_Columns : constant Natural := (if Right.Axes = 2 then Right.Columns else 1);

      Shape : constant Tensor_Shape :=
         (case Right.Axes is
            when 1 => (1 => Left_Rows),
            when 2 => (1 => Left_Rows, 2 => Right_Columns),
            when others => raise Not_Implemented_Yet);  --  FIXME
   begin
      --  Matrix-matrix or matrix-vector or vector-matrix multiplication
      return Result : CPU_Tensor := Zeros (Shape) do
         declare
            Result_Data : Element_Array (1 .. Result.Elements)
              with Import, Convention => Ada, Address => Result.Data'Address;
            Left_Data : Element_Array (1 .. Left.Elements)
              with Import, Convention => Ada, Address => Left.Data'Address;
            Right_Data : Element_Array (1 .. Right.Elements)
              with Import, Convention => Ada, Address => Right.Data'Address;
         begin
            for Row_Index in 1 .. Left_Rows loop
               --  Result (Row_Index) := Left (Row_Index) * Right;
               declare
                  Result_Index : constant Natural := To_Index ((Row_Index, 1), Right_Columns) - 1;
               begin
                  for Column_Index in 1 .. Count loop
                     declare
                        Right_Index : constant Natural :=
                          To_Index ((Column_Index, 1), Right_Columns) - 1;
                     begin
                        --  Left_Value := Left (Row_Index, Column_Index)
                        --  Result (Row_Index) := @ + Left_Value * Right (Row_Index)
                        Multiply_Add
                          (Result_Data (Result_Index + 1 .. Result_Index + Right_Columns),
                           Left_Data (To_Index ((Row_Index, Column_Index), Count)),
                           Right_Data (Right_Index + 1 .. Right_Index + Right_Columns));
                     end;
                  end loop;
               end;
            end loop;
         end;
      end return;
   end "*";

   overriding
   function "*" (Left, Right : CPU_Tensor) return Element is
      Result : Element_Type := 0.0;

      Padding : constant Natural :=
        Data_Padding (Size => Left.Size, Count => Left.Elements);

      Last_Left  : constant Vector_Type := Reset_Padding (Left, Padding, 0.0);
      Last_Right : constant Vector_Type := Reset_Padding (Right, Padding, 0.0);

      type Sum_Index_Type is mod 8;

      Sums : array (Sum_Index_Type) of Vector_Type := (others => (others => 0.0));
      --  TODO Do pairwise summation recursively
   begin
      for Index in Left.Data'First .. Left.Data'Last - 1 loop
         declare
            Sum_Index : constant Sum_Index_Type := Sum_Index_Type (Index mod Sums'Length);
         begin
            Sums (Sum_Index) := Sums (Sum_Index) + Left.Data (Index) * Right.Data (Index);
         end;
      end loop;

      declare
         Sum_Index : constant Sum_Index_Type := Sum_Index_Type (Left.Data'Last mod Sums'Length);
      begin
         Sums (Sum_Index) := Sums (Sum_Index) + Last_Left * Last_Right;
      end;

      for Value of Sums loop
         Result := Result + Sum (Value);
      end loop;

      return Element (Result);
   end "*";

   overriding function "**" (Left : CPU_Tensor; Right : Integer) return CPU_Tensor
     renames Operations."**";

   overriding
   function Outer (Left, Right : CPU_Tensor) return CPU_Tensor is
      Shape : constant Tensor_Shape := (1 => Left.Elements, 2 => Right.Elements);
   begin
      return Result : CPU_Tensor := Without_Data (Shape) do
         declare
            Result_Data : Element_Array (1 .. Result.Elements)
              with Import, Convention => Ada, Address => Result.Data'Address;

            Left_Data : Element_Array (1 .. Left.Elements)
              with Import, Convention => Ada, Address => Left.Data'Address;

            Columns : constant Positive := Right.Elements;
         begin
            for Index in 1 .. Left.Elements loop
               declare
                  Row : constant CPU_Tensor := Left_Data (Index) * Right;

                  Row_Data : Element_Array (1 .. Columns)
                    with Import, Convention => Ada, Address => Row.Data'Address;

                  Base_Index : constant Natural := (Index - 1) * Columns;
               begin
                  Result_Data (Base_Index + 1 .. Base_Index + Columns) := Row_Data;
               end;
            end loop;
         end;
      end return;
   end Outer;

   overriding
   function Inverse (Object : CPU_Tensor) return CPU_Tensor renames Operations.Inverse;

   overriding
   function Transpose (Object : CPU_Tensor) return CPU_Tensor is
      Shape : constant Tensor_Shape :=
        (1 => Object.Columns,
         2 => Object.Rows);
   begin
      return Result : CPU_Tensor := Without_Data (Shape) do
         declare
            Result_Data : Element_Array (1 .. Result.Elements)
              with Import, Convention => Ada, Address => Result.Data'Address;

            Object_Data : Element_Array (1 .. Object.Elements)
              with Import, Convention => Ada, Address => Object.Data'Address;

            Rows    : constant Natural := Object.Rows;
            Columns : constant Natural := Object.Columns;

            Result_Columns : Natural renames Rows;
         begin
            for Row_Index in 1 .. Rows loop
               for Column_Index in 1 .. Columns loop
                  Result_Data (To_Index ((Column_Index, Row_Index), Result_Columns)) :=
                    Object_Data (To_Index ((Row_Index, Column_Index), Columns));
               end loop;
            end loop;
         end;
      end return;
   end Transpose;

   ----------------------------------------------------------------------------

   overriding
   function Solve (A, B : CPU_Tensor; Solution : out Solution_Kind) return CPU_Tensor
     renames Operations.Solve;

   overriding
   function Solve (A, B : CPU_Tensor; Form : Triangular_Form) return CPU_Tensor
     renames Operations.Solve;

   overriding
   function Divide_By (B, A : CPU_Tensor) return CPU_Tensor
     renames Operations.Divide_By;

   overriding
   function Divide_By (B, A : CPU_Tensor; Form : Triangular_Form) return CPU_Tensor
     renames Operations.Divide_By;

   overriding
   function QR (Object : CPU_Tensor) return CPU_Tensor
     renames Operations.QR;

   overriding
   function QR (Object : CPU_Tensor; Mode : QR_Mode := Reduced) return QR_Factorization'Class
     renames Operations.QR;

   overriding
   function QR_For_Least_Squares (Object : CPU_Tensor) return QR_Factorization'Class
     renames Operations.QR_For_Least_Squares;

   overriding
   function Least_Squares (Object : QR_Factorization'Class; B : CPU_Tensor) return CPU_Tensor
     renames Operations.Least_Squares;

   overriding
   function Least_Squares (A, B : CPU_Tensor) return CPU_Tensor
     renames Operations.Least_Squares;

   overriding
   function Constrained_Least_Squares (A, B, C, D : CPU_Tensor) return CPU_Tensor
     renames Operations.Constrained_Least_Squares;

   overriding
   function Cholesky (Object : CPU_Tensor; Form : Triangular_Form := Lower) return CPU_Tensor
     renames Operations.Cholesky;

   overriding
   function Cholesky_Update
     (R, V : CPU_Tensor;
      Mode : Update_Mode) return CPU_Tensor renames Operations.Cholesky_Update;

   ----------------------------------------------------------------------------
   --                            Vector operations                           --
   ----------------------------------------------------------------------------

   overriding
   function Norm (Object : CPU_Tensor) return Element renames Operations.Norm;

   overriding
   function Normalize (Object : CPU_Tensor) return CPU_Tensor renames Operations.Normalize;

   overriding
   function Standardize (Object : CPU_Tensor) return CPU_Tensor renames Operations.Standardize;

   overriding
   function Correlation_Coefficient (Left, Right : CPU_Tensor) return Correlation_Element
     renames Operations.Correlation_Coefficient;

   ----------------------------------------------------------------------------
   --                         Element-wise operations                        --
   ----------------------------------------------------------------------------

   overriding function "+" (Left, Right : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Left) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Left.Data (Index) + Right.Data (Index);
         end loop;
      end return;
   end "+";

   overriding function "-" (Left, Right : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Left) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Left.Data (Index) - Right.Data (Index);
         end loop;
      end return;
   end "-";

   overriding function "/" (Left, Right : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Left) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Left.Data (Index) / Right.Data (Index);
         end loop;
      end return;
   end "/";

   overriding function Divide_Or_Zero (Left, Right : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Left) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Divide_Or_Zero (Left.Data (Index), Right.Data (Index));
         end loop;
      end return;
   end Divide_Or_Zero;

   function Power (Left, Right : Element) return Element is
     (if Left = 0.0 and Right = 0.0 then 1.0 else EF."**" (Left, Right));
   --  0.0 ** X = 1.0 if X = 0.0 (EF."**" raises Argument_Error instead of returning 1.0)
   --  0.0 ** X = 0.0 if X > 0.0

   overriding function "**" (Left, Right : CPU_Tensor) return CPU_Tensor is
--     (Exp (Multiply (Right, Log (Left))));

      Padding : constant Natural :=
        Data_Padding (Size => Left.Size, Count => Elements (Left.Shape));

      Last_Left  : constant Vector_Type := Reset_Padding (Left, Padding, 0.0);
      Last_Right : constant Vector_Type := Reset_Padding (Right, Padding, 1.0);
   begin
      return Result : CPU_Tensor := Without_Data (Left) do
         for Index in Result.Data'First .. Result.Data'Last - 1 loop
            for Offset in Vector_Type'Range loop
               declare
                  Left_Element  : Element renames Left.Data (Index) (Offset);
                  Right_Element : Element renames Right.Data (Index) (Offset);
               begin
                  Result.Data (Index) (Offset) := Power (Left_Element, Right_Element);
               end;
            end loop;
         end loop;

         for Offset in Vector_Type'Range loop
            Result.Data (Result.Data'Last) (Offset) :=
              Power (Last_Left (Offset), Last_Right (Offset));
         end loop;
      end return;
   end "**";

   overriding function "**" (Left : CPU_Tensor; Right : Element) return CPU_Tensor is
--     (Exp (Right * Log (Left)));
      use EF;
   begin
      if Right = 0.0 then
         return Ones (Left.Shape);
      elsif Right = 1.0 then
         return Left;
      else
         return Result : CPU_Tensor := Without_Data (Left) do
            for Index in Result.Data'Range loop
               for Offset in Vector_Type'Range loop
                  Result.Data (Index) (Offset) := Left.Data (Index) (Offset) ** Right;
               end loop;
            end loop;
         end return;
      end if;
   end "**";

   overriding function "**" (Left : Element; Right : CPU_Tensor) return CPU_Tensor is
--     (Exp (Right * EF.Log (Left)));

      Padding : constant Natural :=
        Data_Padding (Size => Right.Size, Count => Elements (Right.Shape));

      --  EF."**" raises Constraint_Error if Left = 0.0 and element
      --  in the padding happens to be < 0.0
      Last_Right : constant Vector_Type := Reset_Padding (Right, Padding, 1.0);
   begin
      if Left = 1.0 then
         return Ones (Right.Shape);
      else
         return Result : CPU_Tensor := Without_Data (Right) do
            for Index in Result.Data'First .. Result.Data'Last - 1 loop
               for Offset in Vector_Type'Range loop
                  declare
                     Right_Element : Element renames Right.Data (Index) (Offset);
                  begin
                     Result.Data (Index) (Offset) := Power (Left, Right_Element);
                  end;
               end loop;
            end loop;

            for Offset in Vector_Type'Range loop
               Result.Data (Result.Data'Last) (Offset) := Power (Left, Last_Right (Offset));
            end loop;
         end return;
      end if;
   end "**";

   overriding function "*" (Left : CPU_Tensor; Right : Element) return CPU_Tensor is
      Right_Vector : constant Vector_Type := (others => Right);
   begin
      return Result : CPU_Tensor := Without_Data (Left) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Left.Data (Index) * Right_Vector;
         end loop;
      end return;
   end "*";

   overriding function "/" (Left : Element; Right : CPU_Tensor) return CPU_Tensor is
      Left_Vector : constant Vector_Type := (others => Left);
   begin
      return Result : CPU_Tensor := Without_Data (Right) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Left_Vector / Right.Data (Index);
         end loop;
      end return;
   end "/";

   overriding function "/" (Left : CPU_Tensor; Right : Element) return CPU_Tensor is
      Right_Vector : constant Vector_Type := (others => Right);
   begin
      return Result : CPU_Tensor := Without_Data (Left) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Left.Data (Index) / Right_Vector;
         end loop;
      end return;
   end "/";

   overriding function "+" (Left : CPU_Tensor; Right : Element) return CPU_Tensor is
      Right_Vector : constant Vector_Type := (others => Right);
   begin
      return Result : CPU_Tensor := Without_Data (Left) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Left.Data (Index) + Right_Vector;
         end loop;
      end return;
   end "+";

   overriding function "-" (Left : Element; Right : CPU_Tensor) return CPU_Tensor is
      Left_Vector : constant Vector_Type := (others => Left);
   begin
      return Result : CPU_Tensor := Without_Data (Right) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Left_Vector - Right.Data (Index);
         end loop;
      end return;
   end "-";

   overriding function "-" (Object : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Object) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := -Object.Data (Index);
         end loop;
      end return;
   end "-";

   overriding
   function "*" (Left : Element; Right : CPU_Tensor) return CPU_Tensor renames Operations."*";

   overriding
   function "+" (Left : Element; Right : CPU_Tensor) return CPU_Tensor renames Operations."+";

   overriding
   function "-" (Left : CPU_Tensor; Right : Element) return CPU_Tensor renames Operations."-";

   overriding
   function "mod" (Left, Right : CPU_Tensor) return CPU_Tensor renames Operations."mod";

   overriding
   function "rem" (Left, Right : CPU_Tensor) return CPU_Tensor renames Operations."rem";

   overriding
   function "mod" (Left : CPU_Tensor; Right : Element) return CPU_Tensor renames Operations."mod";

   overriding
   function "rem" (Left : CPU_Tensor; Right : Element) return CPU_Tensor renames Operations."rem";

   overriding function "abs" (Object : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Object) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := abs Object.Data (Index);
         end loop;
      end return;
   end "abs";

   overriding function Multiply (Left, Right : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Left) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Left.Data (Index) * Right.Data (Index);
         end loop;
      end return;
   end Multiply;

   overriding
   function Power (Left : CPU_Tensor; Right : Integer) return CPU_Tensor renames Operations.Power;

   overriding
   function Min (Left : Element; Right : CPU_Tensor) return CPU_Tensor renames Operations.Min;

   overriding
   function Max (Left : Element; Right : CPU_Tensor) return CPU_Tensor renames Operations.Max;

   overriding function Min (Left : CPU_Tensor; Right : Element) return CPU_Tensor is
      Right_Vector : constant Vector_Type := (others => Right);
   begin
      return Result : CPU_Tensor := Without_Data (Left) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Min (Left.Data (Index), Right_Vector);
         end loop;
      end return;
   end Min;

   overriding function Max (Left : CPU_Tensor; Right : Element) return CPU_Tensor is
      Right_Vector : constant Vector_Type := (others => Right);
   begin
      return Result : CPU_Tensor := Without_Data (Left) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Max (Left.Data (Index), Right_Vector);
         end loop;
      end return;
   end Max;

   overriding function Sqrt (Object : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Object) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Sqrt (Object.Data (Index));
         end loop;
      end return;
   end Sqrt;

   overriding function Ceil (Object : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Object) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Ceil (Object.Data (Index));
         end loop;
      end return;
   end Ceil;

   overriding function Floor (Object : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Object) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Floor (Object.Data (Index));
         end loop;
      end return;
   end Floor;

   overriding function Round (Object : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Object) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Round (Object.Data (Index));
         end loop;
      end return;
   end Round;

   overriding function Truncate (Object : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Object) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Truncate (Object.Data (Index));
         end loop;
      end return;
   end Truncate;

   overriding function Exp (Object : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Object) do
         for Index in Result.Data'Range loop
            for Offset in Vector_Type'Range loop
               Result.Data (Index) (Offset) := EF.Exp (Object.Data (Index) (Offset));
            end loop;
         end loop;
      end return;
   end Exp;

   overriding function Log (Object : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Object) do
         for Index in Result.Data'Range loop
            for Offset in Vector_Type'Range loop
               Result.Data (Index) (Offset) := EF.Log (Object.Data (Index) (Offset));
            end loop;
         end loop;
      end return;
   end Log;

   overriding function Log10 (Object : CPU_Tensor) return CPU_Tensor renames Operations.Log10;

   overriding function Log2 (Object : CPU_Tensor) return CPU_Tensor renames Operations.Log2;

   ----------------------------------------------------------------------------
   --                              Trigonometry                              --
   ----------------------------------------------------------------------------

   overriding function Sin (Object : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Object) do
         for Index in Result.Data'Range loop
            for Offset in Vector_Type'Range loop
               Result.Data (Index) (Offset) := EF.Sin (Object.Data (Index) (Offset));
            end loop;
         end loop;
      end return;
   end Sin;

   overriding function Cos (Object : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Object) do
         for Index in Result.Data'Range loop
            for Offset in Vector_Type'Range loop
               Result.Data (Index) (Offset) := EF.Cos (Object.Data (Index) (Offset));
            end loop;
         end loop;
      end return;
   end Cos;

   overriding function Tan (Object : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Object) do
         for Index in Result.Data'Range loop
            for Offset in Vector_Type'Range loop
               Result.Data (Index) (Offset) := EF.Tan (Object.Data (Index) (Offset));
            end loop;
         end loop;
      end return;
   end Tan;

   overriding function Arcsin (Object : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Object) do
         for Index in Result.Data'Range loop
            for Offset in Vector_Type'Range loop
               Result.Data (Index) (Offset) := EF.Arcsin (Object.Data (Index) (Offset));
            end loop;
         end loop;
      end return;
   end Arcsin;

   overriding function Arccos (Object : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Object) do
         for Index in Result.Data'Range loop
            for Offset in Vector_Type'Range loop
               Result.Data (Index) (Offset) := EF.Arccos (Object.Data (Index) (Offset));
            end loop;
         end loop;
      end return;
   end Arccos;

   overriding function Arctan (Left, Right : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Left) do
         for Index in Result.Data'Range loop
            for Offset in Vector_Type'Range loop
               Result.Data (Index) (Offset) :=
                 EF.Arctan (Left.Data (Index) (Offset), Right.Data (Index) (Offset));
            end loop;
         end loop;
      end return;
   end Arctan;

   overriding function Degrees (Object : CPU_Tensor) return CPU_Tensor renames Operations.Degrees;

   overriding function Radians (Object : CPU_Tensor) return CPU_Tensor renames Operations.Radians;

   ----------------------------------------------------------------------------
   --                               Reductions                               --
   ----------------------------------------------------------------------------

   overriding
   function Reduce_Associative
     (Object    : CPU_Tensor;
      Subject   : Expression'Class;
      Initial   : Element) return Element
   is
      CPU_Subject : constant Expression_Type := Expression_Type (Subject);

      Max_Length_Sequential : constant Positive := 128 / Vector_Type'Length;

      function Pairwise (Lower, Upper : Positive) return Vector_Type is
         Length : constant Positive := Upper - Lower + 1;
      begin
         if Length <= Max_Length_Sequential then
            declare
               Result : Vector_Type := Object.Data (Lower);
            begin
               for Index in Lower + 1 .. Upper loop
                  Result := Apply (CPU_Subject, Result, Object.Data (Index));
               end loop;
               return Result;
            end;
         else
            declare
               Half_Index : constant Positive := Lower + Length / 2;
            begin
               return Apply
                 (CPU_Subject,
                  Pairwise (Lower, Half_Index - 1),
                  Pairwise (Half_Index, Upper));
            end;
         end if;
      end Pairwise;

      Padding : constant Natural :=
        Data_Padding (Size => Object.Size, Count => Object.Elements);

      Result : Element_Type := Initial;
   begin
      if Object.Data'Last = 0 then
         return Result;
      end if;

      if Object.Elements > Vector_Type'Length then
         for Element of Pairwise (Object.Data'First, Object.Data'Last - 1) loop
            Result := Apply (CPU_Subject, Result, Element);
         end loop;
      else
         --  Do not perform pairwise applying of expression because Object.Data has only 1 vector,
         --  which is added to Result below
         null;
      end if;

      for Index in Vector_Index_Type'First .. From_Last (Padding) loop
         Result := Apply (CPU_Subject, Result, Object.Data (Object.Data'Last) (Index));
      end loop;

      return Result;
   end Reduce_Associative;

   overriding
   function Reduce_Associative
     (Object  : CPU_Tensor;
      Subject : Expression'Class;
      Initial : Element;
      Axis    : Tensor_Axis) return CPU_Tensor is
   begin
      raise Program_Error;
      return Zeros ((1 => 1));  --  FIXME
   end Reduce_Associative;

   overriding
   function Reduce
     (Object    : CPU_Tensor;
      Subject   : Expression'Class;
      Initial   : Element) return Element
   is
      CPU_Subject : constant Expression_Type := Expression_Type (Subject);

      Data : Element_Array (1 .. Object.Elements)
        with Import, Convention => Ada, Address => Object.Data'Address;

      Result : Element_Type := Initial;
   begin
      for Value of Data loop
         Result := Apply (CPU_Subject, Result, Value);
      end loop;

      return Result;
   end Reduce;

   overriding
   function Reduce
     (Object  : CPU_Tensor;
      Subject : Expression'Class;
      Initial : Element;
      Axis    : Tensor_Axis) return CPU_Tensor is
   begin
      raise Program_Error;
      return Zeros ((1 => 1));  --  FIXME
   end Reduce;

   overriding function Sum (Object : CPU_Tensor) return Element renames Operations.Sum;

   overriding function Product (Object : CPU_Tensor) return Element renames Operations.Product;

   overriding
   function Sum (Object : CPU_Tensor; Axis : Tensor_Axis) return CPU_Tensor
     renames Operations.Sum;

   overriding
   function Product (Object : CPU_Tensor; Axis : Tensor_Axis) return CPU_Tensor
     renames Operations.Product;

   ----------------------------------------------------------------------------
   --                               Statistics                               --
   ----------------------------------------------------------------------------

   overriding function Min (Object : CPU_Tensor) return Element renames Operations.Min;

   overriding function Max (Object : CPU_Tensor) return Element renames Operations.Max;

   overriding function Quantile (Object : CPU_Tensor; P : Probability) return Element
     renames Operations.Quantile;

   overriding function Median (Object : CPU_Tensor) return Element
     renames Operations.Median;

   overriding function Mean (Object : CPU_Tensor) return Element
     renames Operations.Mean;

   overriding
   function Variance (Object : CPU_Tensor; Offset : Natural := 0) return Element
     renames Operations.Variance;

   overriding
   function Standard_Deviation (Object : CPU_Tensor; Offset : Natural := 0) return Element
     renames Operations.Standard_Deviation;

   overriding
   function Min (Left, Right : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Left) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Min (Left.Data (Index), Right.Data (Index));
         end loop;
      end return;
   end Min;

   overriding
   function Max (Left, Right : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Left) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Max (Left.Data (Index), Right.Data (Index));
         end loop;
      end return;
   end Max;

   overriding
   function Min (Object : CPU_Tensor; Axis : Tensor_Axis) return CPU_Tensor
     renames Operations.Min;

   overriding
   function Max (Object : CPU_Tensor; Axis : Tensor_Axis) return CPU_Tensor
     renames Operations.Max;

   overriding
   function Quantile
     (Object : CPU_Tensor;
      P      : Probability;
      Axis   : Tensor_Axis) return CPU_Tensor is
   begin
      raise Program_Error;
      return Zeros ((1 => 1));  --  FIXME
   end Quantile;

   overriding
   function Mean (Object : CPU_Tensor; Axis : Tensor_Axis) return CPU_Tensor
     renames Operations.Mean;

   overriding
   function Variance
     (Object : CPU_Tensor;
      Axis   : Tensor_Axis;
      Offset : Natural := 0) return CPU_Tensor
   renames Operations.Variance;

   overriding
   function Median (Object : CPU_Tensor; Axis : Tensor_Axis) return CPU_Tensor
     renames Operations.Median;

   overriding
   function Standard_Deviation
     (Object : CPU_Tensor;
      Axis   : Tensor_Axis;
      Offset : Natural := 0) return CPU_Tensor
   renames Operations.Standard_Deviation;

   ----------------------------------------------------------------------------
   --                                Logical                                 --
   ----------------------------------------------------------------------------

   overriding function And_Not (Left, Right : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Left, Kind => Bool_Type) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := And_Not (Left.Data (Index), Right.Data (Index));
         end loop;
         Result.Data (Result.Data'Last) := Disable_Padding (Result);
      end return;
   end And_Not;

   overriding function "and" (Left, Right : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Left, Kind => Left.Kind) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Left.Data (Index) and Right.Data (Index);
         end loop;
         Result.Data (Result.Data'Last) := Disable_Padding (Result);
      end return;
   end "and";

   overriding function "and" (Left : Element; Right : CPU_Tensor) return CPU_Tensor is
      Left_Vector : constant Vector_Type := (others => Left);
   begin
      return Result : CPU_Tensor := Without_Data (Right, Kind => Float_Type) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Left_Vector and Right.Data (Index);
         end loop;
         Result.Data (Result.Data'Last) := Disable_Padding (Result);
      end return;
   end "and";

   overriding function "or"  (Left, Right : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Left, Kind => Bool_Type) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Left.Data (Index) or Right.Data (Index);
         end loop;
         Result.Data (Result.Data'Last) := Disable_Padding (Result);
      end return;
   end "or";

   overriding function "xor" (Left, Right : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Left, Kind => Bool_Type) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Left.Data (Index) xor Right.Data (Index);
         end loop;
         Result.Data (Result.Data'Last) := Disable_Padding (Result);
      end return;
   end "xor";

   overriding function "not"  (Object : CPU_Tensor) return CPU_Tensor is
      Zero_Vector : constant Vector_Type := (others => 0.0);

      Mask : constant Vector_Type := Zero_Vector = Zero_Vector;
   begin
      return Result : CPU_Tensor := Without_Data (Object, Kind => Bool_Type) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := And_Not (Object.Data (Index), Mask);
         end loop;
         Result.Data (Result.Data'Last) := Disable_Padding (Result);
      end return;
   end "not";

   ----------------------------------------------------------------------------
   --                              Comparisons                               --
   ----------------------------------------------------------------------------

   overriding
   function "="  (Left : CPU_Tensor; Right : Element) return CPU_Tensor renames Operations."=";

   overriding
   function "/=" (Left : CPU_Tensor; Right : Element) return CPU_Tensor renames Operations."/=";

   overriding function ">"  (Left : CPU_Tensor; Right : Element) return CPU_Tensor is
      Right_Vector : constant Vector_Type := (others => Right);
   begin
      return Result : CPU_Tensor := Without_Data (Left, Kind => Bool_Type) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Left.Data (Index) > Right_Vector;
         end loop;
         Result.Data (Result.Data'Last) := Disable_Padding (Result);
      end return;
   end ">";

   overriding function "<"  (Left : CPU_Tensor; Right : Element) return CPU_Tensor is
      Right_Vector : constant Vector_Type := (others => Right);
   begin
      return Result : CPU_Tensor := Without_Data (Left, Kind => Bool_Type) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Left.Data (Index) < Right_Vector;
         end loop;
         Result.Data (Result.Data'Last) := Disable_Padding (Result);
      end return;
   end "<";

   overriding function ">=" (Left : CPU_Tensor; Right : Element) return CPU_Tensor is
      Right_Vector : constant Vector_Type := (others => Right);
   begin
      return Result : CPU_Tensor := Without_Data (Left, Kind => Bool_Type) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Left.Data (Index) >= Right_Vector;
         end loop;
         Result.Data (Result.Data'Last) := Disable_Padding (Result);
      end return;
   end ">=";

   overriding function "<=" (Left : CPU_Tensor; Right : Element) return CPU_Tensor is
      Right_Vector : constant Vector_Type := (others => Right);
   begin
      return Result : CPU_Tensor := Without_Data (Left, Kind => Bool_Type) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Left.Data (Index) <= Right_Vector;
         end loop;
         Result.Data (Result.Data'Last) := Disable_Padding (Result);
      end return;
   end "<=";

   ----------------------------------------------------------------------------

   overriding
   function "="  (Left : Element; Right : CPU_Tensor) return CPU_Tensor renames Operations."=";

   overriding
   function "/=" (Left : Element; Right : CPU_Tensor) return CPU_Tensor renames Operations."/=";

   overriding
   function ">"  (Left : Element; Right : CPU_Tensor) return CPU_Tensor renames Operations.">";

   overriding
   function "<"  (Left : Element; Right : CPU_Tensor) return CPU_Tensor renames Operations."<";

   overriding
   function ">=" (Left : Element; Right : CPU_Tensor) return CPU_Tensor renames Operations.">=";

   overriding
   function "<=" (Left : Element; Right : CPU_Tensor) return CPU_Tensor renames Operations."<=";

   ----------------------------------------------------------------------------

   overriding function "=" (Left, Right : CPU_Tensor) return Boolean renames Operations."=";

   overriding function "="  (Left, Right : CPU_Tensor) return CPU_Tensor is
      Zero_Vector : constant Vector_Type := (others => 0.0);

      Mask : constant Vector_Type := Zero_Vector = Zero_Vector;
   begin
      case Left.Kind is
         when Float_Type =>
            return (abs (Left - Right) <= Element_Type'Model_Epsilon);
         when Int_Type | Bool_Type =>
            return Result : CPU_Tensor := Without_Data (Left, Kind => Bool_Type) do
               for Index in Result.Data'Range loop
                  Result.Data (Index) :=
                    And_Not (Left.Data (Index) xor Right.Data (Index), Mask);
               end loop;
               Result.Data (Result.Data'Last) := Disable_Padding (Result);
            end return;
      end case;
   end "=";

   overriding function "/=" (Left, Right : CPU_Tensor) return CPU_Tensor is
   begin
      case Left.Kind is
         when Float_Type =>
            return (abs (Left - Right) > Element_Type'Model_Epsilon);
         when Int_Type | Bool_Type =>
            return Result : CPU_Tensor := Without_Data (Left, Kind => Bool_Type) do
               for Index in Result.Data'Range loop
                  Result.Data (Index) := Left.Data (Index) xor Right.Data (Index);
               end loop;
               Result.Data (Result.Data'Last) := Disable_Padding (Result);
            end return;
      end case;
   end "/=";

   overriding function ">"  (Left, Right : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Left, Kind => Bool_Type) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Left.Data (Index) > Right.Data (Index);
         end loop;
         Result.Data (Result.Data'Last) := Disable_Padding (Result);
      end return;
   end ">";

   overriding function "<"  (Left, Right : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Left, Kind => Bool_Type) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Left.Data (Index) < Right.Data (Index);
         end loop;
         Result.Data (Result.Data'Last) := Disable_Padding (Result);
      end return;
   end "<";

   overriding function ">=" (Left, Right : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Left, Kind => Bool_Type) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Left.Data (Index) >= Right.Data (Index);
         end loop;
         Result.Data (Result.Data'Last) := Disable_Padding (Result);
      end return;
   end ">=";

   overriding function "<=" (Left, Right : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Left, Kind => Bool_Type) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Left.Data (Index) <= Right.Data (Index);
         end loop;
         Result.Data (Result.Data'Last) := Disable_Padding (Result);
      end return;
   end "<=";

   ----------------------------------------------------------------------------

   overriding
   function All_Close
     (Left, Right        : CPU_Tensor;
      Relative_Tolerance : Element := 1.0e-05;
      Absolute_Tolerance : Element := Element_Type'Model_Epsilon) return Boolean
   renames Operations.All_Close;

   overriding
   function Any_True (Object : CPU_Tensor; Axis : Tensor_Axis) return CPU_Tensor is
   begin
      raise Program_Error;
      return Zeros ((1 => 1));  --  FIXME
   end Any_True;

   overriding
   function Any_True (Object : CPU_Tensor) return Boolean is
      Padding : constant Natural :=
        Data_Padding (Size => Object.Size, Count => Object.Elements);

      function All_Zeros (Vector : Vector_Type) return Boolean is
        (All_Zeros (Convert (Vector), Convert (Vector)));

      Zero_Vector : constant Vector_Type := (others => 0.0);
      Mask : Integer_Vector_Type := Convert (Zero_Vector = Zero_Vector);
   begin
      for Index in 1 .. Padding loop
         Mask := Shift_Elements_Right (Mask);
      end loop;

      return (for some Index in Object.Data'First .. Object.Data'Last - 1 =>
                not All_Zeros (Object.Data (Index)))
             or not All_Zeros (Object.Data (Object.Data'Last) and Convert (Mask));
   end Any_True;

   overriding
   function All_True (Object : CPU_Tensor; Axis : Tensor_Axis) return CPU_Tensor is
   begin
      raise Program_Error;
      return Zeros ((1 => 1));  --  FIXME
   end All_True;

   overriding
   function All_True (Object : CPU_Tensor) return Boolean is
      Padding : constant Natural :=
        Data_Padding (Size => Object.Size, Count => Object.Elements);

      function All_Ones (Vector : Vector_Type) return Boolean is
        (All_Ones (Convert (Vector), Convert (Vector = Vector)));

      Zero_Vector : constant Vector_Type := (others => 0.0);
      Mask : Integer_Vector_Type := Convert (Zero_Vector = Zero_Vector);
   begin
      for Index in 1 .. Vector_Type'Length - Padding loop
         Mask := Shift_Elements_Left (Mask);
      end loop;

      return (for all Index in Object.Data'First .. Object.Data'Last - 1 =>
                All_Ones (Object.Data (Index)))
             and All_Ones (Object.Data (Object.Data'Last) or Convert (Mask));
   end All_True;

   procedure Reset_Random (Seed : Duration) is
   begin
      Reset (Random_State, Seed);
   end Reset_Random;

   overriding function Random_Uniform (Shape : Tensor_Shape) return CPU_Tensor is
      Value : Vector_Type;
   begin
      return Result : CPU_Tensor := Without_Data (Shape) do
         for I in Result.Data'Range loop
            Next (Random_State, Value);
            Result.Data (I) := Value;
         end loop;
      end return;
   end Random_Uniform;

end Orka.Numerics.Tensors.SIMD_CPU;
