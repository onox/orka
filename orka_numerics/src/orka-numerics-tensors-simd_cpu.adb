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

package body Orka.Numerics.Tensors.SIMD_CPU is

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
     ((Index (1) - 1) * Columns + Index (2));

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
     ((Dimensions => Object.Dimensions,
       Size       => Object.Size,
       Kind       => Kind,
       Shape      => Object.Shape,
       Data       => <>));

   function Without_Data
     (Shape : Tensor_Shape;
      Kind  : Data_Type := Float_Type) return CPU_Tensor
   is
     ((Dimensions => Shape'Length,
       Size       => Data_Vectors (Shape),
       Kind       => Kind,
       Shape      => Shape,
       Data       => <>));

   ----------------------------------------------------------------------------

   overriding
   function "+" (Left, Right : CPU_Expression) return CPU_Expression is
     (Kind     => Binary_Operation,
      Operator => Add,
      Left     => Expression_Holders.To_Holder (Left),
      Right    => Expression_Holders.To_Holder (Right));

   overriding
   function "-" (Left, Right : CPU_Expression) return CPU_Expression is
     (Kind     => Binary_Operation,
      Operator => Subtract,
      Left     => Expression_Holders.To_Holder (Left),
      Right    => Expression_Holders.To_Holder (Right));

   overriding
   function "*" (Left, Right : CPU_Expression) return CPU_Expression is
     (Kind     => Binary_Operation,
      Operator => Multiply,
      Left     => Expression_Holders.To_Holder (Left),
      Right    => Expression_Holders.To_Holder (Right));

   overriding
   function "/" (Left, Right : CPU_Expression) return CPU_Expression is
     (Kind     => Binary_Operation,
      Operator => Divide,
      Left     => Expression_Holders.To_Holder (Left),
      Right    => Expression_Holders.To_Holder (Right));

   overriding
   function Min (Left, Right : CPU_Expression) return CPU_Expression is
     (Kind     => Binary_Operation,
      Operator => Min,
      Left     => Expression_Holders.To_Holder (Left),
      Right    => Expression_Holders.To_Holder (Right));

   overriding
   function Max (Left, Right : CPU_Expression) return CPU_Expression is
     (Kind     => Binary_Operation,
      Operator => Max,
      Left     => Expression_Holders.To_Holder (Left),
      Right    => Expression_Holders.To_Holder (Right));

   overriding
   function "-" (Value : CPU_Expression) return CPU_Expression is
     (Kind           => Unary_Operation,
      Unary_Operator => Minus,
      Expression     => Expression_Holders.To_Holder (Value));

   overriding
   function "abs" (Value : CPU_Expression) return CPU_Expression is
     (Kind           => Unary_Operation,
      Unary_Operator => Absolute,
      Expression     => Expression_Holders.To_Holder (Value));

   overriding
   function Sqrt (Value : CPU_Expression) return CPU_Expression is
     (Kind           => Unary_Operation,
      Unary_Operator => Sqrt,
      Expression     => Expression_Holders.To_Holder (Value));

   overriding function X return CPU_Expression is (Kind => Argument, Argument => X);
   overriding function Y return CPU_Expression is (Kind => Argument, Argument => Y);

   function Number (Value : Element) return CPU_Expression is (Kind => Number, Number => Value);

   overriding function "+" (Left : Element; Right : CPU_Expression) return CPU_Expression is
     (Number (Left) + Right);
   overriding function "+" (Left : CPU_Expression; Right : Element) return CPU_Expression is
     (Left + Number (Right));

   overriding function "-" (Left : Element; Right : CPU_Expression) return CPU_Expression is
     (Number (Left) - Right);
   overriding function "-" (Left : CPU_Expression; Right : Element) return CPU_Expression is
     (Left - Number (Right));

   overriding function "*" (Left : Element; Right : CPU_Expression) return CPU_Expression is
     (Number (Left) * Right);
   overriding function "*" (Left : CPU_Expression; Right : Element) return CPU_Expression is
     (Left * Number (Right));

   overriding function "/" (Left : Element; Right : CPU_Expression) return CPU_Expression is
     (Number (Left) / Right);
   overriding function "/" (Left : CPU_Expression; Right : Element) return CPU_Expression is
     (Left / Number (Right));

   overriding function Min (Left : Element; Right : CPU_Expression) return CPU_Expression is
     (Min (Number (Left), Right));
   overriding function Min (Left : CPU_Expression; Right : Element) return CPU_Expression is
     (Min (Left, Number (Right)));

   overriding function Max (Left : Element; Right : CPU_Expression) return CPU_Expression is
     (Max (Number (Left), Right));
   overriding function Max (Left : CPU_Expression; Right : Element) return CPU_Expression is
     (Max (Left, Number (Right)));

   function Apply
     (Object      : CPU_Expression;
      Left, Right : Vector_Type) return Vector_Type is
   begin
      case Object.Kind is
         when Argument =>
            case Object.Argument is
               when X =>
                  return Left;
               when Y =>
                  return Right;
            end case;
         when Number =>
            return (others => Object.Number);
         when Binary_Operation =>
            declare
               Result_Left : constant Vector_Type :=
                 CPU_Expression (Object.Left.Element).Apply (Left, Right);
               Result_Right : constant Vector_Type :=
                 CPU_Expression (Object.Right.Element).Apply (Left, Right);
            begin
               case Object.Operator is
                  when Add =>
                     return Result_Left + Result_Right;
                  when Subtract =>
                     return Result_Left - Result_Right;
                  when Multiply =>
                     return Result_Left * Result_Right;
                  when Divide =>
                     return Result_Left / Result_Right;
                  when Min =>
                     return Min (Result_Left, Result_Right);
                  when Max =>
                     return Max (Result_Left, Result_Right);
               end case;
            end;
         when Unary_Operation =>
            declare
               Result : constant Vector_Type :=
                 CPU_Expression (Object.Expression.Element).Apply (Left, Right);
            begin
               case Object.Unary_Operator is
                  when Minus =>
                     return -Result;
                  when Absolute =>
                     return abs Result;
                  when Sqrt =>
                     return Sqrt (Result);
               end case;
            end;
      end case;
   end Apply;

   function Apply
     (Object      : CPU_Expression;
      Left, Right : Element) return Element is
   begin
      case Object.Kind is
         when Argument =>
            case Object.Argument is
               when X =>
                  return Left;
               when Y =>
                  return Right;
            end case;
         when Number =>
            return Object.Number;
         when Binary_Operation =>
            declare
               Result_Left : constant Element :=
                 CPU_Expression (Object.Left.Element).Apply (Left, Right);
               Result_Right : constant Element :=
                 CPU_Expression (Object.Right.Element).Apply (Left, Right);
            begin
               case Object.Operator is
                  when Add =>
                     return Result_Left + Result_Right;
                  when Subtract =>
                     return Result_Left - Result_Right;
                  when Multiply =>
                     return Result_Left * Result_Right;
                  when Divide =>
                     return Result_Left / Result_Right;
                  when Min =>
                     return Element'Min (Result_Left, Result_Right);
                  when Max =>
                     return Element'Max (Result_Left, Result_Right);
               end case;
            end;
         when Unary_Operation =>
            declare
               Result : constant Element :=
                 CPU_Expression (Object.Expression.Element).Apply (Left, Right);
            begin
               case Object.Unary_Operator is
                  when Minus =>
                     return -Result;
                  when Absolute =>
                     return abs Result;
                  when Sqrt =>
                     return EF.Sqrt (Result);
               end case;
            end;
      end case;
   end Apply;

   ----------------------------------------------------------------------------

   overriding function Kind (Object : CPU_Tensor) return Data_Type is (Object.Kind);

   overriding function Get (Object : CPU_Tensor; Index : Index_Type) return Element is
      Last_Index : constant Natural := Object.Shape (1);
   begin
      if Index > Last_Index then
         raise Constraint_Error with
           "Stop index (" & Trim (Index) & ") out of bounds (1 .. " & Trim (Last_Index) & ")";
      end if;

      return Object.Data (Data_Vectors (Index)) (Data_Offset (Index));
   end Get;

   overriding function Get (Object : CPU_Tensor; Index : Index_Type) return Boolean is
      One_Vector : constant Integer_Vector_Type := (others => 1);

      Mask       : constant Integer_Vector_Type := Convert (Object.Data (Data_Vectors (Index)));
      Ones_Zeros : constant Integer_Vector_Type := Mask and One_Vector;
   begin
      return Ones_Zeros (Data_Offset (Index)) = 1;
   end Get;

   overriding function Get (Object : CPU_Tensor; Index : Index_Type) return CPU_Tensor is
      Last_Index : constant Natural := Object.Shape (1);

      Count : constant Positive := Object.Shape (2);
      Shape : constant Tensor_Shape := (1 => Count);
   begin
      if Index > Last_Index then
         raise Constraint_Error with
           "Stop index (" & Trim (Index) & ") out of bounds (1 .. " & Trim (Last_Index) & ")";
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

   procedure Set (Object : in out CPU_Tensor; Index : Index_Type; Row : CPU_Tensor)
     with Pre'Class => Object.Shape (2) = Row.Elements;

   procedure Set (Object : in out CPU_Tensor; Index : Index_Type; Row : CPU_Tensor) is
      Count : constant Natural := Row.Elements;

      Row_Data : Element_Array (1 .. Count)
        with Import, Convention => Ada, Address => Row.Data'Address;

      Object_Data : Element_Array (1 .. Object.Elements)
        with Import, Convention => Ada, Address => Object.Data'Address;

      Base_Index : constant Natural := (Index - 1) * Count;
   begin
      Object_Data (Base_Index + 1 .. Base_Index + Row_Data'Length) := Row_Data;
   end Set;

   function Flattened_Index (Object : CPU_Tensor; Index : Tensor_Index) return Index_Type is
      Rows    : constant Natural := Object.Shape (1);
      Columns : constant Natural := Object.Shape (2);
   begin
      if Index (1) > Rows then
         raise Constraint_Error with
           "Stop index (" & Trim (Index (1)) & ") out of bounds (1 .. " & Trim (Rows) & ")";
      end if;

      if Index (2) > Columns then
         raise Constraint_Error with
           "Stop index (" & Trim (Index (2)) & ") out of bounds (1 .. " & Trim (Columns) & ")";
      end if;

      return To_Index (Index, Columns);
   end Flattened_Index;

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

   overriding function Get (Object : CPU_Tensor; Index : Range_Type) return CPU_Tensor is
   begin
      case Object.Dimensions is
         when 1 =>
            return Object.Get (Tensor_Range'(Index, (1, 1)));
         when 2 =>
            return Object.Get (Tensor_Range'(Index, (1, Object.Shape (2))));
      end case;
   end Get;

   overriding function Get (Object : CPU_Tensor; Index : Tensor_Range) return CPU_Tensor is
      Rows    : constant Natural := Object.Shape (1);
      Columns : constant Natural := (if 2 in Object.Shape'Range then Object.Shape (2) else 1);

      Result_Rows    : constant Positive := Index (1).Stop - Index (1).Start + 1;
      Result_Columns : constant Positive := Index (2).Stop - Index (2).Start + 1;
   begin
      case Object.Dimensions is
         when 1 =>
            declare
               Count : constant Positive := Result_Rows;
               Size  : constant Positive := Data_Vectors (Count);
               Shape : constant Tensor_Shape := (1 => Count);

               Row_Start : Index_Type renames Index (1).Start;
               Row_Stop  : Index_Type renames Index (1).Stop;
            begin
               if Index (1).Stop > Rows then
                  raise Constraint_Error with
                    "Stop index (" & Trim (Index (1).Stop) & ") out of bounds (1 .. " &
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
               Shape : constant Tensor_Shape := (1 => Result_Rows, 2 => Result_Columns);
            begin
               if Index (1).Stop > Rows then
                  raise Constraint_Error with
                    "Stop index (" & Trim (Index (1).Stop) & ") out of bounds (1 .. " &
                    Trim (Rows) & ")";
               end if;

               if Index (2).Stop > Columns then
                  raise Constraint_Error with
                    "Stop index (" & Trim (Index (2).Stop) & ") out of bounds (1 .. " &
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

                           Current_Row : constant Natural := Index (1).Start - 1 + I;
                           Base_Index  : constant Natural := (Current_Row - 1) * Columns;

                           Column_Start : Index_Type renames Index (2).Start;
                           Column_Stop  : Index_Type renames Index (2).Stop;
                        begin
                           Result_Data (Result_Index + 1 .. Result_Index + Result_Columns) :=
                             Object_Data (Base_Index + Column_Start .. Base_Index + Column_Stop);
                        end;
                     end loop;
                  end;
               end return;
            end;
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
      SU.Append (Result, "array([");
      case Object.Dimensions is
         when 1 =>
            for I in 1 .. Count loop
               declare
                  First_Element_Of_Row : constant Boolean := (I - 1) mod Row_Count = 0;
                  Last_Element_Of_Row  : constant Boolean := (I - 0) mod Row_Count = 0;

                  Value : Element_Type renames Object.Data (Data_Vectors (I)) (Data_Offset (I));
               begin
                  if First_Element_Of_Row then
                     SU.Append (Result, (if I = 1 then "" else "       "));
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
               Rows    : constant Natural := Object.Shape (1);
               Columns : constant Natural := Object.Shape (2);

               Object_Data : Element_Array (1 .. Count)
                 with Import, Convention => Ada, Address => Object.Data'Address;
            begin
               for I in 1 .. Rows loop
                  SU.Append (Result, (if I = 1 then "" else "       "));
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
      end case;
      SU.Append (Result, "])");

      return SU.To_String (Result);
   end Image;

   overriding
   function Shape (Object : CPU_Tensor) return Tensor_Shape is (Object.Shape);

   overriding
   function Elements (Object : CPU_Tensor) return Natural is (Elements (Object.Shape));

   overriding
   function Dimensions (Object : CPU_Tensor) return Tensor_Dimension is (Object.Dimensions);

   overriding
   function Fill (Shape : Tensor_Shape; Value : Element) return CPU_Tensor is
      Vector : constant Vector_Type := (others => Value);
   begin
      return Result : CPU_Tensor := Without_Data (Shape) do
         Result.Data := (others => Vector);
      end return;
   end Fill;

   overriding
   function Zeros (Shape : Tensor_Shape) return CPU_Tensor is (Fill (Shape, 0.0));

   overriding
   function Zeros (Elements : Positive) return CPU_Tensor is (Zeros ((1 => Elements)));

   overriding
   function Ones (Shape : Tensor_Shape) return CPU_Tensor is (Fill (Shape, 1.0));

   overriding
   function Ones (Elements : Positive) return CPU_Tensor is (Ones ((1 => Elements)));

   overriding
   function To_Tensor (Elements : Element_Array) return CPU_Tensor is
      Shape : constant Tensor_Shape := (1 => Elements'Length);
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
   function To_Boolean_Tensor (Booleans : Boolean_Array) return CPU_Tensor is
      Elements : Element_Array (Booleans'Range);
   begin
      for Index in Elements'Range loop
         Elements (Index) := (if Booleans (Index) then 1.0 else 0.0);
      end loop;

      return To_Tensor (Elements) > 0.0;
   end To_Boolean_Tensor;

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
      Base        : Element := 10.0) return CPU_Tensor
   is (Log_Space (Start    => EF.Log (Start) / EF.Log (Base),
                  Stop     => EF.Log (Stop) / EF.Log (Base),
                  Count    => Count,
                  Interval => Interval,
                  Base     => Base));

   overriding
   function Array_Range (Start, Stop : Element; Step : Element := 1.0) return CPU_Tensor is
     (Linear_Space (Start => Start,
                    Stop  => Stop,
                    Count => Positive'Max (1, Integer (Element'Ceiling ((Stop - Start) / Step))),
                    Interval => Half_Open));

   overriding
   function Array_Range (Stop : Element) return CPU_Tensor is
     (Array_Range (Start => 0.0, Stop => Stop));

   overriding
   function Identity (Size : Positive; Offset : Integer := 0) return CPU_Tensor is
     (Identity (Rows => Size, Columns => Size, Offset => Offset));

   overriding
   function Identity (Rows, Columns : Positive; Offset : Integer := 0) return CPU_Tensor is
      Shape : constant Tensor_Shape := (1 => Rows, 2 => Columns);

      Max_Size : constant Positive := Positive'Max (Rows, Columns);

      Zero_Vector : constant Vector_Type := (others => 0.0);
   begin
      return Result : CPU_Tensor :=
        (Dimensions => Shape'Length,
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
   function Main_Diagonal (Object : CPU_Tensor; Offset : Integer := 0) return CPU_Tensor is
      Rows    : constant Positive := Object.Shape (1);
      Columns : constant Positive := Object.Shape (2);

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
        (Dimensions => Shape'Length,
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
   function Trace (Object : CPU_Tensor; Offset : Integer := 0) return Element is
     (Object.Main_Diagonal (Offset => Offset).Sum);

   overriding
   function Reshape (Object : CPU_Tensor; Shape : Tensor_Shape) return CPU_Tensor is
     (Dimensions => Shape'Length,
      Size       => Object.Size,
      Kind       => Object.Kind,
      Shape      => Shape,
      Data       => Object.Data);

   overriding
   function Reshape (Object : CPU_Tensor; Elements : Positive) return CPU_Tensor is
      Shape : constant Tensor_Shape := (1 => Elements);
   begin
      return
        (Dimensions => Shape'Length,
         Size       => Object.Size,
         Kind       => Object.Kind,
         Shape      => Shape,
         Data       => Object.Data);
   end Reshape;

   overriding
   function Flatten (Object : CPU_Tensor) return CPU_Tensor is
     ((Dimensions => 1,
       Size       => Object.Size,
       Kind       => Object.Kind,
       Shape      => (1 => Object.Elements),
       Data       => Object.Data));

   overriding
   function Concatenate
     (Left, Right : CPU_Tensor;
      Dimension   : Tensor_Dimension) return CPU_Tensor
   is
      Shape : constant Tensor_Shape := Add (Left.Shape, Right.Shape, Dimension);
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
            case Dimension is
               when 1 =>
                  Result_Data (1 .. Left.Elements) := Left_Data;
                  Result_Data (Left.Elements + 1 .. Result_Data'Last) := Right_Data;
               when 2 =>
                  declare
                     Rows : constant Positive := Left.Shape (1);

                     Columns_Left  : constant Positive := Left.Shape (2);
                     Columns_Right : constant Positive := Right.Shape (2);
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
            end case;
         end;
      end return;
   end Concatenate;

   overriding
   function "&" (Left, Right : CPU_Tensor) return CPU_Tensor is (Concatenate (Left, Right, 1));

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
      Left_Rows     : constant Natural := (if Left.Dimensions = 2 then Left.Shape (1) else 1);
      Count         : constant Natural := Right.Shape (1);
      Right_Columns : constant Natural := (if Right.Dimensions = 2 then Right.Shape (2) else 1);

      Shape : constant Tensor_Shape := (1 => Left_Rows, 2 => Right_Columns);
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

   overriding function "**" (Left : CPU_Tensor; Right : Integer) return CPU_Tensor is
      function Matrix_Power (Left : CPU_Tensor; Right : Natural) return CPU_Tensor is
         Result : CPU_Tensor := Identity (Size => Left.Shape (1));

         Log_2 : constant Element := EF.Log (2.0);

         Remaining : Natural := Right;
      begin
         while Remaining > 0 loop
            declare
               Doubling : CPU_Tensor := Left;
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
            return Identity (Size => Left.Shape (1));
         when -1 =>
            return Left.Inverse;
         when Integer'First .. -2 =>
            return Matrix_Power (Left.Inverse, abs Right);
      end case;
   end "**";

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
   function Inverse (Object : CPU_Tensor) return CPU_Tensor is
      Size : constant Natural := Object.Shape (1);

      Solution : Solution_Kind := None;
   begin
      return Result : constant CPU_Tensor := Solve (Object, Identity (Size), Solution) do
         if Solution /= Unique then
            raise Singular_Matrix;
         end if;
      end return;
   end Inverse;

   overriding
   function Transpose (Object : CPU_Tensor) return CPU_Tensor is
   begin
      raise Program_Error;
      return Zeros ((1 => 1));  --  FIXME
   end Transpose;

   function Matrix_Solve (A, B : CPU_Tensor; Solution : out Solution_Kind) return CPU_Tensor is
      Ab : CPU_Tensor := Concatenate (A, B, Dimension => 2);

      Rows    : constant Natural := A.Shape (1);
      Columns : constant Natural := A.Shape (2);

      Columns_Ab : constant Natural := Ab.Shape (2);

      function Find_Largest_Pivot (Row_Index, Column_Index : Index_Type) return Index_Type is
         Pivot_Value : Element    := abs Ab ((Row_Index, Column_Index));
         Pivot_Index : Index_Type := Row_Index;
      begin
         for Index in Row_Index .. Rows loop
            declare
               Value : constant Element := abs Ab ((Index, Column_Index));
            begin
               if Value > Pivot_Value then
                  Pivot_Value := Value;
                  Pivot_Index := Index;
               end if;
            end;
         end loop;

         return Pivot_Index;
      end Find_Largest_Pivot;

      procedure Swap_Rows (I, J : Index_Type) is
      begin
         if I /= J then
            declare
               Row_I : CPU_Tensor renames Ab (I);
               Old_J : constant CPU_Tensor := Ab (J);
            begin
               Set (Ab, J, Row_I);
               Set (Ab, I, Old_J);
            end;
         end if;
      end Swap_Rows;

      procedure Scale_Row (I : Index_Type; Scale : Element) is
         Row_I : CPU_Tensor renames Ab (I);
      begin
         if Scale /= 1.0 then
            Set (Ab, I, Scale * Row_I);
         end if;
      end Scale_Row;

      procedure Replace_Row (Scale : Element; I, J : Index_Type) is
         Row_I : CPU_Tensor renames Ab (I);
         Row_J : CPU_Tensor renames Ab (J);
      begin
         if Scale /= 0.0 then
            Set (Ab, J, Row_J - Scale * Row_I);
         end if;
      end Replace_Row;

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
            while Pivot_Index < Columns and then Ab ((Row_Index, Pivot_Index)) = 0.0 loop
               Pivot_Index := Pivot_Index + 1;
               Row_Index   := Find_Largest_Pivot (Index, Pivot_Index);
            end loop;

            if Ab ((Row_Index, Pivot_Index)) = 0.0 then
               Pivot_Index := 0;
               exit;
            end if;

            Swap_Rows (Index, Row_Index);

            --  Create zeros below the pivot position
            declare
               Pivot_Value : constant Element := Ab ((Index, Pivot_Index));
            begin
               for Row_Index in Index + 1 .. Rows loop
                  Replace_Row (Ab ((Row_Index, Pivot_Index)) / Pivot_Value, Index, Row_Index);
               end loop;
            end;

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
               Scale_Row (Index, 1.0 / Ab ((Index, Pivot_Index)));

               --  Create zeros above the pivot position
               for Row_Index in 1 .. Index - 1 loop
                  Replace_Row (Ab ((Row_Index, Pivot_Index)), Index, Row_Index);
               end loop;
            else
               --  Row contains only zeros; no pivot
               null;
            end if;
         end;
      end loop;

      if (for some I in 1 .. Rows => Pivots (I) = 0 and Any_True (CPU_Tensor'(B (I) /= 0.0))) then
         Solution := None;
      elsif Columns > Rows or else (for some I in 1 .. Columns => Pivots (I) /= I) then
         Solution := Infinite;
      else
         Solution := Unique;
      end if;

      return Ab (Tensor_Range'((1, Rows), (Columns + 1, Columns_Ab)));
   end Matrix_Solve;

   overriding
   function Solve (A, B : CPU_Tensor; Solution : out Solution_Kind) return CPU_Tensor is
   begin
      case B.Dimensions is
         when 1 =>
            return Matrix_Solve (A, B.Reshape ((B.Elements, 1)), Solution).Flatten;
         when 2 =>
            return Matrix_Solve (A, B, Solution);
      end case;
   end Solve;

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

   overriding function "**" (Left, Right : CPU_Tensor) return CPU_Tensor is
--     (Exp (Multiply (Right, Log (Left))));
      use EF;

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
                  if Left_Element = 0.0 and Right_Element = 0.0 then
                     --  EF."**" raises Argument_Error instead of returning 1.0
                     Result.Data (Index) (Offset) := 1.0;
                  else
                     Result.Data (Index) (Offset) := Left_Element ** Right_Element;
                  end if;
               end;
            end loop;
         end loop;

         for Offset in Vector_Type'Range loop
            Result.Data (Result.Data'Last) (Offset) :=
              Last_Left (Offset) ** Last_Right (Offset);
         end loop;
      end return;
   end "**";

   overriding function "**" (Left : CPU_Tensor; Right : Element) return CPU_Tensor is
--     (Exp (Right * Log (Left)));
      use EF;

      One_Vector : constant Vector_Type := (others => 1.0);
   begin
      if Right = 0.0 then
         return Result : CPU_Tensor := Without_Data (Left) do
            Result.Data := (others => One_Vector);
         end return;
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
      use EF;

      One_Vector : constant Vector_Type := (others => 1.0);
   begin
      if Left = 1.0 then
         return Result : CPU_Tensor := Without_Data (Right) do
            Result.Data := (others => One_Vector);
         end return;
      else
         return Result : CPU_Tensor := Without_Data (Right) do
            for Index in Result.Data'Range loop
               for Offset in Vector_Type'Range loop
                  declare
                     Right_Element : Element renames Right.Data (Index) (Offset);
                  begin
                     if Left = 0.0 and Right_Element = 0.0 then
                        Result.Data (Index) (Offset) := 1.0;
                     else
                        Result.Data (Index) (Offset) := Left ** Right.Data (Index) (Offset);
                     end if;
                  end;
               end loop;
            end loop;
         end return;
      end if;
   end "**";

   overriding function "*" (Left : Element; Right : CPU_Tensor) return CPU_Tensor is
     (Right * Left);

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

   overriding function "+" (Left : Element; Right : CPU_Tensor) return CPU_Tensor is
     (Right + Left);

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

   overriding function "-" (Left : CPU_Tensor; Right : Element) return CPU_Tensor is
     (Left + (-Right));

   overriding function "-" (Object : CPU_Tensor) return CPU_Tensor is
   begin
      return Result : CPU_Tensor := Without_Data (Object) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := -Object.Data (Index);
         end loop;
      end return;
   end "-";

   overriding function "mod" (Left, Right : CPU_Tensor) return CPU_Tensor is
     (((Left rem Right) + Right) rem Right);

   overriding function "rem" (Left, Right : CPU_Tensor) return CPU_Tensor is
     (Left - Multiply (Truncate (Left / Right), Right));

   overriding function "mod" (Left : CPU_Tensor; Right : Element) return CPU_Tensor is
     (((Left rem Right) + Right) rem Right);

   overriding function "rem" (Left : CPU_Tensor; Right : Element) return CPU_Tensor is
     (Left - Truncate (Left / Right) * Right);

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

   overriding function Power (Left : CPU_Tensor; Right : Integer) return CPU_Tensor is
      function Vector_Power (Left : CPU_Tensor; Right : Natural) return CPU_Tensor is
         Result : CPU_Tensor := Ones (Left.Shape);

         Log_2 : constant Element := EF.Log (2.0);

         Remaining : Natural := Right;
      begin
         while Remaining > 0 loop
            declare
               Doubling : CPU_Tensor := Left;
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
      if Right > 0 then
         return Vector_Power (Left, Right);
      elsif Right < 0 then
         return 1.0 / Vector_Power (Left, abs Right);
      else
         return Ones (Elements => Left.Shape (1));
      end if;
   end Power;

   overriding function Min (Left : Element; Right : CPU_Tensor) return CPU_Tensor is
     (Min (Right, Left));

   overriding function Min (Left : CPU_Tensor; Right : Element) return CPU_Tensor is
      Right_Vector : constant Vector_Type := (others => Right);
   begin
      return Result : CPU_Tensor := Without_Data (Left) do
         for Index in Result.Data'Range loop
            Result.Data (Index) := Min (Left.Data (Index), Right_Vector);
         end loop;
      end return;
   end Min;

   overriding function Max (Left : Element; Right : CPU_Tensor) return CPU_Tensor is
     (Max (Right, Left));

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

   overriding function Log10 (Object : CPU_Tensor) return CPU_Tensor is
     (Log (Object) / EF.Log (10.0));

   overriding function Log2 (Object : CPU_Tensor) return CPU_Tensor is
     (Log (Object) / EF.Log (2.0));

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

   overriding function Degrees (Object : CPU_Tensor) return CPU_Tensor is
     (Object / Ada.Numerics.Pi * 180.0);

   overriding function Radians (Object : CPU_Tensor) return CPU_Tensor is
     (Object / 180.0 * Ada.Numerics.Pi);

   ----------------------------------------------------------------------------

   overriding
   function Reduce
     (Object    : CPU_Tensor;
      Subject   : Expression'Class;
      Initial   : Element) return Element
   is
      Result : Element_Type := Initial;

      CPU_Subject : constant CPU_Expression := CPU_Expression (Subject);

      Padding : constant Natural :=
        Data_Padding (Size => Object.Size, Count => Object.Elements);

      type Result_Index_Type is mod 8;

      Sums : array (Result_Index_Type) of Vector_Type := (others => (others => Initial));
      --  TODO Do pairwise summation recursively

      Accumulated_Result : Vector_Type := (others => Initial);

      Last_Index : constant Natural :=
        Natural'Min (Object.Data'First + Sums'Length - 1, Object.Data'Last - 1);
   begin
      for Index in Object.Data'First .. Last_Index loop
         Sums (Result_Index_Type (Index mod Sums'Length)) := Object.Data (Index);
      end loop;

      for Index in Last_Index + 1 .. Object.Data'Last - 1 loop
         declare
            Result_Index : constant Result_Index_Type := Result_Index_Type (Index mod Sums'Length);
         begin
            Sums (Result_Index) := CPU_Subject.Apply (Sums (Result_Index), Object.Data (Index));
         end;
      end loop;

      Accumulated_Result := Sums (Result_Index_Type (Object.Data'First mod Sums'Length));

      for Index in Object.Data'First + 1 .. Last_Index loop
         declare
            Result_Index : constant Result_Index_Type := Result_Index_Type (Index mod Sums'Length);
         begin
            Accumulated_Result := CPU_Subject.Apply (Accumulated_Result, Sums (Result_Index));
         end;
      end loop;

      if Object.Elements > Vector_Type'Length then
         for Element of Accumulated_Result loop
            Result := CPU_Subject.Apply (Result, Element);
         end loop;
      else
         --  Ignore Accumulated_Result because Object.Data has only 1 vector,
         --  which is added to Result below
         null;
      end if;

      for Index in Vector_Index_Type'First .. From_Last (Padding) loop
         Result := CPU_Subject.Apply (Result, Object.Data (Object.Data'Last) (Index));
      end loop;

      return Result;
   end Reduce;

   overriding
   function Reduce
     (Object    : CPU_Tensor;
      Subject   : Expression'Class;
      Initial   : Element;
      Dimension : Tensor_Dimension) return CPU_Tensor is
   begin
      raise Program_Error;
      return Zeros ((1 => 1));  --  FIXME
   end Reduce;

   overriding function Sum (Object : CPU_Tensor) return Element is
      Expression_Sum : constant CPU_Expression := X + Y;
   begin
      return Object.Reduce (Expression_Sum, 0.0);
   end Sum;

   overriding function Product (Object : CPU_Tensor) return Element is
      Expression_Product : constant CPU_Expression := X * Y;
   begin
      return Object.Reduce (Expression_Product, 1.0);
   end Product;

   ----------------------------------------------------------------------------
   --                               Statistics                               --
   ----------------------------------------------------------------------------

   overriding function Min (Object : CPU_Tensor) return Element is
      Expression_Min : constant CPU_Expression := Min (X, Y);
   begin
      return Object.Reduce (Expression_Min, Element'Last);
   end Min;

   overriding function Max (Object : CPU_Tensor) return Element is
      Expression_Max : constant CPU_Expression := Max (X, Y);
   begin
      return Object.Reduce (Expression_Max, Element'First);
   end Max;

   --  See https://en.wikipedia.org/wiki/Median#Medians_for_samples
   overriding function Quantile (Object : CPU_Tensor; P : Probability) return Element is
   begin
      raise Program_Error;
      return 0.0;  --  FIXME
   end Quantile;

   overriding function Median (Object : CPU_Tensor) return Element is
     (Object.Quantile (0.5));

   overriding function Mean (Object : CPU_Tensor) return Element is
     (Object.Sum / Element (Object.Elements));

   overriding
   function Variance (Object : CPU_Tensor; Offset : Natural := 0) return Element is
     (Sum (Power (Object - Object.Mean, 2)) / (Element (Object.Elements - Offset)));

   overriding
   function Standard_Deviation (Object : CPU_Tensor; Offset : Natural := 0) return Element is
     (EF.Sqrt (Object.Variance (Offset)));

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
   function Min (Object : CPU_Tensor; Dimension : Tensor_Dimension) return CPU_Tensor is
   begin
      raise Program_Error;
      return Zeros ((1 => 1));  --  FIXME
   end Min;

   overriding
   function Max (Object : CPU_Tensor; Dimension : Tensor_Dimension) return CPU_Tensor is
   begin
      raise Program_Error;
      return Zeros ((1 => 1));  --  FIXME
   end Max;

   overriding
   function Quantile
     (Object    : CPU_Tensor;
      P         : Probability;
      Dimension : Tensor_Dimension) return CPU_Tensor is
   begin
      raise Program_Error;
      return Zeros ((1 => 1));  --  FIXME
   end Quantile;

   overriding
   function Median (Object : CPU_Tensor; Dimension : Tensor_Dimension) return CPU_Tensor is
     (Object.Quantile (0.5, Dimension));

   overriding
   function Mean (Object : CPU_Tensor; Dimension : Tensor_Dimension) return CPU_Tensor is
   begin
      raise Program_Error;
      return Zeros ((1 => 1));  --  FIXME
   end Mean;

   overriding
   function Variance
     (Object    : CPU_Tensor;
      Dimension : Tensor_Dimension;
      Offset    : Natural := 0) return CPU_Tensor is
   begin
      raise Program_Error;
      return Zeros ((1 => 1));  --  FIXME
   end Variance;

   overriding
   function Standard_Deviation
     (Object    : CPU_Tensor;
      Dimension : Tensor_Dimension;
      Offset    : Natural := 0) return CPU_Tensor
   is (Sqrt (Object.Variance (Dimension, Offset)));

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

   overriding function "="  (Left : CPU_Tensor; Right : Element) return CPU_Tensor is
   begin
      case Left.Kind is
         when Float_Type =>
            return (abs (Left - Right) <= Element_Type'Model_Epsilon);
         when Int_Type | Bool_Type =>
            declare
               Right_Vector : constant Vector_Type := (others => Right);
            begin
               return Result : CPU_Tensor := Without_Data (Left, Kind => Bool_Type) do
                  for Index in Result.Data'Range loop
                     Result.Data (Index) := Left.Data (Index) = Right_Vector;
                  end loop;
                  Result.Data (Result.Data'Last) := Disable_Padding (Result);
               end return;
            end;
      end case;
   end "=";

   overriding function "/=" (Left : CPU_Tensor; Right : Element) return CPU_Tensor is
   begin
      case Left.Kind is
         when Float_Type =>
            return (abs (Left - Right) > Element_Type'Model_Epsilon);
         when Int_Type | Bool_Type =>
            declare
               Right_Vector : constant Vector_Type := (others => Right);
            begin
               return Result : CPU_Tensor := Without_Data (Left, Kind => Bool_Type) do
                  for Index in Result.Data'Range loop
                     Result.Data (Index) := Left.Data (Index) /= Right_Vector;
                  end loop;
                  Result.Data (Result.Data'Last) := Disable_Padding (Result);
               end return;
            end;
      end case;
   end "/=";

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

   overriding function "="  (Left : Element; Right : CPU_Tensor) return CPU_Tensor is
     (Right = Left);

   overriding function "/=" (Left : Element; Right : CPU_Tensor) return CPU_Tensor is
     (Right /= Left);

   overriding function ">"  (Left : Element; Right : CPU_Tensor) return CPU_Tensor is
     (Right < Left);

   overriding function "<"  (Left : Element; Right : CPU_Tensor) return CPU_Tensor is
     (Right > Left);

   overriding function ">=" (Left : Element; Right : CPU_Tensor) return CPU_Tensor is
     (Right <= Left);

   overriding function "<=" (Left : Element; Right : CPU_Tensor) return CPU_Tensor is
     (Right >= Left);

   ----------------------------------------------------------------------------

   overriding function "=" (Left, Right : CPU_Tensor) return Boolean is
     (All_True (Left = Right));

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
   is (All_True (abs (Left - Right) <= Absolute_Tolerance + Relative_Tolerance * abs Right));

   overriding
   function Any_True (Object : CPU_Tensor; Dimension : Tensor_Dimension) return CPU_Tensor is
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
   function All_True (Object : CPU_Tensor; Dimension : Tensor_Dimension) return CPU_Tensor is
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
