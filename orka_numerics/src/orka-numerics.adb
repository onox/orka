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

package body Orka.Numerics is

   function Elements (Shape : Tensor_Shape) return Natural is
      Result : Natural := 1;
   begin
      for Elements of Shape loop
         Result := Result * Elements;
      end loop;

      return Result;
   end Elements;

   function Shape (Index : Tensor_Range) return Tensor_Shape is
   begin
      return Result : Tensor_Shape (Index'Range) do
         for Axis in Result'Range loop
            Result (Axis) := Index (Axis).Stop - Index (Axis).Start + 1;
         end loop;
      end return;
   end Shape;

   function Trim (Value : Natural) return String is
      Result : constant String := Value'Image;
   begin
      return Result (Result'First + 1 .. Result'Last);
   end Trim;

   function Image (Shape : Tensor_Shape) return String is
     (case Shape'Length is
        when 1 =>
           "(" & Trim (Shape (1)) & ")",
        when 2 =>
           "(" & Trim (Shape (1)) & ", " & Trim (Shape (2)) & ")",
        when others =>
           raise Program_Error);

   function Image (Index : Tensor_Index) return String is
     (case Index'Length is
        when 1 =>
           "(" & Trim (Index (1)) & ")",
        when 2 =>
           "(" & Trim (Index (1)) & ", " & Trim (Index (2)) & ")",
        when others =>
           raise Program_Error);

   ----------------------------------------------------------------------------

   function Add (Left, Right : Tensor_Shape; Axis : Tensor_Axis) return Tensor_Shape is
      Result : Tensor_Shape := Left;
   begin
      Result (Axis) := Result (Axis) + Right (Axis);

      return Result;
   end Add;

   function To_Index (Index : Tensor_Index; Shape : Tensor_Shape) return Index_Type is
      Result : Index_Type := Index (Index'Last);
      Size   : Natural    := Shape (Index'Last);
   begin
      for Axis in reverse 1 .. Index'Last - 1 loop
         Result := (Index (Axis) - 1) * Size + Result;
         Size   := Size * Shape (Axis);
      end loop;

      return Result;
   end To_Index;

   function Full_Range (Shape : Tensor_Shape; Index : Tensor_Range) return Tensor_Range is
      Result : Tensor_Range (Shape'Range);
   begin
      for Axis in Result'Range loop
         if Axis in Index'Range then
            Result (Axis) := Index (Axis);
         else
            Result (Axis) := (Start => 1, Stop => Shape (Axis));
         end if;
      end loop;

      return Result;
   end Full_Range;

   function Full_Shape
     (Axes : Tensor_Axis;
      Shape      : Tensor_Shape;
      Justify    : Alignment) return Tensor_Shape
   is
      Result : Tensor_Shape (1 .. Axes) := [others => 1];
      Offset : constant Tensor_Axis'Base :=
        (case Justify is
           when Left  => 0,
           when Right => Result'Last - Shape'Last);
   begin
      for Axis in Shape'Range loop
         Result (Offset + Axis) := Shape (Axis);
      end loop;

      return Result;
   end Full_Shape;

end Orka.Numerics;
