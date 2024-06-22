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

package Orka.Numerics is
   pragma Pure;

   Singular_Matrix              : exception;
   Not_Positive_Definite_Matrix : exception;

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

   function Range_Length (Index : Range_Type) return Positive is (Index.Stop - Index.Start + 1);

   type Tensor_Range is array (Tensor_Axis range <>) of Range_Type;

   function Shape (Index : Tensor_Range) return Tensor_Shape
     with Post => Index'Length = Shape'Result'Length
                    and (for all D in Index'Range =>
                           Index (D).Stop - Index (D).Start + 1 = Shape'Result (D));

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
     (Axes    : Tensor_Axis;
      Shape   : Tensor_Shape;
      Justify : Alignment) return Tensor_Shape;
   --  Return a shape padded at the beginning or end with 1's for missing Axes
   --
   --  For example, if Axes = 3 and Alignment = Right and Shape has 2 Axes,
   --  then the first Axis of the result is 1 and the last two Axes
   --  equal to the given shape.

   Not_Implemented_Yet : exception;

end Orka.Numerics;
