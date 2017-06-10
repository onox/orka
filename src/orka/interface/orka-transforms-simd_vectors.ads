--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

with Orka.SIMD;

generic
   type Element_Type is digits <>;
   type Vector_Type is array (SIMD.Index_Homogeneous) of Element_Type;
   with function "*" (Left, Right : Vector_Type) return Vector_Type;
   with function Add_Vectors (Left, Right : Vector_Type) return Vector_Type;
   with function Subtract_Vectors (Left, Right : Vector_Type) return Vector_Type;
   with function Minus_Vector (Elements : Vector_Type) return Vector_Type;
   with function Absolute_Vector (Elements : Vector_Type) return Vector_Type;
   with function Sum (Elements : Vector_Type) return Element_Type;
   with function Divide_Or_Zero (Left, Right : Vector_Type) return Vector_Type;
   with function Cross_Product (Left, Right : Vector_Type) return Vector_Type;
package Orka.Transforms.SIMD_Vectors is
   pragma Preelaborate;

   subtype Vector4 is Vector_Type;

   function Zero_Value return Vector_Type is
     ((0.0, 0.0, 0.0, 0.0))
   with Inline;

   function "+" (Left, Right : Vector_Type) return Vector_Type renames Add_Vectors;

   function "-" (Left, Right : Vector_Type) return Vector_Type renames Subtract_Vectors;

   function "-" (Elements : Vector_Type) return Vector_Type renames Minus_Vector;

   function "abs" (Elements : Vector_Type) return Vector_Type renames Absolute_Vector;

   function "*" (Factor : Element_Type; Elements : Vector_Type) return Vector_Type;

   function "*" (Elements : Vector_Type; Factor : Element_Type) return Vector_Type;

   function Magnitude2 (Elements : Vector_Type) return Element_Type
     with Inline;

   function Magnitude (Elements : Vector_Type) return Element_Type;

   function Normalize (Elements : Vector_Type) return Vector_Type;

   function Normalized (Elements : Vector_Type) return Boolean;

   function Distance (Left, Right : Vector_Type) return Element_Type;

   function Projection (Elements, Direction : Vector_Type) return Vector_Type;

   function Perpendicular (Elements, Direction : Vector_Type) return Vector_Type;

   function Angle (Left, Right : Vector_Type) return Element_Type;

   function Dot (Left, Right : Vector_Type) return Element_Type;

   function Cross (Left, Right : Vector_Type) return Vector_Type renames Cross_Product;

end Orka.Transforms.SIMD_Vectors;
