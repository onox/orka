--  SPDX-License-Identifier: Apache-2.0
--
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

with Ada.Numerics;

generic
   type Element_Type is digits <>;
   type Vector_Type is array (Index_Homogeneous) of Element_Type;
   with function Multiply_Vectors (Left, Right : Vector_Type) return Vector_Type;
   with function Add_Vectors (Left, Right : Vector_Type) return Vector_Type;
   with function Subtract_Vectors (Left, Right : Vector_Type) return Vector_Type;
   with function Minus_Vector (Elements : Vector_Type) return Vector_Type;
   with function Absolute_Vector (Elements : Vector_Type) return Vector_Type;
   with function Sum (Elements : Vector_Type) return Element_Type;
   with function Divide_Or_Zero (Left, Right : Vector_Type) return Vector_Type;
   with function Cross_Product (Left, Right : Vector_Type) return Vector_Type;
package Orka.Transforms.SIMD_Vectors is
   pragma Pure;

   subtype Vector4 is Vector_Type;

   type Direction is new Vector4
     with Dynamic_Predicate => Direction (W) = 0.0;

   type Point is new Vector4
     with Dynamic_Predicate => Point (W) = 1.0;

   function Zero return Vector4 is
     ((0.0, 0.0, 0.0, 0.0))
   with Inline;

   function Zero_Direction return Direction is
     ((0.0, 0.0, 0.0, 0.0))
   with Inline;

   function Zero_Point return Point is
     ((0.0, 0.0, 0.0, 1.0))
   with Inline;

   function To_Radians (Angle : Element_Type) return Element_Type is
     (Angle / 180.0 * Ada.Numerics.Pi);

   function To_Degrees (Angle : Element_Type) return Element_Type is
     (Angle / Ada.Numerics.Pi * 180.0);

   function "=" (Left, Right : Vector_Type) return Boolean;

   function "+" (Left, Right : Vector_Type) return Vector_Type renames Add_Vectors;

   function "-" (Left, Right : Vector_Type) return Vector_Type renames Subtract_Vectors;

   function "-" (Elements : Vector_Type) return Vector_Type renames Minus_Vector;

   function "abs" (Elements : Vector_Type) return Vector_Type renames Absolute_Vector;

   function "*" (Left, Right : Vector_Type) return Vector_Type renames Multiply_Vectors;

   function "*" (Factor : Element_Type; Elements : Vector_Type) return Vector_Type;

   function "*" (Elements : Vector_Type; Factor : Element_Type) return Vector_Type;

   ----------------------------------------------------------------------------

   function "*" (Factor : Element_Type; Elements : Direction) return Direction;

   function "*" (Elements : Direction; Factor : Element_Type) return Direction;

   function "-" (Elements : Point) return Point;

   function "+" (Left, Right : Direction) return Direction;

   function "+" (Left : Point; Right : Direction) return Point;

   function "+" (Left : Direction; Right : Point) return Point;

   function "-" (Left, Right : Point) return Direction;

   ----------------------------------------------------------------------------

   function Magnitude2 (Elements : Vector_Type) return Element_Type
     with Inline;

   function Magnitude (Elements : Vector_Type) return Element_Type;
   --  Return the magnitude or length of the vector

   function Length (Elements : Vector_Type) return Element_Type renames Magnitude;
   --  Return the magnitude or length of the vector

   function Normalize (Elements : Vector_Type) return Vector_Type;
   --  Return the unit vector of the given vector

   function Normalized (Elements : Vector_Type) return Boolean;
   --  Return True if the vector is normalized, False otherwise

   function Distance (Left, Right : Point) return Element_Type;
   --  Return the distance between two points

   function Projection (Elements, Direction : Vector_Type) return Vector_Type;
   --  Return the projection of a vector in some direction

   function Perpendicular (Elements, Direction : Vector_Type) return Vector_Type;
   --  Return a vector perpendicular to the projection of the vector in
   --  the given direction

   function Angle (Left, Right : Vector_Type) return Element_Type;
   --  Return the angle in radians between two vectors

   function Dot (Left, Right : Vector_Type) return Element_Type;

   function Cross (Left, Right : Vector_Type) return Vector_Type renames Cross_Product;

   function Slerp
     (Left, Right : Vector_Type;
      Weight      : Element_Type) return Vector_Type
   with Pre  => Weight in 0.0 .. 1.0,
        Post => Normalized (Slerp'Result);
   --  Return the interpolated unit vector on the shortest arc
   --  between the Left and Right vectors

end Orka.Transforms.SIMD_Vectors;
