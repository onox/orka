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

with Ada.Numerics.Generic_Elementary_Functions;

package body Orka.Transforms.SIMD_Vectors is

   function "-" (Elements : Point) return Point is
      Result : Vector4 := -Vector4 (Elements);
   begin
      Result (W) := Elements (W);
      return Point (Result);
   end "-";

   function "-" (Left, Right : Direction) return Direction is
     (Direction (Vector4 (Left) - Vector4 (Right)));

   function "+" (Left, Right : Direction) return Direction is
     (Direction (Vector4 (Left) + Vector4 (Right)));

   function "+" (Left : Point; Right : Direction) return Point is
     (Point (Vector4 (Left) + Vector4 (Right)));

   function "+" (Left : Direction; Right : Point) return Point is
     (Point (Vector4 (Left) + Vector4 (Right)));

   function "-" (Left, Right : Point) return Direction is
     (Direction (Vector4 (Left) - Vector4 (Right)));

   ----------------------------------------------------------------------------

   package EF is new Ada.Numerics.Generic_Elementary_Functions (Element_Type);

   function Magnitude2 (Elements : Vector_Type) return Element_Type is
     (Sum (Elements * Elements));

   function "*" (Factor : Element_Type; Elements : Vector_Type) return Vector_Type is
   begin
      return [Factor, Factor, Factor, Factor] * Elements;
   end "*";

   function "*" (Elements : Vector_Type; Factor : Element_Type) return Vector_Type is
     (Factor * Elements);

   function "*" (Factor : Element_Type; Elements : Direction) return Direction is
     (Direction (Factor * Vector4 (Elements)));

   function "*" (Elements : Direction; Factor : Element_Type) return Direction is
     (Factor * Elements);

   function Magnitude (Elements : Vector_Type) return Element_Type is
   begin
      return EF.Sqrt (Magnitude2 (Elements));
   end Magnitude;

   function Normalize (Elements : Vector_Type) return Vector_Type is
      Length : constant Element_Type := Magnitude (Elements);
   begin
      return Divide_Or_Zero (Elements, [Length, Length, Length, Length]);
   end Normalize;

   function Normalize_Fast (Elements : Vector_Type) return Vector_Type is
      Length_Squared : constant Element_Type := Magnitude2 (Elements);
   begin
      return Elements * Reciprocal_Sqrt ([others => Length_Squared]);
   end Normalize_Fast;

   function Normalized (Elements : Vector_Type) return Boolean is
      function Is_Equivalent (Expected, Result : Element_Type) return Boolean is
         --  Because the square root is not computed, the bounds need
         --  to be increased to +/- 2 * Epsilon + Epsilon ** 2. Since
         --  Epsilon < 1, we can simply take +/- 3 * Epsilon
         Epsilon : constant Element_Type := 3.0 * Element_Type'Model_Epsilon;
      begin
         return abs (Result - Expected) <= Epsilon;
      end Is_Equivalent;
   begin
      return Is_Equivalent (1.0, Magnitude2 (Elements));
   end Normalized;

   function Distance (Left, Right : Point) return Element_Type is
     (Magnitude (Vector_Type (Left - Right)));

   function Projection (Elements, Direction : Vector_Type) return Vector_Type is
      Unit_Direction : constant Vector_Type := Normalize (Direction);
   begin
      --  The dot product gives the magnitude of the projected vector:
      --  |A_b| = |A| * cos(theta) = A . U_b
      return Dot (Elements, Unit_Direction) * Unit_Direction;
   end Projection;

   function Perpendicular (Elements, Direction : Vector_Type) return Vector_Type is
     (Elements - Projection (Elements, Direction));

   function Angle (Left, Right : Vector_Type) return Element_Type is
   begin
      return EF.Arccos (Dot (Left, Right) / (Magnitude (Left) * Magnitude (Right)));
   end Angle;

   function Dot (Left, Right : Vector_Type) return Element_Type is
     (Sum (Left * Right));

   function Slerp
     (Left, Right : Vector_Type;
      Weight      : Element_Type) return Vector_Type
   is
      Cos_Angle : constant Element_Type := Dot (Left, Right);
      Angle     : constant Element_Type := EF.Arccos (Cos_Angle);

      SA : constant Element_Type := EF.Sin (Angle);

      SL : constant Element_Type := EF.Sin ((1.0 - Weight) * Angle);
      SR : constant Element_Type := EF.Sin (Weight * Angle);
   begin
      return (SL / SA) * Left + (SR / SA) * Right;
   end Slerp;

end Orka.Transforms.SIMD_Vectors;
