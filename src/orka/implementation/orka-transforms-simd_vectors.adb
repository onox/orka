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

   package EF is new Ada.Numerics.Generic_Elementary_Functions (Element_Type);

   use SIMD;

   function Magnitude2 (Elements : Vector_Type) return Element_Type is
     (Sum (Elements * Elements));

   function "*" (Factor : Element_Type; Elements : Vector_Type) return Vector_Type is
   begin
      return (Factor, Factor, Factor, Factor) * Elements;
   end "*";

   function "*" (Elements : Vector_Type; Factor : Element_Type) return Vector_Type is
     (Factor * Elements);

   function Magnitude (Elements : Vector_Type) return Element_Type is
   begin
      return EF.Sqrt (Magnitude2 (Elements));
   end Magnitude;

   function Normalize (Elements : Vector_Type) return Vector_Type is
      Length : constant Element_Type := Magnitude (Elements);
   begin
      return Divide_Or_Zero (Elements, (Length, Length, Length, Length));
   end Normalize;

   function Normalized (Elements : Vector_Type) return Boolean is
      function Is_Equivalent (Expected, Result : Element_Type) return Boolean is
         Epsilon : constant Element_Type := 2.0 ** (1 - Element_Type'Model_Mantissa);
      begin
         return Result in Expected - Epsilon .. Expected + Epsilon;
      end Is_Equivalent;
   begin
      return Is_Equivalent (1.0, Magnitude (Elements));
   end Normalized;

   function Distance (Left, Right : Vector_Type) return Element_Type is
     (Magnitude (Left - Right));

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
      return EF.Arccos (Dot (Left, Right) / (Magnitude (Left) * Magnitude (Right)), 360.0);
   end Angle;

   function Dot (Left, Right : Vector_Type) return Element_Type is
     (Sum (Left * Right));

end Orka.Transforms.SIMD_Vectors;
