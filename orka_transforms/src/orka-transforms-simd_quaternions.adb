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

package body Orka.Transforms.SIMD_Quaternions is

   package EF is new Ada.Numerics.Generic_Elementary_Functions (Vectors.Element_Type);

   function Vector_Part (Elements : Quaternion) return Vector4 is
   begin
      return Result : Vector4 := Vector4 (Elements) do
         Result (W) := 0.0;
      end return;
   end Vector_Part;

   function "+" (Left, Right : Quaternion) return Quaternion is
      use Vectors;
   begin
      return Quaternion (Vector4 (Left) + Vector4 (Right));
   end "+";

   function "*" (Left, Right : Quaternion) return Quaternion is
      use Vectors;

      Lv : constant Vector4 := Vector_Part (Left);
      Rv : constant Vector4 := Vector_Part (Right);

      Ls : constant Vectors.Element_Type := Left (W);
      Rs : constant Vectors.Element_Type := Right (W);

      V : constant Vector4 := Ls * Rv + Rs * Lv + Vectors.Cross (Lv, Rv);
      S : constant Element_Type := Ls * Rs - Vectors.Dot (Lv, Rv);
   begin
      return Result : Quaternion := Quaternion (V) do
         Result (W) := S;
      end return;
   end "*";

   function "*" (Left : Vectors.Element_Type; Right : Quaternion) return Quaternion is
      use Vectors;
   begin
      return Quaternion (Left * Vector4 (Right));
   end "*";

   function "*" (Left : Quaternion; Right : Vectors.Element_Type) return Quaternion is
     (Right * Left);
   --  Scalar multiplication is commutative

   function Conjugate (Elements : Quaternion) return Quaternion is
      Element_W : constant Vectors.Element_Type := Elements (W);

      use Vectors;
   begin
      return Result : Quaternion := Quaternion (-Vector4 (Elements)) do
         Result (W) := Element_W;
      end return;
   end Conjugate;

   function Inverse (Elements : Quaternion) return Quaternion is
      Length : constant Vectors.Element_Type := Vectors.Magnitude2 (Vector4 (Elements));
   begin
      return Quaternion (Vectors.Divide_Or_Zero
        (Vector4 (Conjugate (Elements)), [Length, Length, Length, Length]));
   end Inverse;

   function Norm (Elements : Quaternion) return Vectors.Element_Type is
     (Vectors.Magnitude (Vector4 (Elements)));

   function Normalize (Elements : Quaternion) return Quaternion is
     (Quaternion (Vectors.Normalize (Vector4 (Elements))));

   function Normalized (Elements : Quaternion) return Boolean is
     (Vectors.Normalized (Vector4 (Elements)));

   function To_Axis_Angle (Elements : Quaternion) return Axis_Angle is
      use type Vectors.Element_Type;

      Angle : constant Vectors.Element_Type := EF.Arccos (Elements (W)) * 2.0;
      SA    : constant Vectors.Element_Type := EF.Sin (Angle / 2.0);
   begin
      if SA /= 0.0 then
         declare
            use Vectors;
            Axis : Vector4 := Vector4 (Elements) * (1.0 / SA);
         begin
            Axis (W) := 0.0;
            return (Axis => Vectors.Direction (Vectors.Normalize (Axis)), Angle => Angle);
         end;
      else
         --  Singularity occurs when angle is 0. Return an arbitrary axis
         return (Axis => [1.0, 0.0, 0.0, 0.0], Angle => 0.0);
      end if;
   end To_Axis_Angle;

   function From_Axis_Angle (Value : Axis_Angle) return Quaternion is
     (R (Axis => Vector4 (Value.Axis), Angle => Value.Angle));

   function R
     (Axis  : Vector4;
      Angle : Vectors.Element_Type) return Quaternion
   is
      use type Vectors.Element_Type;

      CA : constant Vectors.Element_Type := EF.Cos (Angle / 2.0);
      SA : constant Vectors.Element_Type := EF.Sin (Angle / 2.0);

      use Vectors;

      Result : Quaternion := Quaternion (Axis * SA);
   begin
      Result (W) := CA;
      return Normalize (Result);
   end R;

   function R (Left, Right : Vector4) return Quaternion is
      use type Vectors.Element_Type;

      S : constant Vector4 := Vectors.Normalize (Left);
      T : constant Vector4 := Vectors.Normalize (Right);

      E   : constant Vectors.Element_Type := Vectors.Dot (S, T);
      SRE : constant Vectors.Element_Type := EF.Sqrt (2.0 * (1.0 + E));

      use Vectors;
   begin
      --  Division by zero if Left and Right are in opposite direction.
      --  Use rotation axis perpendicular to s and angle of 180 degrees
      if SRE /= 0.0 then
         --  Equation 4.53 from chapter 4.3 Quaternions from Real-Time Rendering
         --  (third edition, 2008)
         declare
            Result : Quaternion := Quaternion ((1.0 / SRE) * Vectors.Cross (S, T));
         begin
            Result (W) := SRE / 2.0;
            return Normalize (Result);
         end;
      else
         if abs S (Z) < abs S (X) then
            return R ([S (Y), -S (X), 0.0, 0.0], Ada.Numerics.Pi);
         else
            return R ([0.0, -S (Z), S (Y), 0.0], Ada.Numerics.Pi);
         end if;
      end if;
   end R;

   function Difference (Left, Right : Quaternion) return Quaternion is
   begin
      return Right * Conjugate (Left);
   end Difference;

   function Rotate
     (Vector   : Vector4;
      Rotation : Quaternion) return Vector4
   is (Vectors.Vector_Type (Rotation * Quaternion (Vector) * Conjugate (Rotation)));

   function Lerp
     (Left, Right : Quaternion;
      Time        : Vectors.Element_Type) return Quaternion
   is
      use type Vectors.Element_Type;
   begin
      return Normalize ((1.0 - Time) * Left + Time * Right);
   end Lerp;

   function Slerp
     (Left, Right : Quaternion;
      Time        : Vectors.Element_Type) return Quaternion
   is
      use type Vectors.Element_Type;

      function Clamp (V : Vectors.Element_Type) return Vectors.Element_Type is
        (Vectors.Element_Type'Max (-1.0, Vectors.Element_Type'Min (V, 1.0)));

      Cos_Angle : constant Vectors.Element_Type := Vectors.Dot (Vector4 (Left), Vector4 (Right));
      Angle     : constant Vectors.Element_Type := EF.Arccos (Clamp (Cos_Angle));

      SA : constant Vectors.Element_Type := EF.Sin (Angle);

      SL : constant Vectors.Element_Type := EF.Sin ((1.0 - Time) * Angle);
      SR : constant Vectors.Element_Type := EF.Sin (Time * Angle);

      use Vectors;
   begin
      if SA = 0.0 then
         return Lerp (Left, Right, Time);
      else
         return Normalize ((SL / SA) * Left + (SR / SA) * Right);
      end if;
   end Slerp;

end Orka.Transforms.SIMD_Quaternions;
