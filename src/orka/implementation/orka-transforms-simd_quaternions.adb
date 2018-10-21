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

with Orka.SIMD;

package body Orka.Transforms.SIMD_Quaternions is

   package EF is new Ada.Numerics.Generic_Elementary_Functions (Vectors.Element_Type);

   function Vector_Part (Elements : Quaternion) return Vector4 is
   begin
      return Result : Vector4 := Vector4 (Elements) do
         Result (SIMD.W) := 0.0;
      end return;
   end Vector_Part;

   function "*" (Left, Right : Quaternion) return Quaternion is
      use Vectors;

      Lv : constant Vector4 := Vector_Part (Left);
      Rv : constant Vector4 := Vector_Part (Right);

      Ls : constant Vectors.Element_Type := Left (SIMD.W);
      Rs : constant Vectors.Element_Type := Right (SIMD.W);

      V : constant Vector4 := Ls * Rv + Rs * Lv + Vectors.Cross (Lv, Rv);
      S : constant Element_Type := Ls * Rs - Vectors.Dot (Lv, Rv);
   begin
      return Result : Quaternion := Quaternion (V) do
         Result (SIMD.W) := S;
      end return;
   end "*";

   function Conjugate (Elements : Quaternion) return Quaternion is
      W : constant Vectors.Element_Type := Elements (SIMD.W);

      use Vectors;
   begin
      return Result : Quaternion := Quaternion (-Vector4 (Elements)) do
         Result (SIMD.W) := W;
      end return;
   end Conjugate;

   function Norm (Elements : Quaternion) return Vectors.Element_Type is
     (Vectors.Magnitude (Vector4 (Elements)));

   function Normalize (Elements : Quaternion) return Quaternion is
     (Quaternion (Vectors.Normalize (Vector4 (Elements))));

   function Normalized (Elements : Quaternion) return Boolean is
     (Vectors.Normalized (Vector4 (Elements)));

   function R
     (Axis  : Vector4;
      Angle : Vectors.Element_Type) return Quaternion
   is
      use type Vectors.Element_Type;

      CA : constant Vectors.Element_Type := EF.Cos (Angle / 2.0, 360.0);
      SA : constant Vectors.Element_Type := EF.Sin (Angle / 2.0, 360.0);

      use Vectors;
   begin
      return Result : Quaternion := Quaternion (Axis * SA) do
         Result (SIMD.W) := CA;
      end return;
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
            Result (SIMD.W) := SRE / 2.0;
            return Normalize (Result);
         end;
      else
         if abs S (SIMD.Z) < abs S (SIMD.X) then
            return R ((S (SIMD.Y), -S (SIMD.X), 0.0, 0.0), 180.0);
         else
            return R ((0.0, -S (SIMD.Z), S (SIMD.Y), 0.0), 180.0);
         end if;
      end if;
   end R;

   function Difference (Left, Right : Quaternion) return Quaternion is
   begin
      return Right * Conjugate (Left);
   end Difference;

   procedure Rotate_At_Origin
     (Vector   : in out Vector4;
      Elements : Quaternion) is
   begin
      Vector := Vectors.Vector_Type (Elements * Quaternion (Vector) * Conjugate (Elements));
   end Rotate_At_Origin;

   function Lerp
     (Left, Right : Quaternion;
      Time        : Vectors.Element_Type) return Quaternion
   is
      use type Vectors.Element_Type;
      use Vectors;
   begin
      return Normalize (Quaternion ((1.0 - Time) * Vector4 (Left) + Time * Vector4 (Right)));
   end Lerp;

   function Slerp
     (Left, Right : Quaternion;
      Time        : Vectors.Element_Type) return Quaternion
   is
      use type Vectors.Element_Type;

      Cos_Angle : constant Vectors.Element_Type := Vectors.Dot (Vector4 (Left), Vector4 (Right));
      Angle     : constant Vectors.Element_Type := EF.Arccos (Cos_Angle, 360.0);

      SA : constant Vectors.Element_Type := EF.Sin (Angle, 360.0);

      SL : constant Vectors.Element_Type := EF.Sin ((1.0 - Time) * Angle, 360.0);
      SR : constant Vectors.Element_Type := EF.Sin (Time * Angle, 360.0);

      use Vectors;
   begin
      return Quaternion ((SL / SA) * Vector4 (Left) + (SR / SA) * Vector4 (Right));
   end Slerp;

end Orka.Transforms.SIMD_Quaternions;
