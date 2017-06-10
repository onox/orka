--  Copyright (c) 2016 onox <denkpadje@gmail.com>
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

package body Orka.Transforms.SIMD_Matrices is

   package EF is new Ada.Numerics.Generic_Elementary_Functions (Element_Type);

   use SIMD;

   function T (Offset : Vector_Type) return Matrix_Type is
      Result : Matrix_Type := Identity_Value;
      Old_W  : constant Element_Type := Result (W) (W);
   begin
      Result (W) := Offset;
      Result (W) (W) := Old_W;
      return Result;
   end T;

   function Rx (Angle : Element_Type) return Matrix_Type is
      CA : constant Element_Type := EF.Cos (Angle, 360.0);
      SA : constant Element_Type := EF.Sin (Angle, 360.0);
      Result : Matrix_Type := Identity_Value;
   begin
      Result (Y) := (0.0,  CA, SA, 0.0);
      Result (Z) := (0.0, -SA, CA, 0.0);
      return Result;
   end Rx;

   function Ry (Angle : Element_Type) return Matrix_Type is
      CA : constant Element_Type := EF.Cos (Angle, 360.0);
      SA : constant Element_Type := EF.Sin (Angle, 360.0);
      Result : Matrix_Type := Identity_Value;
   begin
      Result (X) := (CA, 0.0, -SA, 0.0);
      Result (Z) := (SA, 0.0,  CA, 0.0);
      return Result;
   end Ry;

   function Rz (Angle : Element_Type) return Matrix_Type is
      CA : constant Element_Type := EF.Cos (Angle, 360.0);
      SA : constant Element_Type := EF.Sin (Angle, 360.0);
      Result : Matrix_Type := Identity_Value;
   begin
      Result (X) := (CA,  SA, 0.0, 0.0);
      Result (Y) := (-SA, CA, 0.0, 0.0);
      return Result;
   end Rz;

   function R (Axis : Vector_Type; Angle : Element_Type) return Matrix_Type is
      CA : constant Element_Type := EF.Cos (Angle, 360.0);
      SA : constant Element_Type := EF.Sin (Angle, 360.0);
      Result : Matrix_Type := Identity_Value;

      MCA : constant Element_Type := 1.0 - CA;

      MCARXY : constant Element_Type := MCA * Axis (X) * Axis (Y);
      MCARXZ : constant Element_Type := MCA * Axis (X) * Axis (Z);
      MCARYZ : constant Element_Type := MCA * Axis (Y) * Axis (Z);

      RXSA : constant Element_Type := Axis (X) * SA;
      RYSA : constant Element_Type := Axis (Y) * SA;
      RZSA : constant Element_Type := Axis (Z) * SA;

      R11 : constant Element_Type := CA + MCA * Axis (X)**2;
      R12 : constant Element_Type := MCARXY + RZSA;
      R13 : constant Element_Type := MCARXZ - RYSA;

      R21 : constant Element_Type := MCARXY - RZSA;
      R22 : constant Element_Type := CA + MCA * Axis (Y)**2;
      R23 : constant Element_Type := MCARYZ + RXSA;

      R31 : constant Element_Type := MCARXZ + RYSA;
      R32 : constant Element_Type := MCARYZ - RXSA;
      R33 : constant Element_Type := CA + MCA * Axis (Z)**2;
   begin
      Result (X) := (R11, R12, R13, 0.0);
      Result (Y) := (R21, R22, R23, 0.0);
      Result (Z) := (R31, R32, R33, 0.0);
      return Result;
   end R;

   function R (Quaternion : Vector_Type) return Matrix_Type is
      Result : Matrix_Type := Identity_Value;
   begin
      Result (X) (X) := 1.0 - 2.0 * (Quaternion (Y) * Quaternion (Y) + Quaternion (Z) * Quaternion (Z));
      Result (X) (Y) := 2.0 * (Quaternion (X) * Quaternion (Y) - Quaternion (Z) * Quaternion (W));
      Result (X) (Z) := 2.0 * (Quaternion (X) * Quaternion (Z) + Quaternion (Y) * Quaternion (W));

      Result (Y) (X) := 2.0 * (Quaternion (X) * Quaternion (Y) + Quaternion (Z) * Quaternion (W));
      Result (Y) (Y) := 1.0 - 2.0 * (Quaternion (X) * Quaternion (X) + Quaternion (Z) * Quaternion (Z));
      Result (Y) (Z) := 2.0 * (Quaternion (Y) * Quaternion (Z) - Quaternion (X) * Quaternion (W));

      Result (Z) (X) := 2.0 * (Quaternion (X) * Quaternion (Z) - Quaternion (Y) * Quaternion (W));
      Result (Z) (Y) := 2.0 * (Quaternion (Y) * Quaternion (Z) + Quaternion (X) * Quaternion (W));
      Result (Z) (Z) := 1.0 - 2.0 * (Quaternion (X) * Quaternion (X) + Quaternion (Y) * Quaternion (Y));

      return Result;
   end R;

   function S (Factors : Vector_Type) return Matrix_Type is
      Result : Matrix_Type := Identity_Value;
   begin
      Result (X) (X) := Factors (X);
      Result (Y) (Y) := Factors (Y);
      Result (Z) (Z) := Factors (Z);
      return Result;
   end S;

   function "+" (Offset : Vector_Type; Matrix : Matrix_Type) return Matrix_Type is
   begin
      return T (Offset) * Matrix;
   end "+";

   function "*" (Factor : Element_Type; Matrix : Matrix_Type) return Matrix_Type is
   begin
      return S ((Factor, Factor, Factor, 1.0)) * Matrix;
   end "*";

   procedure Rotate_At_Origin (Matrix : in out Matrix_Type; Axis : Vector_Type; Angle : Element_Type) is
   begin
      Matrix := R (Axis, Angle) * Matrix;
   end Rotate_At_Origin;

   procedure Rotate (Matrix : in out Matrix_Type; Axis : Vector_Type;
                     Angle  : Element_Type; Point : Vector_Type) is
   begin
      Matrix := T (Point) * R (Axis, Angle) * T (-Point) * Matrix;
   end Rotate;

   procedure Rotate_At_Origin (Matrix : in out Matrix_Type; Quaternion : Vector_Type) is
   begin
      Matrix := R (Quaternion) * Matrix;
   end Rotate_At_Origin;

   procedure Rotate (Matrix : in out Matrix_Type; Quaternion : Vector_Type;
                     Point  : Vector_Type) is
   begin
      Matrix := T (Point) * R (Quaternion) * T (-Point) * Matrix;
   end Rotate;

   procedure Rotate_X_At_Origin (Matrix : in out Matrix_Type; Angle : Element_Type) is
   begin
      Matrix := Rx (Angle) * Matrix;
   end Rotate_X_At_Origin;

   procedure Rotate_X (Matrix : in out Matrix_Type; Angle : Element_Type; Point : Vector_Type) is
   begin
      Matrix := T (Point) * Rx (Angle) * T (-Point) * Matrix;
   end Rotate_X;

   procedure Rotate_Y_At_Origin (Matrix : in out Matrix_Type; Angle : Element_Type) is
   begin
      Matrix := Ry (Angle) * Matrix;
   end Rotate_Y_At_Origin;

   procedure Rotate_Y (Matrix : in out Matrix_Type; Angle : Element_Type; Point : Vector_Type) is
   begin
      Matrix := T (Point) * Ry (Angle) * T (-Point) * Matrix;
   end Rotate_Y;

   procedure Rotate_Z_At_Origin (Matrix : in out Matrix_Type; Angle : Element_Type) is
   begin
      Matrix := Rz (Angle) * Matrix;
   end Rotate_Z_At_Origin;

   procedure Rotate_Z (Matrix : in out Matrix_Type; Angle : Element_Type; Point : Vector_Type) is
   begin
      Matrix := T (Point) * Rz (Angle) * T (-Point) * Matrix;
   end Rotate_Z;

   procedure Translate (Matrix : in out Matrix_Type; Offset : Vector_Type) is
   begin
      Matrix := Offset + Matrix;
   end Translate;

   procedure Scale (Matrix : in out Matrix_Type; Factors : Vector_Type) is
   begin
      Matrix := S (Factors) * Matrix;
   end Scale;

   procedure Scale (Matrix : in out Matrix_Type; Factor : Element_Type) is
   begin
      Matrix := Factor * Matrix;
   end Scale;

   procedure Transpose (Matrix : in out Matrix_Type) is
   begin
      Matrix := Transpose_Matrix (Matrix);
   end Transpose;

   function Finite_Perspective (FOV, Aspect, Z_Near, Z_Far : Element_Type) return Matrix_Type is
      F : constant Element_Type := 1.0 / EF.Tan (0.5 * FOV, 360.0);
      Result : Matrix_Type := Identity_Value;
   begin
      Result (X) (X) := F / Aspect;
      Result (Y) (Y) := F;
      Result (Z) (Z) := (Z_Near + Z_Far) / (Z_Near - Z_Far);
      Result (W) (Z) := (2.0 * Z_Near * Z_Far) / (Z_Near - Z_Far);
      Result (Z) (W) := Element_Type (-1.0);
      Result (W) (W) := Element_Type (0.0);
      return Result;
   end Finite_Perspective;

   function Infinite_Perspective (FOV, Aspect, Z_Near : Element_Type) return Matrix_Type is
      F : constant Element_Type := 1.0 / EF.Tan (0.5 * FOV, 360.0);
      Result : Matrix_Type := Identity_Value;
   begin
      Result (X) (X) := F / Aspect;
      Result (Y) (Y) := F;
      Result (Z) (Z) := Element_Type (-1.0);
      Result (W) (Z) := -2.0 * Z_Near;
      Result (Z) (W) := Element_Type (-1.0);
      Result (W) (W) := Element_Type (0.0);
      return Result;
   end Infinite_Perspective;

   function Orthographic (X_Mag, Y_Mag, Z_Near, Z_Far : Element_Type) return Matrix_Type is
      Result : Matrix_Type := Identity_Value;
   begin
      Result (X) (X) := 1.0 / X_Mag;
      Result (Y) (Y) := 1.0 / Y_Mag;
      Result (Z) (Z) := 2.0 / (Z_Near - Z_Far);
      Result (W) (Z) := (Z_Near + Z_Far) / (Z_Near - Z_Far);
      Result (W) (W) := Element_Type (1.0);
      return Result;
   end Orthographic;

end Orka.Transforms.SIMD_Matrices;
