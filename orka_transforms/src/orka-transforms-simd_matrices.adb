--  SPDX-License-Identifier: Apache-2.0
--
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

   function Diagonal (Elements : Vector_Type) return Matrix_Type is
      Result : Matrix_Type := Identity_Matrix;
   begin
      for Index in Elements'Range loop
         Result (Index) (Index) := Elements (Index);
      end loop;
      return Result;
   end Diagonal;

   function Main_Diagonal (Matrix : Matrix_Type) return Vector_Type is
     (Matrix (X) (X), Matrix (Y) (Y), Matrix (Z) (Z), Matrix (W) (W));

   function Trace (Matrix : Matrix_Type) return Element_Type is
     (Vectors.Sum (Main_Diagonal (Matrix)));

   function "*" (Left : Vector_Type; Right : Matrix_Type) return Vector_Type is
      Result : Vector_Type;
   begin
      for Column in Right'Range loop
         Result (Column) := Vectors.Dot (Left, Right (Column));
      end loop;

      return Result;
   end "*";

   function Outer (Left, Right : Vector_Type) return Matrix_Type is
      use Vectors;

      Result : Matrix_Type;
   begin
      for Index in Right'Range loop
         Result (Index) := Right (Index) * Left;
      end loop;

      return Result;
   end Outer;

   ----------------------------------------------------------------------------

   function T (Offset : Vectors.Point) return Matrix_Type is
      Result : Matrix_Type := Identity_Matrix;
   begin
      Result (W) := Vector_Type (Offset);
      return Result;
   end T;

   function T (Offset : Vector_Type) return Matrix_Type is
      Result : Matrix_Type := Identity_Matrix;
   begin
      Result (W) := Offset;
      Result (W) (W) := 1.0;
      return Result;
   end T;

   function Rx (Angle : Element_Type) return Matrix_Type is
      CA : constant Element_Type := EF.Cos (Angle);
      SA : constant Element_Type := EF.Sin (Angle);
      Result : Matrix_Type := Identity_Matrix;
   begin
      Result (Y) := (0.0,  CA, SA, 0.0);
      Result (Z) := (0.0, -SA, CA, 0.0);
      return Result;
   end Rx;

   function Ry (Angle : Element_Type) return Matrix_Type is
      CA : constant Element_Type := EF.Cos (Angle);
      SA : constant Element_Type := EF.Sin (Angle);
      Result : Matrix_Type := Identity_Matrix;
   begin
      Result (X) := (CA, 0.0, -SA, 0.0);
      Result (Z) := (SA, 0.0,  CA, 0.0);
      return Result;
   end Ry;

   function Rz (Angle : Element_Type) return Matrix_Type is
      CA : constant Element_Type := EF.Cos (Angle);
      SA : constant Element_Type := EF.Sin (Angle);
      Result : Matrix_Type := Identity_Matrix;
   begin
      Result (X) := (CA,  SA, 0.0, 0.0);
      Result (Y) := (-SA, CA, 0.0, 0.0);
      return Result;
   end Rz;

   function R (Axis : Vector_Type; Angle : Element_Type) return Matrix_Type is
      CA : constant Element_Type := EF.Cos (Angle);
      SA : constant Element_Type := EF.Sin (Angle);

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

      Result : Matrix_Type;
   begin
      Result (X) := (R11, R12, R13, 0.0);
      Result (Y) := (R21, R22, R23, 0.0);
      Result (Z) := (R31, R32, R33, 0.0);
      Result (W) := (0.0, 0.0, 0.0, 1.0);
      return Result;
   end R;

   function R (Quaternion : Vector_Type) return Matrix_Type is
      Result : Matrix_Type := Identity_Matrix;

      Q_X : constant Element_Type := Quaternion (X);
      Q_Y : constant Element_Type := Quaternion (Y);
      Q_Z : constant Element_Type := Quaternion (Z);
      Q_W : constant Element_Type := Quaternion (W);

      S : constant := 2.0;
      --  S = 2 / Norm (Quaternion)
   begin
      Result (X) (X) := 1.0 - S * (Q_Y * Q_Y + Q_Z * Q_Z);
      Result (X) (Y) := S * (Q_X * Q_Y + Q_Z * Q_W);
      Result (X) (Z) := S * (Q_X * Q_Z - Q_Y * Q_W);

      Result (Y) (X) := S * (Q_X * Q_Y - Q_Z * Q_W);
      Result (Y) (Y) := 1.0 - S * (Q_X * Q_X + Q_Z * Q_Z);
      Result (Y) (Z) := S * (Q_Y * Q_Z + Q_X * Q_W);

      Result (Z) (X) := S * (Q_X * Q_Z + Q_Y * Q_W);
      Result (Z) (Y) := S * (Q_Y * Q_Z - Q_X * Q_W);
      Result (Z) (Z) := 1.0 - S * (Q_X * Q_X + Q_Y * Q_Y);

      return Result;
   end R;

   function S (Factors : Vector_Type) return Matrix_Type is
      Result : Matrix_Type := Identity_Matrix;
   begin
      Result (X) (X) := Factors (X);
      Result (Y) (Y) := Factors (Y);
      Result (Z) (Z) := Factors (Z);
      return Result;
   end S;

   ----------------------------------------------------------------------------

   function FOV (Width, Distance : Element_Type) return Element_Type is
     (2.0 * EF.Arctan (Width  / (2.0 * Distance)));

   function Finite_Perspective (FOV, Aspect, Z_Near, Z_Far : Element_Type) return Matrix_Type is
      F : constant Element_Type := 1.0 / EF.Tan (0.5 * FOV);
      Result : Matrix_Type := Identity_Matrix;
   begin
      Result (X) (X) := F / Aspect;
      Result (Y) (Y) := F;

      --  Depth normalized to [0, 1] instead of [-1 , 1]
      Result (Z) (Z) := Z_Far / (Z_Near - Z_Far);
      Result (W) (Z) := (Z_Near * Z_Far) / (Z_Near - Z_Far);

      Result (Z) (W) := Element_Type (-1.0);
      Result (W) (W) := Element_Type (0.0);
      return Result;
   end Finite_Perspective;

   function Infinite_Perspective (FOV, Aspect, Z_Near : Element_Type) return Matrix_Type is
      F : constant Element_Type := 1.0 / EF.Tan (0.5 * FOV);
      Result : Matrix_Type := Identity_Matrix;
   begin
      Result (X) (X) := F / Aspect;
      Result (Y) (Y) := F;

      --  Depth normalized to [0, 1] instead of [-1 , 1]
      Result (Z) (Z) := Element_Type (-1.0);
      Result (W) (Z) := -Z_Near;

      Result (Z) (W) := Element_Type (-1.0);
      Result (W) (W) := Element_Type (0.0);

      return Result;
   end Infinite_Perspective;

   function Infinite_Perspective_Reversed_Z
     (FOV, Aspect, Z_Near : Element_Type) return Matrix_Type
   is
      F : constant Element_Type := 1.0 / EF.Tan (0.5 * FOV);
      Result : Matrix_Type := Identity_Matrix;
   begin
      Result (X) (X) := F / Aspect;
      Result (Y) (Y) := F;

      --  Depth normalized to [1, 0] instead of [-1 , 1]
      Result (Z) (Z) := Element_Type (0.0);
      Result (W) (Z) := Z_Near;

      Result (Z) (W) := Element_Type (-1.0);
      Result (W) (W) := Element_Type (0.0);

      return Result;
   end Infinite_Perspective_Reversed_Z;

   function Orthographic (X_Mag, Y_Mag, Z_Near, Z_Far : Element_Type) return Matrix_Type is
      Result : Matrix_Type := Identity_Matrix;
   begin
      Result (X) (X) := 2.0 / X_Mag;
      Result (Y) (Y) := 2.0 / Y_Mag;

      --  Depth normalized to [0, 1] instead of [-1, 1]
      Result (Z) (Z) := -1.0 / (Z_Far - Z_Near);
      Result (W) (Z) := -Z_Near / (Z_Far - Z_Near);

      return Result;
   end Orthographic;

end Orka.Transforms.SIMD_Matrices;
