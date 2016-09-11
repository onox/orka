--  Copyright (c) 2015 onox <denkpadje@gmail.com>
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

package body GL.Transforms is

   package Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions (Single);

   function To_Radians (Angle : Single) return Single is
   begin
      return Angle * Ada.Numerics.Pi / 180.0;
   end To_Radians;

   procedure Translate (Matrix : in out Matrix4; Offset : Vector3) is
   begin
      Matrix (W, X) := Matrix (X, X) * Offset (X) + Matrix (Y, X) * Offset (Y) + Matrix (Z, X) * Offset (Z) + Matrix (W, X);
      Matrix (W, Y) := Matrix (X, Y) * Offset (X) + Matrix (Y, Y) * Offset (Y) + Matrix (Z, Y) * Offset (Z) + Matrix (W, Y);
      Matrix (W, Z) := Matrix (X, Z) * Offset (X) + Matrix (Y, Z) * Offset (Y) + Matrix (Z, Z) * Offset (Z) + Matrix (W, Z);
      Matrix (W, W) := Matrix (X, W) * Offset (X) + Matrix (Y, W) * Offset (Y) + Matrix (Z, W) * Offset (Z) + Matrix (W, W);
   end Translate;

   procedure Rotate_X (Matrix : in out Matrix4; Angle : Single) is
      CA : constant Single := Elementary_Functions.Cos (To_Radians (Angle), 2.0 * Ada.Numerics.Pi);
      SA : constant Single := Elementary_Functions.Sin (To_Radians (Angle), 2.0 * Ada.Numerics.Pi);
      Result_Matrix : Matrix4 := Matrix;
   begin
      Result_Matrix (Y, X) := CA * Matrix (Y, X) + SA * Matrix (Z, X);
      Result_Matrix (Y, Y) := CA * Matrix (Y, Y) + SA * Matrix (Z, Y);
      Result_Matrix (Y, Z) := CA * Matrix (Y, Z) + SA * Matrix (Z, Z);
      Result_Matrix (Y, W) := CA * Matrix (Y, W) + SA * Matrix (Z, W);

      Result_Matrix (Z, X) := -SA * Matrix (Y, X) + CA * Matrix (Z, X);
      Result_Matrix (Z, Y) := -SA * Matrix (Y, Y) + CA * Matrix (Z, Y);
      Result_Matrix (Z, Z) := -SA * Matrix (Y, Z) + CA * Matrix (Z, Z);
      Result_Matrix (Z, W) := -SA * Matrix (Y, W) + CA * Matrix (Z, W);

      Matrix := Result_Matrix;
   end Rotate_X;

   procedure Rotate_Y (Matrix : in out Matrix4; Angle : Single) is
      CA : constant Single := Elementary_Functions.Cos (To_Radians (Angle), 2.0 * Ada.Numerics.Pi);
      SA : constant Single := Elementary_Functions.Sin (To_Radians (Angle), 2.0 * Ada.Numerics.Pi);
      Result_Matrix : Matrix4 := Matrix;
   begin
      Result_Matrix (X, X) := CA * Matrix (X, X) - SA * Matrix (Z, X);
      Result_Matrix (X, Y) := CA * Matrix (X, Y) - SA * Matrix (Z, Y);
      Result_Matrix (X, Z) := CA * Matrix (X, Z) - SA * Matrix (Z, Z);
      Result_Matrix (X, W) := CA * Matrix (X, W) - SA * Matrix (Z, W);

      Result_Matrix (Z, X) := SA * Matrix (X, X) + CA * Matrix (Z, X);
      Result_Matrix (Z, Y) := SA * Matrix (X, Y) + CA * Matrix (Z, Y);
      Result_Matrix (Z, Z) := SA * Matrix (X, Z) + CA * Matrix (Z, Z);
      Result_Matrix (Z, W) := SA * Matrix (X, W) + CA * Matrix (Z, W);

      Matrix := Result_Matrix;
   end Rotate_Y;

   procedure Rotate_Z (Matrix : in out Matrix4; Angle : Single) is
      CA : constant Single := Elementary_Functions.Cos (To_Radians (Angle), 2.0 * Ada.Numerics.Pi);
      SA : constant Single := Elementary_Functions.Sin (To_Radians (Angle), 2.0 * Ada.Numerics.Pi);
      Result_Matrix : Matrix4 := Matrix;
   begin
      Result_Matrix (X, X) := CA * Matrix (X, X) + SA * Matrix (Y, X);
      Result_Matrix (X, Y) := CA * Matrix (X, Y) + SA * Matrix (Y, Y);
      Result_Matrix (X, Z) := CA * Matrix (X, Z) + SA * Matrix (Y, Z);
      Result_Matrix (X, W) := CA * Matrix (X, W) + SA * Matrix (Y, W);

      Result_Matrix (Y, X) := -SA * Matrix (X, X) + CA * Matrix (Y, X);
      Result_Matrix (Y, Y) := -SA * Matrix (X, Y) + CA * Matrix (Y, Y);
      Result_Matrix (Y, Z) := -SA * Matrix (X, Z) + CA * Matrix (Y, Z);
      Result_Matrix (Y, W) := -SA * Matrix (X, W) + CA * Matrix (Y, W);

      Matrix := Result_Matrix;
   end Rotate_Z;

   procedure Rotate (Matrix    : in out Matrix4;
                     Direction : in     Vector3;
                     Angle     : in     Single) is
      CA : constant Single := Elementary_Functions.Cos (To_Radians (Angle), 2.0 * Ada.Numerics.Pi);
      SA : constant Single := Elementary_Functions.Sin (To_Radians (Angle), 2.0 * Ada.Numerics.Pi);

      MCA : constant Single := 1.0 - CA;

      MCARXY : constant Single := MCA * Direction (X) * Direction (Y);
      MCARXZ : constant Single := MCA * Direction (X) * Direction (Z);
      MCARYZ : constant Single := MCA * Direction (Y) * Direction (Z);

      RXSA : constant Single := Direction (X) * SA;
      RYSA : constant Single := Direction (Y) * SA;
      RZSA : constant Single := Direction (Z) * SA;

      R11 : constant Single := CA + MCA * Direction (X)**2;
      R12 : constant Single := MCARXY + RZSA;
      R13 : constant Single := MCARXZ - RYSA;

      R21 : constant Single := MCARXY - RZSA;
      R22 : constant Single := CA + MCA * Direction (Y)**2;
      R23 : constant Single := MCARYZ + RXSA;

      R31 : constant Single := MCARXZ + RYSA;
      R32 : constant Single := MCARYZ - RXSA;
      R33 : constant Single := CA + MCA * Direction (Z)**2;

      Result_Matrix : Matrix4 := Matrix;
   begin
      Result_Matrix (X, X) := R11 * Matrix (X, X) + R12 * Matrix (Y, X) + R13 * Matrix (Z, X);
      Result_Matrix (X, Y) := R11 * Matrix (X, Y) + R12 * Matrix (Y, Y) + R13 * Matrix (Z, X);
      Result_Matrix (X, Z) := R11 * Matrix (X, Z) + R12 * Matrix (Y, Z) + R13 * Matrix (Z, X);
      Result_Matrix (X, W) := R11 * Matrix (X, W) + R12 * Matrix (Y, W) + R13 * Matrix (Z, X);

      Result_Matrix (Y, X) := R21 * Matrix (X, X) + R22 * Matrix (Y, X) + R23 * Matrix (Z, X);
      Result_Matrix (Y, Y) := R21 * Matrix (X, Y) + R22 * Matrix (Y, Y) + R23 * Matrix (Z, X);
      Result_Matrix (Y, Z) := R21 * Matrix (X, Z) + R22 * Matrix (Y, Z) + R23 * Matrix (Z, X);
      Result_Matrix (Y, W) := R21 * Matrix (X, W) + R22 * Matrix (Y, W) + R23 * Matrix (Z, X);

      Result_Matrix (Z, X) := R31 * Matrix (X, X) + R32 * Matrix (Y, X) + R33 * Matrix (Z, X);
      Result_Matrix (Z, Y) := R31 * Matrix (X, Y) + R32 * Matrix (Y, Y) + R33 * Matrix (Z, X);
      Result_Matrix (Z, Z) := R31 * Matrix (X, Z) + R32 * Matrix (Y, Z) + R33 * Matrix (Z, X);
      Result_Matrix (Z, W) := R31 * Matrix (X, W) + R32 * Matrix (Y, W) + R33 * Matrix (Z, X);

      Matrix := Result_Matrix;
   end Rotate;

   procedure Scale (Matrix : in out Matrix4; Factors : Vector3) is
   begin
      Matrix (X, X) := Matrix (X, X) * Factors (X);
      Matrix (X, Y) := Matrix (X, Y) * Factors (X);
      Matrix (X, Z) := Matrix (X, Z) * Factors (X);
      Matrix (X, W) := Matrix (X, W) * Factors (X);

      Matrix (Y, X) := Matrix (Y, X) * Factors (Y);
      Matrix (Y, Y) := Matrix (Y, Y) * Factors (Y);
      Matrix (Y, Z) := Matrix (Y, Z) * Factors (Y);
      Matrix (Y, W) := Matrix (Y, W) * Factors (Y);

      Matrix (Z, X) := Matrix (Z, X) * Factors (Z);
      Matrix (Z, Y) := Matrix (Z, Y) * Factors (Z);
      Matrix (Z, Z) := Matrix (Z, Z) * Factors (Z);
      Matrix (Z, W) := Matrix (Z, W) * Factors (Z);
   end Scale;

   procedure Scale (Matrix : in out Matrix4; Factor : Single) is
      Factors : constant Vector3 := (Factor, Factor, Factor);
   begin
      Scale (Matrix, Factors);
   end Scale;

   function Perspective (FOV, Aspect, Z_Near, Z_Far : Single) return Matrix4 is
      F : constant Single := 1.0 / Elementary_Functions.Tan (FOV / 2.0, 360.0);
      Matrix : Matrix4 := Identity4;
   begin
      Matrix (X, X) := F / Aspect;
      Matrix (Y, Y) := F;
      Matrix (Z, Z) := (Z_Near + Z_Far) / (Z_Near - Z_Far);
      Matrix (W, Z) := (2.0 * Z_Near * Z_Far) / (Z_Near - Z_Far);
      Matrix (Z, W) := Single (-1.0);
      Matrix (W, W) := Single (0.0);
      return Matrix;
   end Perspective;

end GL.Transforms;
