--------------------------------------------------------------------------------
-- Copyright (c) 2015 onox <denkpadje@gmail.com>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with Ada.Numerics.Generic_Elementary_Functions;

package body GL.Transforms is

   package Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions (Single);

   function To_Radians (Angle : Single) return Single is
   begin
      return Angle * Ada.Numerics.Pi / 180.0;
   end To_Radians;

   procedure Translate (Matrix : in out Matrix4; Vector : Vector3) is
   begin
      Matrix (W, X) := Matrix (X, X) * Vector (X) + Matrix (Y, X) * Vector (Y) + Matrix (Z, X) * Vector (Z) + Matrix (W, X);
      Matrix (W, Y) := Matrix (X, Y) * Vector (X) + Matrix (Y, Y) * Vector (Y) + Matrix (Z, Y) * Vector (Z) + Matrix (W, Y);
      Matrix (W, Z) := Matrix (X, Z) * Vector (X) + Matrix (Y, Z) * Vector (Y) + Matrix (Z, Z) * Vector (Z) + Matrix (W, Z);
      Matrix (W, W) := Matrix (X, W) * Vector (X) + Matrix (Y, W) * Vector (Y) + Matrix (Z, W) * Vector (Z) + Matrix (W, W);
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

   procedure Scale (Matrix : in out Matrix4; Vector : Vector3) is
   begin
      Matrix (X, X) := Matrix (X, X) * Vector (X);
      Matrix (X, Y) := Matrix (X, Y) * Vector (X);
      Matrix (X, Z) := Matrix (X, Y) * Vector (X);
      Matrix (X, W) := Matrix (X, W) * Vector (X);

      Matrix (Y, X) := Matrix (Y, X) * Vector (Y);
      Matrix (Y, Y) := Matrix (Y, Y) * Vector (Y);
      Matrix (Y, Z) := Matrix (Y, Z) * Vector (Y);
      Matrix (Y, W) := Matrix (Y, W) * Vector (Y);

      Matrix (Z, X) := Matrix (Z, X) * Vector (Z);
      Matrix (Z, Y) := Matrix (Z, Y) * Vector (Z);
      Matrix (Z, Z) := Matrix (Z, Z) * Vector (Z);
      Matrix (Z, W) := Matrix (Z, W) * Vector (Z);
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
