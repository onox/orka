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

with Ahven; use Ahven;

with GL.Types;

with Orka.SIMD;
with Orka.Transforms.Doubles.Vectors;

package body Test_Transforms_Doubles_Vectors is

   use GL.Types;
   use Orka.SIMD;
   use Orka.Transforms.Doubles.Vectors;

   use type Vector4;

   package EF is new Ada.Numerics.Generic_Elementary_Functions (Double);

   function Is_Equivalent (Expected, Result : GL.Types.Double) return Boolean is
      Epsilon  : constant GL.Types.Double := 2.0 ** (1 - GL.Types.Double'Model_Mantissa);
   begin
      return Result in Expected - Epsilon .. Expected + Epsilon;
   end Is_Equivalent;

   overriding
   procedure Initialize (T : in out Test) is
   begin
      T.Set_Name ("Vectors");

      T.Add_Test_Routine (Test_Add'Access, "Test '+' operator");
      T.Add_Test_Routine (Test_Subtract'Access, "Test '-' operator");
      T.Add_Test_Routine (Test_Scale'Access, "Test '*' operator");
      T.Add_Test_Routine (Test_Absolute'Access, "Test 'abs' operator");
      T.Add_Test_Routine (Test_Magnitude'Access, "Test Magnitude function");
      T.Add_Test_Routine (Test_Normalize'Access, "Test Normalize function");
      T.Add_Test_Routine (Test_Distance'Access, "Test Distance function");
      T.Add_Test_Routine (Test_Projection'Access, "Test Projection function");
      T.Add_Test_Routine (Test_Perpendicular'Access, "Test Perpendicular function");
      T.Add_Test_Routine (Test_Angle'Access, "Test Angle function");
      T.Add_Test_Routine (Test_Dot_Product'Access, "Test Dot function");
      T.Add_Test_Routine (Test_Cross_Product'Access, "Test Cross function");
   end Initialize;

   procedure Test_Add is
      Left  : constant Vector4 := (2.0, 3.0, 4.0, 0.0);
      Right : constant Vector4 := (-2.0, 3.0, 0.0, -1.0);

      Expected : constant Vector4 := (0.0, 6.0, 4.0, -1.0);
      Result   : constant Vector4 := Left + Right;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Add;

   procedure Test_Subtract is
      Left  : constant Vector4 := (2.0, 3.0, 4.0, 0.0);
      Right : constant Vector4 := (-2.0, 3.0, 0.0, -1.0);

      Expected : constant Vector4 := (4.0, 0.0, 4.0, 1.0);
      Result   : constant Vector4 := Left - Right;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Subtract;

   procedure Test_Scale is
      Elements : constant Vector4 := (2.0, 3.0, 1.0, 0.0);

      Expected : constant Vector4 := (4.0, 6.0, 2.0, 0.0);
      Result   : constant Vector4 := 2.0 * Elements;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Scale;

   procedure Test_Absolute is
      Elements : constant Vector4 := (-2.0, 0.0, 1.0, -1.0);

      Expected : constant Vector4 := (2.0, 0.0, 1.0, 1.0);
      Result   : constant Vector4 := abs Elements;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Absolute;

   procedure Test_Magnitude is
      Elements : constant Vector4 := (1.0, -2.0, 3.0, -4.0);

      Expected : constant GL.Types.Double := EF.Sqrt (1.0**2 + (-2.0)**2 + 3.0**2 + (-4.0)**2);
      Result   : constant GL.Types.Double := Magnitude (Elements);
   begin
      Assert (Is_Equivalent (Expected, Result), "Unexpected Double " & GL.Types.Double'Image (Result));
   end Test_Magnitude;

   procedure Test_Normalize is
      Elements : constant Vector4 := (1.0, -2.0, 3.0, -4.0);

      Expected : constant GL.Types.Double := 1.0;
      Result   : constant GL.Types.Double := Magnitude (Normalize (Elements));
   begin
      Assert (Is_Equivalent (Expected, Result), "Unexpected Double " & GL.Types.Double'Image (Result));
   end Test_Normalize;

   procedure Test_Distance is
      Left  : constant Vector4 := (2.0, 5.0, 0.0, 0.0);
      Right : constant Vector4 := (2.0, 2.0, 0.0, 0.0);

      Expected : constant GL.Types.Double := 3.0;
      Result   : constant GL.Types.Double := Distance (Left, Right);
   begin
      Assert (Is_Equivalent (Expected, Result), "Unexpected Double " & GL.Types.Double'Image (Result));
   end Test_Distance;

   procedure Test_Projection is
      Elements  : constant Vector4 := (3.0, 4.0, 0.0, 0.0);
      Direction : constant Vector4 := (0.0, 1.0, 0.0, 0.0);

      Expected : constant Vector4 := (0.0, 4.0, 0.0, 0.0);
      Result   : constant Vector4 := Projection (Elements, Direction);
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Projection;

   procedure Test_Perpendicular is
      Elements  : constant Vector4 := (3.0, 4.0, 0.0, 0.0);
      Direction : constant Vector4 := (0.0, 1.0, 0.0, 0.0);

      Expected : constant Vector4 := (3.0, 0.0, 0.0, 0.0);
      Result   : constant Vector4 := Perpendicular (Elements, Direction);
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Perpendicular;

   procedure Test_Angle is
      Left  : constant Vector4 := (3.0, 0.0, 0.0, 0.0);
      Right : constant Vector4 := (0.0, 4.0, 0.0, 0.0);

      Expected : constant GL.Types.Double := 90.0;
      Result   : constant GL.Types.Double := Angle (Left, Right);
   begin
      Assert (Is_Equivalent (Expected, Result), "Unexpected Double " & GL.Types.Double'Image (Result));
   end Test_Angle;

   procedure Test_Dot_Product is
      Left  : constant Vector4 := (1.0, 2.0, 3.0, 4.0);
      Right : constant Vector4 := (2.0, 3.0, 4.0, 5.0);

      Expected : constant GL.Types.Double := 40.0;
      Result   : constant GL.Types.Double := Dot (Left, Right);
   begin
      Assert (Is_Equivalent (Expected, Result), "Unexpected Double " & GL.Types.Double'Image (Result));
   end Test_Dot_Product;

   procedure Test_Cross_Product is
      Left  : constant Vector4 := (2.0, 4.0, 8.0, 0.0);
      Right : constant Vector4 := (5.0, 6.0, 7.0, 0.0);

      Expected : constant Vector4 := (-20.0, 26.0, -8.0, 0.0);
      Result   : constant Vector4 := Cross (Left, Right);
   begin
      for I in X .. Z loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Cross_Product;

end Test_Transforms_Doubles_Vectors;
