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

with AUnit.Assertions;
with AUnit.Test_Caller;

with Orka.Transforms.Doubles.Vectors;

package body Test_Transforms_Doubles_Vectors is

   use Orka;
   use Orka.Transforms.Doubles.Vectors;

   use type Vector4;

   package EF is new Ada.Numerics.Generic_Elementary_Functions (Float_64);

   function Is_Equivalent (Expected, Result : Float_64) return Boolean is
     (abs (Result - Expected) <= Float_64'Model_Epsilon);

   use AUnit.Assertions;

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(Transforms - Doubles - Vectors) ";
   begin
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '+' operator", Test_Add'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '-' operator", Test_Subtract'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '*' operator", Test_Scale'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test 'abs' operator", Test_Absolute'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Magnitude function", Test_Magnitude'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Normalize function", Test_Normalize'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Distance function", Test_Distance'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Projection function", Test_Projection'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Perpendicular function", Test_Perpendicular'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Angle function", Test_Angle'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Dot function", Test_Dot_Product'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Cross function", Test_Cross_Product'Access));

      return Test_Suite'Access;
   end Suite;

   procedure Test_Add (Object : in out Test) is
      Left  : constant Vector4 := (2.0, 3.0, 4.0, 0.0);
      Right : constant Vector4 := (-2.0, 3.0, 0.0, -1.0);

      Expected : constant Vector4 := (0.0, 6.0, 4.0, -1.0);
      Result   : constant Vector4 := Left + Right;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & I'Image);
      end loop;
   end Test_Add;

   procedure Test_Subtract (Object : in out Test) is
      Left  : constant Vector4 := (2.0, 3.0, 4.0, 0.0);
      Right : constant Vector4 := (-2.0, 3.0, 0.0, -1.0);

      Expected : constant Vector4 := (4.0, 0.0, 4.0, 1.0);
      Result   : constant Vector4 := Left - Right;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & I'Image);
      end loop;
   end Test_Subtract;

   procedure Test_Scale (Object : in out Test) is
      Elements : constant Vector4 := (2.0, 3.0, 1.0, 0.0);

      Expected : constant Vector4 := (4.0, 6.0, 2.0, 0.0);
      Result   : constant Vector4 := 2.0 * Elements;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & I'Image);
      end loop;
   end Test_Scale;

   procedure Test_Absolute (Object : in out Test) is
      Elements : constant Vector4 := (-2.0, 0.0, 1.0, -1.0);

      Expected : constant Vector4 := (2.0, 0.0, 1.0, 1.0);
      Result   : constant Vector4 := abs Elements;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & I'Image);
      end loop;
   end Test_Absolute;

   procedure Test_Magnitude (Object : in out Test) is
      Elements : constant Vector4 := (1.0, -2.0, 3.0, -4.0);

      Expected : constant Float_64 := EF.Sqrt (1.0**2 + (-2.0)**2 + 3.0**2 + (-4.0)**2);
      Result   : constant Float_64 := Magnitude (Elements);
   begin
      Assert (Is_Equivalent (Expected, Result), "Unexpected Double " & Result'Image);
   end Test_Magnitude;

   procedure Test_Normalize (Object : in out Test) is
      Elements : constant Vector4 := (1.0, -2.0, 3.0, -4.0);

      Expected : constant Float_64 := 1.0;
      Result   : constant Float_64 := Magnitude (Normalize (Elements));
   begin
      Assert (Is_Equivalent (Expected, Result), "Unexpected Double " & Result'Image);
   end Test_Normalize;

   procedure Test_Distance (Object : in out Test) is
      Left  : constant Point := (2.0, 5.0, 0.0, 1.0);
      Right : constant Point := (2.0, 2.0, 0.0, 1.0);

      Expected : constant Float_64 := 3.0;
      Result   : constant Float_64 := Distance (Left, Right);
   begin
      Assert (Is_Equivalent (Expected, Result), "Unexpected Double " & Result'Image);
   end Test_Distance;

   procedure Test_Projection (Object : in out Test) is
      Elements  : constant Vector4 := (3.0, 4.0, 0.0, 0.0);
      Direction : constant Vector4 := (0.0, 1.0, 0.0, 0.0);

      Expected : constant Vector4 := (0.0, 4.0, 0.0, 0.0);
      Result   : constant Vector4 := Projection (Elements, Direction);
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & I'Image);
      end loop;
   end Test_Projection;

   procedure Test_Perpendicular (Object : in out Test) is
      Elements  : constant Vector4 := (3.0, 4.0, 0.0, 0.0);
      Direction : constant Vector4 := (0.0, 1.0, 0.0, 0.0);

      Expected : constant Vector4 := (3.0, 0.0, 0.0, 0.0);
      Result   : constant Vector4 := Perpendicular (Elements, Direction);
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & I'Image);
      end loop;
   end Test_Perpendicular;

   procedure Test_Angle (Object : in out Test) is
      Left  : constant Vector4 := (3.0, 0.0, 0.0, 0.0);
      Right : constant Vector4 := (0.0, 4.0, 0.0, 0.0);

      Expected : constant Float_64 := To_Radians (90.0);
      Result   : constant Float_64 := Angle (Left, Right);
   begin
      Assert (Is_Equivalent (Expected, Result), "Unexpected Double " & Result'Image);
   end Test_Angle;

   procedure Test_Dot_Product (Object : in out Test) is
      Left  : constant Vector4 := (1.0, 2.0, 3.0, 4.0);
      Right : constant Vector4 := (2.0, 3.0, 4.0, 5.0);

      Expected : constant Float_64 := 40.0;
      Result   : constant Float_64 := Dot (Left, Right);
   begin
      Assert (Is_Equivalent (Expected, Result), "Unexpected Double " & Result'Image);
   end Test_Dot_Product;

   procedure Test_Cross_Product (Object : in out Test) is
      Left  : constant Vector4 := (2.0, 4.0, 8.0, 0.0);
      Right : constant Vector4 := (5.0, 6.0, 7.0, 0.0);

      Expected : constant Vector4 := (-20.0, 26.0, -8.0, 0.0);
      Result   : constant Vector4 := Cross (Left, Right);
   begin
      for I in X .. Z loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & I'Image);
      end loop;
   end Test_Cross_Product;

end Test_Transforms_Doubles_Vectors;
