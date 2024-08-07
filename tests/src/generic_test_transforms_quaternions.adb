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
with AUnit.Test_Fixtures;

package body Generic_Test_Transforms_Quaternions is

   use Orka;
   use Quaternions;

   use AUnit.Assertions;

   use type Vector4;

   subtype Element_Type is Vectors.Element_Type;
   use type Element_Type;

   package EF is new Ada.Numerics.Generic_Elementary_Functions (Element_Type);

   function To_Radians (Angle : Element_Type) return Element_Type renames Vectors.To_Radians;

   function Is_Equivalent (Expected, Result : Element_Type) return Boolean is
     (abs (Result - Expected) <= Element_Type'Model_Epsilon + 1.0e-05 * abs Expected);

   procedure Assert_Equivalent (Expected, Result : Vector4) is
   begin
      for I in Index_4D loop
         Assert (Is_Equivalent (Expected (I), Result (I)),
           "Unexpected element " & Expected (I)'Image & " instead of " & Result (I)'Image &
           " at " & I'Image);
      end loop;
   end Assert_Equivalent;

   procedure Assert_Equivalent (Expected, Result : Quaternion) is
   begin
      for I in Index_4D loop
         Assert (Is_Equivalent (Expected (I), Result (I)),
           "Unexpected element " & Expected (I)'Image & " instead of " & Result (I)'Image &
           " at " & I'Image);
      end loop;
   end Assert_Equivalent;

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Multiplication (Object : in out Test) is
      Half_Angle : constant Element_Type := 45.0;
   begin
      declare
         Axis  : constant Vector4 := Vectors.Normalize ([1.0, 2.0, 3.0, 0.0]);

         Sin_Value : constant Element_Type := EF.Sin (Half_Angle, 360.0);
         Cos_Value : constant Element_Type := EF.Cos (Half_Angle, 360.0);

         Result   : constant Quaternion :=
           R (Axis, To_Radians (Half_Angle)) * R (Axis, To_Radians (Half_Angle));
         Expected : constant Quaternion := [Axis (X) * Sin_Value,
                                            Axis (Y) * Sin_Value,
                                            Axis (Z) * Sin_Value,
                                            Cos_Value];
      begin
         Assert_Equivalent (Expected, Result);
         Assert (Normalized (Result), "Result not normalized");
      end;

      declare
         Axis  : constant Vector4 := [1.0, 0.0, 0.0, 0.0];
         Angle_Radians : constant Element_Type := To_Radians (30.0);

         Rotation : constant Quaternion :=
           R (Axis, Angle_Radians) * R (Axis, Angle_Radians) * R (Axis, Angle_Radians);

         Expected : constant Vector4 := [0.0, 0.0, 1.0, 0.0];
         Result   : constant Vector4 := Rotate ([0.0, 1.0, 0.0, 0.0], Rotation);
      begin
         Assert_Equivalent (Expected, Result);
      end;
   end Test_Multiplication;

   procedure Test_Conjugate (Object : in out Test) is
      Elements : constant Quaternion := [1.0, 2.0, 3.0, 4.0];

      Expected : constant Quaternion := [-1.0, -2.0, -3.0, 4.0];
      Result   : constant Quaternion := Conjugate (Elements);
   begin
      Assert_Equivalent (Expected, Result);
   end Test_Conjugate;

   procedure Test_Norm (Object : in out Test) is
      Elements_1 : constant Quaternion := [1.0, 2.0, 3.0, 4.0];
      Elements_2 : constant Quaternion := [-1.0, 2.0, -3.0, 4.0];
      Elements_3 : constant Quaternion := [0.0, 0.0, 0.0, 0.0];

      Result_1   : constant Element_Type := Norm (Elements_1);
      Result_2   : constant Element_Type := Norm (Elements_2);
      Result_3   : constant Element_Type := Norm (Elements_3);

      Expected_1 : constant Element_Type := EF.Sqrt (30.0);
      Expected_2 : constant Element_Type := EF.Sqrt (30.0);
      Expected_3 : constant Element_Type := 0.0;
   begin
      Assert (Is_Equivalent (Expected_1, Result_1), "Unexpected Single " & Result_1'Image);
      Assert (Is_Equivalent (Expected_2, Result_2), "Unexpected Single " & Result_2'Image);
      Assert (Is_Equivalent (Expected_3, Result_3), "Unexpected Single " & Result_3'Image);
   end Test_Norm;

   procedure Test_Normalize (Object : in out Test) is
      Elements : constant Quaternion := [1.0, 2.0, 3.0, 4.0];
   begin
      Assert (not Normalized (Elements), "Elements is unexpectedly a unit quaternion");
      declare
         Result : constant Quaternion := Normalize (Elements);
      begin
         Assert (Normalized (Result), "Result not normalized");
      end;
   end Test_Normalize;

   procedure Test_Normalized (Object : in out Test) is
      Elements_1 : constant Quaternion := [0.0, 0.0, 0.0, 1.0];
      Elements_2 : constant Quaternion := [0.5, 0.5, 0.5, -0.5];
      Elements_3 : constant Quaternion := [0.0, -1.0, 0.0, 0.0];

      Elements_4 : constant Quaternion := [1.0, 2.0, 3.0, 4.0];
   begin
      Assert (Normalized (Elements_1), "Elements_1 not normalized");
      Assert (Normalized (Elements_2), "Elements_2 not normalized");
      Assert (Normalized (Elements_3), "Elements_3 not normalized");

      Assert (not Normalized (Elements_4), "Elements_4 normalized");
   end Test_Normalized;

   procedure Test_Rotate_Axis_Angle (Object : in out Test) is
      Axis  : constant Vector4 := Vectors.Normalize ([1.0, 2.0, 3.0, 0.0]);
      Angle : constant Element_Type := 90.0;

      Sin_Value : constant Element_Type := EF.Sin (45.0, 360.0);
      Cos_Value : constant Element_Type := EF.Cos (45.0, 360.0);

      Result   : constant Quaternion := R (Axis, To_Radians (Angle));
      Expected : constant Quaternion := [Axis (X) * Sin_Value,
                                         Axis (Y) * Sin_Value,
                                         Axis (Z) * Sin_Value,
                                         Cos_Value];
   begin
      Assert_Equivalent (Expected, Result);
      Assert (Normalized (Result), "Result not normalized");
   end Test_Rotate_Axis_Angle;

   procedure Test_Axis_Angle (Object : in out Test) is
      Axis  : constant Vector4 := Vectors.Normalize ([1.0, 2.0, 3.0, 0.0]);
      Angle : constant Element_Type := 90.0;

      Expected : constant Axis_Angle := (Axis  => Vectors.Direction (Axis),
                                         Angle => To_Radians (Angle));
      Actual : constant Axis_Angle := To_Axis_Angle (From_Axis_Angle (Expected));
   begin
      Assert_Equivalent (Vector4 (Expected.Axis), Vector4 (Actual.Axis));
      Assert (Is_Equivalent (Expected.Angle, Actual.Angle),
        "Unexpected angle " & Actual.Angle'Image);
   end Test_Axis_Angle;

   procedure Test_Axis_Angle_No_Rotation (Object : in out Test) is
      Actual   : constant Element_Type := To_Axis_Angle (Identity).Angle;
      Expected : constant Element_Type := 0.0;
   begin
      Assert (Is_Equivalent (Expected, Actual), "Unexpected angle " & Actual'Image);
   end Test_Axis_Angle_No_Rotation;

   procedure Test_Rotate_Vectors (Object : in out Test) is
      Start_Vector : constant Vector4 := [0.0, 1.0, 0.0, 0.0];
      End_Vector   : constant Vector4 := [0.0, 0.0, 1.0, 0.0];

      Axis  : constant Vector4 := [1.0, 0.0, 0.0, 0.0];
      Angle : constant Element_Type := 90.0;

      Expected_1 : constant Quaternion := R (Axis, To_Radians (Angle));
      Result_1   : constant Quaternion := R (Start_Vector, End_Vector);

      Expected_2 : constant Quaternion := R (Axis, To_Radians (-Angle));
      Result_2   : constant Quaternion := R (End_Vector, Start_Vector);
   begin
      Assert_Equivalent (Expected_1, Result_1);
      Assert_Equivalent (Expected_2, Result_2);
   end Test_Rotate_Vectors;

   procedure Test_Rotate (Object : in out Test) is
      Axis  : constant Vector4 := [1.0, 0.0, 0.0, 0.0];
      Angle : constant Element_Type := 90.0;

      Rotation : constant Quaternion := R (Axis, To_Radians (Angle));

      Expected : constant Vector4 := [0.0, 0.0, 1.0, 0.0];
      Result   : constant Vector4 := Rotate ([0.0, 1.0, 0.0, 0.0], Rotation);
   begin
      Assert_Equivalent (Expected, Result);
   end Test_Rotate;

   procedure Test_Difference (Object : in out Test) is
      Axis    : constant Vector4 := [1.0, 0.0, 0.0, 0.0];
      Angle_A : constant Element_Type := 30.0;
      Angle_B : constant Element_Type := 90.0;

      Rotation_A : constant Quaternion := R (Axis, To_Radians (Angle_A));
      Rotation_B : constant Quaternion := R (Axis, To_Radians (Angle_B));

      Actual   : constant Axis_Angle := To_Axis_Angle (Difference (Rotation_A, Rotation_B));
      Expected : constant Axis_Angle := (Axis  => Vectors.Direction (Axis),
                                         Angle => To_Radians (60.0));
   begin
      Assert_Equivalent (Vector4 (Expected.Axis), Vector4 (Actual.Axis));
      Assert (Is_Equivalent (Expected.Angle, Actual.Angle),
        "Unexpected angle " & Actual.Angle'Image);
   end Test_Difference;

   procedure Test_Slerp (Object : in out Test) is
      Axis  : constant Vector4 := [1.0, 0.0, 0.0, 0.0];
      Angle : constant Element_Type := 45.0;

      Start_Quaternion : constant Quaternion := R (Axis, To_Radians (0.0));
      End_Quaternion   : constant Quaternion := R (Axis, To_Radians (90.0));

      Expected : constant Quaternion := R (Axis, To_Radians (Angle));
      Result   : constant Quaternion := Slerp (Start_Quaternion, End_Quaternion, 0.5);
   begin
      Assert_Equivalent (Expected, Result);
   end Test_Slerp;

   ----------------------------------------------------------------------------

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(Transforms - " & Suite_Name & " - Quaternions) ";
   begin
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '*' operator", Test_Multiplication'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Conjugate function", Test_Conjugate'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Norm function", Test_Norm'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Normalize function", Test_Normalize'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Normalized function", Test_Normalized'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test to/from Axis_Angle functions", Test_Axis_Angle'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test To_Axis_Angle function with Identity", Test_Axis_Angle_No_Rotation'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Rotate_Axis_Angle function", Test_Rotate_Axis_Angle'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Rotate_Vectors function", Test_Rotate_Vectors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Rotate function", Test_Rotate'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Difference function", Test_Difference'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Slerp function", Test_Slerp'Access));

      return Test_Suite'Access;
   end Suite;

end Generic_Test_Transforms_Quaternions;
