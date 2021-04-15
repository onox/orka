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

with Orka.Transforms.Singles.Quaternions;
with Orka.Transforms.Singles.Vectors;

package body Test_Transforms_Singles_Quaternions is

   use Orka;
   use Orka.Transforms.Singles.Quaternions;

   use AUnit.Assertions;

   package Vectors renames Orka.Transforms.Singles.Vectors;

   use type Vector4;

   package EF is new Ada.Numerics.Generic_Elementary_Functions (Float_32);

   function To_Radians (Angle : Float_32) return Float_32 renames Vectors.To_Radians;

   function Is_Equivalent (Expected, Result : Float_32) return Boolean is
     (abs (Result - Expected) <= Float_32'Model_Epsilon);

   procedure Assert_Equivalent (Expected, Result : Vector4) is
   begin
      for I in Index_Homogeneous loop
         Assert (Is_Equivalent (Expected (I), Result (I)),
           "Unexpected element " & Expected (I)'Image & " instead of " & Result (I)'Image &
           " at " & I'Image);
      end loop;
   end Assert_Equivalent;

   procedure Assert_Equivalent (Expected, Result : Quaternion) is
   begin
      for I in Index_Homogeneous loop
         Assert (Is_Equivalent (Expected (I), Result (I)),
           "Unexpected element " & Expected (I)'Image & " instead of " & Result (I)'Image &
           " at " & I'Image);
      end loop;
   end Assert_Equivalent;

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(Transforms - Singles - Quaternions) ";
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
        (Name & "Test Rotate_Axis_Angle function", Test_Rotate_Axis_Angle'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Rotate_Vectors function", Test_Rotate_Vectors'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Rotate_At_Origin procedure", Test_Rotate_At_Origin'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Slerp function", Test_Slerp'Access));

      return Test_Suite'Access;
   end Suite;

   procedure Test_Multiplication (Object : in out Test) is
      Half_Angle : constant Float_32 := 45.0;
   begin
      declare
         Axis  : constant Vector4 := Vectors.Normalize ((1.0, 2.0, 3.0, 0.0));

         Sin_Value : constant Float_32 := EF.Sin (Half_Angle, 360.0);
         Cos_Value : constant Float_32 := EF.Cos (Half_Angle, 360.0);

         Result   : constant Quaternion :=
           R (Axis, To_Radians (Half_Angle)) * R (Axis, To_Radians (Half_Angle));
         Expected : constant Quaternion := (Axis (X) * Sin_Value,
                                            Axis (Y) * Sin_Value,
                                            Axis (Z) * Sin_Value,
                                            Cos_Value);
      begin
         Assert_Equivalent (Expected, Result);
         Assert (Normalized (Result), "Result not normalized");
      end;

      declare
         Axis  : constant Vector4 := (1.0, 0.0, 0.0, 0.0);
         Angle_Radians : constant Float_32 := To_Radians (30.0);

         Rotation : constant Quaternion :=
           R (Axis, Angle_Radians) * R (Axis, Angle_Radians) * R (Axis, Angle_Radians);

         Expected : constant Vector4 := (0.0, 0.0, 1.0, 0.0);
         Result   : Vector4 := (0.0, 1.0, 0.0, 0.0);
      begin
         Rotate_At_Origin (Result, Rotation);
         Assert_Equivalent (Expected, Result);
      end;
   end Test_Multiplication;

   procedure Test_Conjugate (Object : in out Test) is
      Elements : constant Quaternion := (1.0, 2.0, 3.0, 4.0);

      Expected : constant Quaternion := (-1.0, -2.0, -3.0, 4.0);
      Result   : constant Quaternion := Conjugate (Elements);
   begin
      Assert_Equivalent (Expected, Result);
   end Test_Conjugate;

   procedure Test_Norm (Object : in out Test) is
      Elements_1 : constant Quaternion := (1.0, 2.0, 3.0, 4.0);
      Elements_2 : constant Quaternion := (-1.0, 2.0, -3.0, 4.0);
      Elements_3 : constant Quaternion := (0.0, 0.0, 0.0, 0.0);

      Result_1   : constant Float_32 := Norm (Elements_1);
      Result_2   : constant Float_32 := Norm (Elements_2);
      Result_3   : constant Float_32 := Norm (Elements_3);

      Expected_1 : constant Float_32 := EF.Sqrt (30.0);
      Expected_2 : constant Float_32 := EF.Sqrt (30.0);
      Expected_3 : constant Float_32 := 0.0;
   begin
      Assert (Is_Equivalent (Expected_1, Result_1), "Unexpected Single " & Result_1'Image);
      Assert (Is_Equivalent (Expected_2, Result_2), "Unexpected Single " & Result_2'Image);
      Assert (Is_Equivalent (Expected_3, Result_3), "Unexpected Single " & Result_3'Image);
   end Test_Norm;

   procedure Test_Normalize (Object : in out Test) is
      Elements : constant Quaternion := (1.0, 2.0, 3.0, 4.0);
   begin
      Assert (not Normalized (Elements), "Elements is unexpectedly a unit quaternion");
      declare
         Result : constant Quaternion := Normalize (Elements);
      begin
         Assert (Normalized (Result), "Result not normalized");
      end;
   end Test_Normalize;

   procedure Test_Normalized (Object : in out Test) is
      Elements_1 : constant Quaternion := (0.0, 0.0, 0.0, 1.0);
      Elements_2 : constant Quaternion := (0.5, 0.5, 0.5, -0.5);
      Elements_3 : constant Quaternion := (0.0, -1.0, 0.0, 0.0);

      Elements_4 : constant Quaternion := (1.0, 2.0, 3.0, 4.0);
   begin
      Assert (Normalized (Elements_1), "Elements_1 not normalized");
      Assert (Normalized (Elements_2), "Elements_2 not normalized");
      Assert (Normalized (Elements_3), "Elements_3 not normalized");

      Assert (not Normalized (Elements_4), "Elements_4 normalized");
   end Test_Normalized;

   procedure Test_Rotate_Axis_Angle (Object : in out Test) is
      Axis  : constant Vector4 := Vectors.Normalize ((1.0, 2.0, 3.0, 0.0));
      Angle : constant Float_32 := 90.0;

      Sin_Value : constant Float_32 := EF.Sin (45.0, 360.0);
      Cos_Value : constant Float_32 := EF.Cos (45.0, 360.0);

      Result   : constant Quaternion := R (Axis, To_Radians (Angle));
      Expected : constant Quaternion := (Axis (X) * Sin_Value,
                                         Axis (Y) * Sin_Value,
                                         Axis (Z) * Sin_Value,
                                         Cos_Value);
   begin
      Assert_Equivalent (Expected, Result);
      Assert (Normalized (Result), "Result not normalized");
   end Test_Rotate_Axis_Angle;

   procedure Test_Rotate_Vectors (Object : in out Test) is
      Start_Vector : constant Vector4 := (0.0, 1.0, 0.0, 0.0);
      End_Vector   : constant Vector4 := (0.0, 0.0, 1.0, 0.0);

      Axis  : constant Vector4 := (1.0, 0.0, 0.0, 0.0);
      Angle : constant Float_32 := 90.0;

      Expected_1 : constant Quaternion := R (Axis, To_Radians (Angle));
      Result_1   : constant Quaternion := R (Start_Vector, End_Vector);

      Expected_2 : constant Quaternion := R (Axis, To_Radians (-Angle));
      Result_2   : constant Quaternion := R (End_Vector, Start_Vector);
   begin
      Assert_Equivalent (Expected_1, Result_1);
      Assert_Equivalent (Expected_2, Result_2);
   end Test_Rotate_Vectors;

   procedure Test_Rotate_At_Origin (Object : in out Test) is
      Axis  : constant Vector4 := (1.0, 0.0, 0.0, 0.0);
      Angle : constant Float_32 := 90.0;

      Rotation : constant Quaternion := R (Axis, To_Radians (Angle));

      Expected : constant Vector4 := (0.0, 0.0, 1.0, 0.0);
      Result   : Vector4 := (0.0, 1.0, 0.0, 0.0);
   begin
      Rotate_At_Origin (Result, Rotation);
      Assert_Equivalent (Expected, Result);
   end Test_Rotate_At_Origin;

   procedure Test_Slerp (Object : in out Test) is
      Axis  : constant Vector4 := (1.0, 0.0, 0.0, 0.0);
      Angle : constant Float_32 := 45.0;

      Start_Quaternion : constant Quaternion := R (Axis, To_Radians (0.0));
      End_Quaternion   : constant Quaternion := R (Axis, To_Radians (90.0));

      Expected : constant Quaternion := R (Axis, To_Radians (Angle));
      Result   : constant Quaternion := Slerp (Start_Quaternion, End_Quaternion, 0.5);
   begin
      Assert_Equivalent (Expected, Result);
   end Test_Slerp;

end Test_Transforms_Singles_Quaternions;
