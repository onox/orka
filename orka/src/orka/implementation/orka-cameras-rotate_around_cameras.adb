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

with Orka.Transforms.Doubles.Matrices;
with Orka.Transforms.Doubles.Matrix_Conversions;

package body Orka.Cameras.Rotate_Around_Cameras is

   procedure Set_Angles
     (Object : in out Rotate_Around_Camera;
      Alpha  : Angle;
      Beta   : Angle) is
   begin
      Object.Alpha := Alpha;
      Object.Beta  := Beta;
   end Set_Angles;

   procedure Set_Radius
     (Object : in out Rotate_Around_Camera;
      Radius : Distance) is
   begin
      Object.Radius := Radius;
   end Set_Radius;

   overriding
   procedure Update (Object : in out Rotate_Around_Camera; Delta_Time : Duration) is
      use all type Inputs.Pointers.Button_State;
      use all type Inputs.Pointers.Dimension;
      use all type Inputs.Pointers.Pointer_Mode;

      Pointer_State : constant Inputs.Pointers.Pointer_State := Object.Input.State;
      Using_Camera  : constant Boolean := Pointer_State.Buttons (Inputs.Pointers.Right) = Pressed;

      Relative      : Inputs.Pointers.Coordinate renames Pointer_State.Relative;
      Scroll        : Inputs.Pointers.Coordinate renames Pointer_State.Scroll;
   begin
      Object.Input.Set_Mode (if Using_Camera then Locked else Visible);

      if Using_Camera then
         Object.Alpha := Normalize_Angle (Object.Alpha + Relative (X) * Object.Scale (X));
         Object.Beta  := Normalize_Angle (Object.Beta  + Relative (Y) * Object.Scale (Y));
      end if;

      Object.Radius := Clamp_Distance (Object.Radius - Scroll (Y) * Object.Scale (Z));
   end Update;

   use Orka.Transforms.Doubles.Matrices;
   use Orka.Transforms.Doubles.Matrix_Conversions;

   overriding
   function View_Matrix (Object : Rotate_Around_Camera) return Transforms.Matrix4 is
     (Convert (Rx (Object.Beta) * Ry (Object.Alpha) * Object.Rotate_To_Up));

   function View_Matrix_Inverse (Object : Rotate_Around_Camera) return Matrix4 is
     (Transpose (Object.Rotate_To_Up) * Ry (-Object.Alpha) * Rx (-Object.Beta))
   with Inline;

   overriding
   function View_Matrix_Inverse (Object : Rotate_Around_Camera) return Transforms.Matrix4 is
     (Convert (Object.View_Matrix_Inverse));

   overriding
   function View_Position (Object : Rotate_Around_Camera) return Vector4 is
      View_Matrix : constant Matrix4 :=
         Object.Target_Position + Object.View_Matrix_Inverse * T ((0.0, 0.0, Object.Radius, 0.0));
   begin
      return View_Matrix (W);
   end View_Position;

   overriding
   function Create_Camera (Lens : Lens_Ptr) return Rotate_Around_Camera is
   begin
      return Rotate_Around_Camera'(Camera with
        Lens   => Lens,
        others => <>);
   end Create_Camera;

end Orka.Cameras.Rotate_Around_Cameras;
