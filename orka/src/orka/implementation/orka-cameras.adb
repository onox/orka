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

with Orka.Transforms.Doubles.Quaternions;

package body Orka.Cameras is

   function Projection_Matrix (Object : Camera_Lens) return Transforms.Matrix4 is
      Width  : constant Float_32 := Float_32 (Object.Width);
      Height : constant Float_32 := Float_32 (Object.Height);
   begin
      if Object.Reversed_Z then
         return Transforms.Infinite_Perspective_Reversed_Z (Object.FOV, Width / Height, 0.1);
      else
         return Transforms.Infinite_Perspective (Object.FOV, Width / Height, 0.1);
      end if;
   end Projection_Matrix;

   -----------------------------------------------------------------------------

   procedure Set_Input_Scale
     (Object  : in out Camera;
      X, Y, Z : Float_64) is
   begin
      Object.Scale := (X, Y, Z, 0.0);
   end Set_Input_Scale;

   procedure Set_Up_Direction
     (Object    : in out Camera;
      Direction : Vector4) is
   begin
      Object.Up := Direction;
   end Set_Up_Direction;

   function Lens (Object : Camera) return Camera_Lens is (Object.Lens);

   procedure Set_Lens (Object : in out Camera; Lens : Camera_Lens) is
   begin
      Object.Lens := Lens;
   end Set_Lens;

   procedure Set_Position
     (Object   : in out First_Person_Camera;
      Position : Vector4) is
   begin
      Object.Position := Position;
   end Set_Position;

   overriding
   procedure Look_At
     (Object : in out Third_Person_Camera;
      Target : Behaviors.Behavior_Ptr) is
   begin
      Object.Target := Target;
   end Look_At;

   -----------------------------------------------------------------------------

   overriding
   function View_Position (Object : First_Person_Camera) return Vector4 is
     (Object.Position);

   overriding
   function Target_Position (Object : Third_Person_Camera) return Vector4 is
     (Object.Target.Position);

   -----------------------------------------------------------------------------

   function Create_Lens
     (Width, Height : Positive;
      FOV           : Float_32;
      Context       : Contexts.Context'Class) return Camera_Lens is
   begin
      return
        (Width      => Width,
         Height     => Height,
         FOV        => FOV,
         Reversed_Z => Context.Enabled (Contexts.Reversed_Z));
   end Create_Lens;

   function Projection_Matrix (Object : Camera) return Transforms.Matrix4 is
     (Projection_Matrix (Object.Lens));

   function Look_At (Target, Camera, Up_World : Vector4) return Matrix4 is
      use Orka.Transforms.Doubles.Vectors;

      Forward : constant Vector4
        := Normalize ((Target - Camera));
      Side    : constant Vector4 := Normalize (Cross (Forward, Up_World));
      Up      : constant Vector4 := Cross (Side, Forward);
   begin
      return
        ((Side (X), Up (X), -Forward (X), 0.0),
         (Side (Y), Up (Y), -Forward (Y), 0.0),
         (Side (Z), Up (Z), -Forward (Z), 0.0),
         (0.0, 0.0, 0.0, 1.0));
   end Look_At;

   function Rotate_To_Up (Object : Camera'Class) return Matrix4 is
      package Quaternions renames Orka.Transforms.Doubles.Quaternions;

      use Orka.Transforms.Doubles.Matrices;
   begin
      return R (Vector4 (Quaternions.R (Y_Axis, Object.Up)));
   end Rotate_To_Up;

   protected body Change_Updater is
      procedure Add (Value : Vector4) is
         use Orka.Transforms.Doubles.Vectors;
      begin
         Change := Change + Value;
         Is_Set := True;
         Update := Relative;
      end Add;

      procedure Set (Value : Vector4) is
      begin
--         Change := Value;

         Change (X) := Value (X);
         Change (Y) := Value (Y);
         Change (Z) := Value (Z);
         Change (W) := Value (W);

         Is_Set := False;
         Update := Absolute;
      end Set;

      procedure Get (Value : in out Vector4; Mode : out Update_Mode) is
      begin
         Value (X) := Change (X);
         Value (Y) := Change (Y);
         Value (Z) := Change (Z);
         Value (W) := Change (W);

         Mode := Update;

         if Is_Set then
            Change := (0.0, 0.0, 0.0, 0.0);
         end if;
      end Get;
   end Change_Updater;

end Orka.Cameras;
