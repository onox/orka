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

with Ada.Tags.Generic_Dispatching_Constructor;

package body Orka.Cameras is

   procedure Set_FOV (Object : in out Camera_Lens; FOV : GL.Types.Single) is
   begin
      Object.FOV := FOV;
   end Set_FOV;

   function FOV (Object : Camera_Lens) return GL.Types.Single is
     (Object.FOV);

   function Projection_Matrix (Object : Camera_Lens) return Transforms.Matrix4 is
      Width  : constant GL.Types.Single := GL.Types.Single (Object.Width);
      Height : constant GL.Types.Single := GL.Types.Single (Object.Height);
   begin
      return Transforms.Infinite_Perspective (Object.FOV, Width / Height, 0.1);
   end Projection_Matrix;

   -----------------------------------------------------------------------------

   procedure Set_Position
     (Object   : in out First_Person_Camera;
      Position : Transforms.Vector4) is
   begin
      Object.Position := Position;
   end Set_Position;

   procedure Set_Orientation
     (Object : in out Look_From_Camera;
      Roll, Pitch, Yaw : Angle) is
   begin
      Object.Roll := Roll;
      Object.Pitch := Pitch;
      Object.Yaw := Yaw;
   end Set_Orientation;

   overriding
   procedure Look_At
     (Object : in out Look_At_Camera;
      Target : Transforms.Vector4) is
   begin
      Object.Target := Target;
   end Look_At;

   overriding
   procedure Look_At
     (Object : in out Third_Person_Camera;
      Target : Transforms.Vector4) is
   begin
      Object.Target := Target;
   end Look_At;

   procedure Set_Angles
     (Object : in out Rotate_Around_Camera;
      Alpha  : Angle;
      Beta   : Angle) is
   begin
      Object.Alpha := Alpha;
      Object.Beta := Beta;
   end Set_Angles;

   procedure Set_Radius
     (Object : in out Rotate_Around_Camera;
      Radius : Distance) is
   begin
      Object.Radius := Radius;
   end Set_Radius;

   procedure Set_Radius (Object : in out Follow_Camera; Radius : Distance) is
   begin
      Object.Radius := Radius;
   end Set_Radius;

   procedure Set_Height (Object : in out Follow_Camera; Height : Distance) is
   begin
      Object.Height := Height;
   end Set_Height;

   procedure Set_Direction
     (Object    : in out Follow_Camera;
      Direction : Angle) is
   begin
      Object.Direction := Direction;
   end Set_Direction;

   -----------------------------------------------------------------------------

   overriding
   procedure Update (Object : in out Look_From_Camera; Delta_Time : Duration) is
      Using_Camera : constant Boolean := Object.Input.Button_Pressed (Orka.Inputs.Right);
   begin
      Object.Input.Lock_Pointer (Using_Camera);

      if Using_Camera then
         Object.Yaw   := Object.Yaw   + Object.Input.Delta_X;
         Object.Pitch := Object.Pitch + Object.Input.Delta_Y;
      end if;
   end Update;

   overriding
   procedure Update (Object : in out Look_At_Camera; Delta_Time : Duration) is
      Using_Camera : constant Boolean := Object.Input.Button_Pressed (Orka.Inputs.Right);
   begin
      Object.Input.Lock_Pointer (Using_Camera);

      --  TODO
   end Update;

   overriding
   procedure Update (Object : in out Rotate_Around_Camera; Delta_Time : Duration) is
      Using_Camera : constant Boolean := Object.Input.Button_Pressed (Orka.Inputs.Right);
   begin
      Object.Input.Lock_Pointer (Using_Camera);

      if Using_Camera then
         Object.Alpha := Object.Alpha + Object.Input.Delta_X;
         Object.Beta  := Object.Beta  + Object.Input.Delta_Y;
      end if;

      Object.Radius := Object.Radius - Object.Input.Scroll_Y;
   end Update;

   overriding
   procedure Update (Object : in out Follow_Camera; Delta_Time : Duration) is
      Using_Camera : constant Boolean := Object.Input.Button_Pressed (Orka.Inputs.Right);
   begin
      Object.Input.Lock_Pointer (Using_Camera);

      --  TODO
   end Update;

   overriding
   procedure After_Update (Object : in out Camera; Delta_Time : Duration) is
   begin
      --  TODO Call Object.View_Matrix and set some uniform with the value
      null;
   end After_Update;

   -----------------------------------------------------------------------------

   use Transforms;

   overriding
   function View_Matrix (Object : Look_From_Camera) return Transforms.Matrix4 is
   begin
      return Ry (Object.Roll) * Rx (Object.Pitch) * Rz (Object.Yaw) * T (Object.Position);
   end View_Matrix;

   overriding
   function View_Matrix (Object : Look_At_Camera) return Transforms.Matrix4 is
   begin
      --  TODO
      return Transforms.Identity_Value;
   end View_Matrix;

   overriding
   function View_Matrix (Object : Rotate_Around_Camera) return Transforms.Matrix4 is
   begin
      return (0.0, 0.0, -Object.Radius, 0.0) + Rx (Object.Beta) * Rz (Object.Alpha) * T (Object.Target);
   end View_Matrix;

   overriding
   function View_Matrix (Object : Follow_Camera) return Transforms.Matrix4 is
   begin
      --  TODO
      return Transforms.Identity_Value;
   end View_Matrix;

   -----------------------------------------------------------------------------

   function Create_Lens
     (Width, Height : Positive;
      FOV : GL.Types.Single) return Camera_Lens'Class is
   begin
      return Result : Camera_Lens (Width, Height) do
         Result.FOV := FOV;
      end return;
   end Create_Lens;

   -----------------------------------------------------------------------------

   Kind_To_Tag : constant array (Camera_Kind) of Ada.Tags.Tag :=
     (Look_From     => Look_From_Camera'Tag,
      Look_At       => Look_At_Camera'Tag,
      Rotate_Around => Rotate_Around_Camera'Tag,
      Follow        => Follow_Camera'Tag);

   overriding
   function Create_Camera
     (Parameters : not null access Parameter_Record) return Look_From_Camera is
   begin
      return Look_From_Camera'(Camera with
        Input => Parameters.Input,
        Lens  => Parameters.Lens, others => <>);
   end Create_Camera;

   overriding
   function Create_Camera
     (Parameters : not null access Parameter_Record) return Look_At_Camera is
   begin
      return Look_At_Camera'(Camera with
        Input => Parameters.Input,
        Lens  => Parameters.Lens, others => <>);
   end Create_Camera;

   overriding
   function Create_Camera
     (Parameters : not null access Parameter_Record) return Rotate_Around_Camera is
   begin
      return Rotate_Around_Camera'(Camera with
        Input => Parameters.Input,
        Lens  => Parameters.Lens, others => <>);
   end Create_Camera;

   overriding
   function Create_Camera
     (Parameters : not null access Parameter_Record) return Follow_Camera is
   begin
      return Follow_Camera'(Camera with
        Input => Parameters.Input,
        Lens  => Parameters.Lens, others => <>);
   end Create_Camera;

   function Create_Camera
     (Kind  : Camera_Kind;
      Input : Inputs.Pointer_Input_Ptr;
      Lens  : Lens_Ptr) return Camera'Class
   is
      function Create is new Ada.Tags.Generic_Dispatching_Constructor
        (Camera, Parameter_Record, Create_Camera);

      Parameters : aliased Parameter_Record := (Input, Lens);
   begin
      return Create (Kind_To_Tag (Kind), Parameters'Access);
   end Create_Camera;

end Orka.Cameras;
