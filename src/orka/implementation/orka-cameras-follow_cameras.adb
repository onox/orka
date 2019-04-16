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

package body Orka.Cameras.Follow_Cameras is

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

   overriding
   procedure Update (Object : in out Follow_Camera; Delta_Time : Duration) is
      Using_Camera : constant Boolean := Object.Input.Button_Pressed (Inputs.Pointers.Right);
   begin
      Object.Input.Lock_Pointer (Using_Camera);

      --  TODO
   end Update;

   overriding
   function View_Matrix (Object : Follow_Camera) return Transforms.Matrix4 is
   begin
      --  TODO
      return Transforms.Identity_Value;
   end View_Matrix;

   overriding
   function Create_Camera
     (Input : Inputs.Pointers.Pointer_Input_Ptr;
      Lens  : Lens_Ptr;
      FB    : Rendering.Framebuffers.Framebuffer_Ptr) return Follow_Camera is
   begin
      return Follow_Camera'(Camera with
        Input  => Input,
        Lens   => Lens,
        FB     => FB,
        others => <>);
   end Create_Camera;

end Orka.Cameras.Follow_Cameras;
