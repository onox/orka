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

with Orka.SIMD;
with Orka.Transforms.Singles.Vectors;

package body Orka.Cameras.Look_At_Cameras is

   package Vector_Transforms renames Orka.Transforms.Singles.Vectors;

   overriding
   procedure Look_At
     (Object : in out Look_At_Camera;
      Target : Behaviors.Behavior_Ptr) is
   begin
      Object.Target := Target;
   end Look_At;

   procedure Set_Up_Direction
     (Object    : in out Look_At_Camera;
      Direction : Transforms.Vector4) is
   begin
      Object.Up := Direction;
   end Set_Up_Direction;

   --  Look_At camera does not need to implement Update because the
   --  view matrix does not depend on the pointer (it is computed using
   --  the camera's and target's positions)

   overriding
   function View_Matrix (Object : Look_At_Camera) return Transforms.Matrix4 is
      use Vector_Transforms;
      use Transforms;
      use Orka.SIMD;

      Rotate_To_GL : constant Matrix4 := Ry (-90.0) * Rx (-90.0);

      Forward : constant Vector_Type
        := Normalize (Rotate_To_GL * (Object.Target.Position - Object.Position));
      Side    : constant Vector_Type := Cross (Forward, Object.Up);
      Up      : constant Vector_Type := Cross (Side, Forward);
   begin
      return
        ((Side (X), Up (X), -Forward (X), 0.0),
         (Side (Y), Up (Y), -Forward (Y), 0.0),
         (Side (Z), Up (Z), -Forward (Z), 0.0),
         (0.0, 0.0, 0.0, 1.0));
   end View_Matrix;

   overriding
   function Create_Camera
     (Input : Inputs.Pointers.Pointer_Input_Ptr;
      Lens  : Lens_Ptr;
      FB    : Rendering.Framebuffers.Framebuffer_Ptr) return Look_At_Camera is
   begin
      return Look_At_Camera'(Camera with
        Input  => Input,
        Lens   => Lens,
        FB     => FB,
        others => <>);
   end Create_Camera;

end Orka.Cameras.Look_At_Cameras;
