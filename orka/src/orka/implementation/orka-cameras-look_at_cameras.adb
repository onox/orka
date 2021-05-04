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

with Orka.Transforms.Doubles.Matrix_Conversions;

package body Orka.Cameras.Look_At_Cameras is

   overriding
   procedure Look_At
     (Object : in out Look_At_Camera;
      Target : Behaviors.Behavior_Ptr) is
   begin
      Object.Target := Target;
   end Look_At;

   --  Look_At camera does not need to implement Update because the
   --  view matrix does not depend on the pointer (it is computed using
   --  the camera's and target's positions)

   use Orka.Transforms.Doubles.Matrix_Conversions;

   overriding
   function View_Matrix (Object : Look_At_Camera) return Transforms.Matrix4 is
     (Convert (Look_At (Object.Target.Position, Object.Position, Object.Up)));

   overriding
   function View_Matrix_Inverse (Object : Look_At_Camera) return Transforms.Matrix4 is
      use Transforms;
   begin
      return Transpose (Object.View_Matrix);
   end View_Matrix_Inverse;

   overriding
   function Target_Position (Object : Look_At_Camera) return Vector4 is
     (Object.Target.Position);

   overriding
   function Create_Camera
     (Input : Inputs.Pointers.Pointer_Input_Ptr;
      Lens  : Lens_Ptr) return Look_At_Camera is
   begin
      return Look_At_Camera'(Camera with
        Input  => Input,
        Lens   => Lens,
        others => <>);
   end Create_Camera;

end Orka.Cameras.Look_At_Cameras;
