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

package Orka.Cameras.Follow_Cameras is
   pragma Preelaborate;

   type Follow_Camera is new Third_Person_Camera with private;

   procedure Set_Radius (Object : in out Follow_Camera; Radius : Distance);

   procedure Set_Height (Object : in out Follow_Camera; Height : Distance);

   procedure Set_Direction
     (Object    : in out Follow_Camera;
      Direction : Angle);

   overriding
   function View_Matrix (Object : Follow_Camera) return Transforms.Matrix4;

   overriding
   procedure Update (Object : in out Follow_Camera; Delta_Time : Duration);

   overriding
   function Create_Camera
     (Input : Inputs.Pointers.Pointer_Input_Ptr;
      Lens  : Lens_Ptr;
      FB    : Rendering.Framebuffers.Framebuffer_Ptr) return Follow_Camera;

private

   type Follow_Camera is new Third_Person_Camera with record
      Height, Radius : Distance := 1.0;
      Direction      : Angle := 0.0;
   end record;

end Orka.Cameras.Follow_Cameras;
