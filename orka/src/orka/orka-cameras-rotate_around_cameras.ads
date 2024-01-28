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

package Orka.Cameras.Rotate_Around_Cameras is
   pragma Preelaborate;

   type Rotate_Around_Camera is new Third_Person_Camera with private;

   procedure Set_Angles
     (Object : in out Rotate_Around_Camera;
      Alpha  : Angle;
      Beta   : Angle);

   procedure Set_Radius
     (Object : in out Rotate_Around_Camera;
      Radius : Distance);

   procedure Set_Orientation
     (Object : in out Rotate_Around_Camera;
      Value  : Vector4);

   procedure Change_Orientation
     (Object : in out Rotate_Around_Camera;
      Value  : Vector4);

   overriding
   function View_Matrix (Object : Rotate_Around_Camera) return Transforms.Matrix4;

   overriding
   function View_Matrix_Inverse (Object : Rotate_Around_Camera) return Transforms.Matrix4;

   overriding
   function View_Position (Object : Rotate_Around_Camera) return Vector4;

   overriding
   procedure Update (Object : in out Rotate_Around_Camera; Delta_Time : Duration);

   overriding
   function Create_Camera (Lens : Camera_Lens) return Rotate_Around_Camera;

private

   type Rotate_Around_Camera is new Third_Person_Camera with record
      Alpha  : Angle := 0.0;
      Beta   : Angle := 0.0;
      Radius : Distance := 1.0;
      Updater : Change_Updater_Ptr := new Change_Updater;
   end record;

end Orka.Cameras.Rotate_Around_Cameras;
