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

package body Orka.Cameras.Look_From_Cameras is

   procedure Set_Orientation
     (Object : in out Look_From_Camera;
      Roll, Pitch, Yaw : Angle) is
   begin
      Object.Roll  := Roll;
      Object.Pitch := Pitch;
      Object.Yaw   := Yaw;
   end Set_Orientation;

   procedure Change_Orientation
     (Object : in out Look_From_Camera;
      Value  : Vector4) is
   begin
      Object.Updater.Set (Value);
   end Change_Orientation;

   overriding
   procedure Update (Object : in out Look_From_Camera; Delta_Time : Duration) is
      Change : Vector4;
      Mode   : Update_Mode;
   begin
      Object.Updater.Get (Change, Mode);

      Object.Yaw   := Normalize_Angle (Object.Yaw   + Change (X) * Object.Scale (X));
      Object.Pitch := Normalize_Angle (Object.Pitch + Change (Y) * Object.Scale (Y));
   end Update;

   use Orka.Transforms.Doubles.Matrices;
   use Orka.Transforms.Doubles.Matrix_Conversions;

   overriding
   function View_Matrix (Object : Look_From_Camera) return Transforms.Matrix4 is
     (Convert (Ry (Object.Roll) * Rx (Object.Pitch) * Ry (Object.Yaw) * Object.Rotate_To_Up));

   overriding
   function View_Matrix_Inverse (Object : Look_From_Camera) return Transforms.Matrix4 is
     (Convert (Transpose (Object.Rotate_To_Up) *
        Ry (-Object.Yaw) * Rx (-Object.Pitch) * Ry (-Object.Roll)));

   overriding
   function Create_Camera (Lens : Camera_Lens) return Look_From_Camera is
   begin
      return (Lens => Lens, others => <>);
   end Create_Camera;

end Orka.Cameras.Look_From_Cameras;
