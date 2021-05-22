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

package Orka.Cameras.Look_From_Cameras is
   pragma Preelaborate;

   type Look_From_Camera is new First_Person_Camera with private;

   procedure Set_Orientation
     (Object : in out Look_From_Camera;
      Roll, Pitch, Yaw : Angle);

   overriding
   function View_Matrix (Object : Look_From_Camera) return Transforms.Matrix4;

   overriding
   function View_Matrix_Inverse (Object : Look_From_Camera) return Transforms.Matrix4;

   overriding
   procedure Update (Object : in out Look_From_Camera; Delta_Time : Duration);

   overriding
   function Create_Camera (Lens : Lens_Ptr) return Look_From_Camera;

private

   type Look_From_Camera is new First_Person_Camera with record
      Roll, Pitch, Yaw : Angle := 0.0;
   end record;

end Orka.Cameras.Look_From_Cameras;
