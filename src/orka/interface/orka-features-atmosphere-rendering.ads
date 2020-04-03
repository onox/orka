--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2020 onox <denkpadje@gmail.com>
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

with Orka.Behaviors;
with Orka.Cameras;
with Orka.Rendering.Buffers;
with Orka.Resources.Locations;

private with GL.Low_Level.Enums;

private with Orka.Rendering.Programs.Uniforms;

package Orka.Features.Atmosphere.Rendering is
   pragma Preelaborate;

   type Exposure_Type is digits 6 range 0.0 .. 50.0;

   type Atmosphere is tagged private;

   function Create_Atmosphere
     (Data     : not null access constant Model_Data;
      Location : Resources.Locations.Location_Ptr) return Atmosphere;

   procedure Render
     (Object : in out Atmosphere;
      Camera : Cameras.Camera_Ptr;
      Planet : Behaviors.Behavior_Ptr;
      Sun    : Behaviors.Behavior_Ptr;
      Exposure : Exposure_Type := 1.0;
      Offset   : Cameras.Transforms.Vector4 := Cameras.Transforms.Vectors.Zero_Point);
   --  Render the atmosphere on the far plane

private

   package LE renames GL.Low_Level.Enums;
   package Programs renames Orka.Rendering.Programs;

   type Atmosphere is tagged record
      Program : Programs.Program;

      Distance_Scale : GL.Types.Double;
      Exposure_Scale : Exposure_Type;

      Uniform_Camera_Offset   : Programs.Uniforms.Uniform (LE.Single_Vec4);
      Uniform_Camera_Position : Programs.Uniforms.Uniform (LE.Single_Vec4);
      Uniform_Planet_Position : Programs.Uniforms.Uniform (LE.Single_Vec4);

      Uniform_Sun_Direction : Programs.Uniforms.Uniform (LE.Single_Vec4);
      Uniform_Exposure      : Programs.Uniforms.Uniform (LE.Single_Type);

      Uniform_View : Programs.Uniforms.Uniform (LE.Single_Matrix4);
      Uniform_Proj : Programs.Uniforms.Uniform (LE.Single_Matrix4);
   end record;

end Orka.Features.Atmosphere.Rendering;
