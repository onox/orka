--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2019 onox <denkpadje@gmail.com>
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

with Orka.Rendering.Buffers;
with Orka.Resources.Locations;
with Orka.Transforms.Singles.Matrices;

private with GL.Low_Level.Enums;

private with Orka.Rendering.Programs.Uniforms;

package Orka.Rendering.Debug.Coordinate_Axes is
   pragma Preelaborate;

   package Transforms renames Orka.Transforms.Singles.Matrices;

   type Coordinate_Axes is tagged private;

   function Create_Coordinate_Axes
     (Location : Resources.Locations.Location_Ptr) return Coordinate_Axes;

   procedure Render
     (Object        : in out Coordinate_Axes;
      Width, Height : Positive;
      Axis_Size     : Transforms.Vector4 := (4.0, 100.0, 16.0, 32.0);
      View, Proj    : Transforms.Matrix4;
      Transforms    : Rendering.Buffers.Bindable_Buffer'Class)
   with Pre => Transforms.Length > 0;
   --  Render three coordinates axes for each transform
   --
   --  The buffer Transforms, containing the transform matrices, must
   --  contain n matrices for n coordinate axes. This buffer controls how
   --  many coordinate axes are rendered.

private

   package LE renames GL.Low_Level.Enums;

   type Coordinate_Axes is tagged record
      Program : Rendering.Programs.Program;

      Uniform_View    : Programs.Uniforms.Uniform (LE.Single_Matrix4);
      Uniform_Proj    : Programs.Uniforms.Uniform (LE.Single_Matrix4);
      Uniform_Size    : Programs.Uniforms.Uniform (LE.UInt_Vec2);
      Uniform_Axis    : Programs.Uniforms.Uniform (LE.Single_Vec4);
   end record;

end Orka.Rendering.Debug.Coordinate_Axes;
