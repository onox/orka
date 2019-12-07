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
     (Object     : in out Coordinate_Axes;
      View, Proj : Transforms.Matrix4;
      Transforms, Sizes : Rendering.Buffers.Bindable_Buffer'Class)
   with Pre => Transforms.Length > 0 and Sizes.Length in 1 | Transforms.Length;
   --  Render three coordinates axes for each transform
   --
   --  The buffer Transforms, containing the transform matrices, must
   --  contain one or n matrices for n lines. If all lines exist
   --  in the same world space, then one matrix transform is sufficient.
   --
   --  The buffer Sizes must contain one or n singles.

private

   package LE renames GL.Low_Level.Enums;

   type Coordinate_Axes is tagged record
      Program : Rendering.Programs.Program;

      Uniform_Visible : Programs.Uniforms.Uniform (LE.Bool_Type);

      Uniform_View    : Programs.Uniforms.Uniform (LE.Single_Matrix4);
      Uniform_Proj    : Programs.Uniforms.Uniform (LE.Single_Matrix4);
   end record;

end Orka.Rendering.Debug.Coordinate_Axes;
