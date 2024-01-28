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

package Orka.Rendering.Debug.Spheres is
   pragma Preelaborate;

   package Transforms renames Orka.Transforms.Singles.Matrices;

   type Sphere is tagged private;

   function Create_Sphere
     (Location : Resources.Locations.Location_Ptr;
      Color    : Transforms.Vector4 := (1.0, 1.0, 1.0, 1.0);
      Normals  : Boolean := False;
      Cells_Horizontal : Positive := 36;
      Cells_Vertical   : Positive := 18) return Sphere;

   procedure Render
     (Object     : in out Sphere;
      View, Proj : Transforms.Matrix4;
      Transforms, Spheres : Rendering.Buffers.Bindable_Buffer'Class)
   with Pre => Transforms.Length > 0 and Spheres.Length in 2 | 2 * Transforms.Length;
   --  Render a sphere for each transform
   --
   --  The buffer Transforms, containing the transform matrices, must
   --  contain n matrices for n spheres. This buffer controls how many
   --  spheres are rendered.
   --
   --  The buffer Spheres must contain two or 2 * n singles. Each pair
   --  of singles describes the semi-major axis and the flattening of
   --  a sphere. For a perfect sphere with no flattening, use 0.0 for
   --  the flattening.

private

   package LE renames GL.Low_Level.Enums;

   type Sphere is tagged record
      Program : Rendering.Programs.Program;

      Cells_Horizontal, Cells_Vertical : Positive;

      Uniform_Visible : Programs.Uniforms.Uniform (LE.Bool_Type);
      Uniform_View    : Programs.Uniforms.Uniform (LE.Single_Matrix4);
      Uniform_Proj    : Programs.Uniforms.Uniform (LE.Single_Matrix4);
   end record;

end Orka.Rendering.Debug.Spheres;
