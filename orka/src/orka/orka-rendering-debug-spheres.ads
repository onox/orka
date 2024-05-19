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

with Orka.Contexts;
with Orka.Frame_Graphs;
with Orka.Rendering.Buffers;
with Orka.Rendering.Textures;
with Orka.Transforms.Singles.Matrices;

private with Orka.Rendering.Shaders.Objects;
private with Orka.Rendering.Shaders.Uniforms;
private with Orka.Types;

package Orka.Rendering.Debug.Spheres is
   pragma Preelaborate;

   package Transforms renames Orka.Transforms.Singles.Matrices;

   type Sphere (Context : not null access constant Orka.Contexts.Context'Class) is tagged limited private;

   function Create_Sphere
     (Context : aliased Orka.Contexts.Context'Class;
      Color   : Transforms.Vector4 := [1.0, 1.0, 1.0, 1.0];
      Normals : Boolean := False;
      Cells_Horizontal : Positive := 36;
      Cells_Vertical   : Positive := 18) return Sphere;

   function Create_Graph
     (Object       : Sphere;
      Color, Depth : Orka.Rendering.Textures.Texture_Description) return Orka.Frame_Graphs.Frame_Graph;

   procedure Set_Data
     (Object     : in out Sphere;
      View, Proj : Transforms.Matrix4;
      Transforms, Spheres : Rendering.Buffers.Buffer)
   with Pre => Spheres.Length in 2 | 2 * Transforms.Length;
   --  Set matrices and buffers to render a sphere for each transform
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

   package LE renames Orka.Rendering.Textures.LE;

   type Spheres_Hidden_Program_Callback (Data : not null access Sphere) is limited new Orka.Frame_Graphs.Program_Callback with null record;
   type Spheres_Visible_Program_Callback (Data : not null access Sphere) is limited new Orka.Frame_Graphs.Program_Callback with null record;

   overriding procedure Run (Object : Spheres_Hidden_Program_Callback);
   overriding procedure Run (Object : Spheres_Visible_Program_Callback);

   type Sphere (Context : not null access constant Orka.Contexts.Context'Class) is tagged limited record
      Program : Rendering.Shaders.Objects.Shader_Objects;

      Cells_Horizontal, Cells_Vertical : Positive;

      Uniform_Visible : Shaders.Uniforms.Uniform (LE.Bool_Type);
      Uniform_View    : Shaders.Uniforms.Uniform (LE.Single_Matrix4);
      Uniform_Proj    : Shaders.Uniforms.Uniform (LE.Single_Matrix4);

      Callback_Hidden  : aliased Spheres_Hidden_Program_Callback (Sphere'Access);
      Callback_Visible : aliased Spheres_Visible_Program_Callback (Sphere'Access);

      Transforms : Rendering.Buffers.Buffer (Orka.Types.Single_Matrix_Type);
      Spheres    : Rendering.Buffers.Buffer (Orka.Types.Single_Type);
   end record;

end Orka.Rendering.Debug.Spheres;
