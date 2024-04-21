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

package Orka.Rendering.Debug.Coordinate_Axes is
   pragma Preelaborate;

   package Transforms renames Orka.Transforms.Singles.Matrices;

   type Coordinate_Axes (Context : not null access constant Orka.Contexts.Context'Class) is tagged limited private;

   function Create_Coordinate_Axes (Context : aliased Orka.Contexts.Context'Class) return Coordinate_Axes;

   function Create_Graph
     (Object       : Coordinate_Axes;
      Color, Depth : Orka.Rendering.Textures.Texture_Description) return Orka.Frame_Graphs.Frame_Graph;

   procedure Set_Data
     (Object        : in out Coordinate_Axes;
      Width, Height : Positive;
      Axis_Size     : Transforms.Vector4 := [4.0, 100.0, 16.0, 32.0];
      View, Proj    : Transforms.Matrix4;
      Transforms    : Rendering.Buffers.Buffer)
   with Pre => Transforms.Length > 0;
   --  Render three coordinates axes for each transform
   --
   --  The buffer Transforms, containing the transform matrices, must
   --  contain n matrices for n coordinate axes. This buffer controls how
   --  many coordinate axes are rendered.

private

   package LE renames Orka.Rendering.Textures.LE;

   type Axes_Program_Callback (Data : not null access Coordinate_Axes) is
     limited new Orka.Frame_Graphs.Program_Callback with null record;

   overriding procedure Run (Object : Axes_Program_Callback);

   type Coordinate_Axes (Context : not null access constant Orka.Contexts.Context'Class) is tagged limited record
      Program : Rendering.Shaders.Objects.Shader_Objects;

      Uniform_View : Shaders.Uniforms.Uniform (LE.Single_Matrix4);
      Uniform_Proj : Shaders.Uniforms.Uniform (LE.Single_Matrix4);
      Uniform_Size : Shaders.Uniforms.Uniform (LE.UInt_Vec2);
      Uniform_Axis : Shaders.Uniforms.Uniform (LE.Single_Vec4);

      Callback : aliased Axes_Program_Callback (Coordinate_Axes'Access);

      Transforms : Rendering.Buffers.Buffer (Orka.Types.Single_Matrix_Type);
   end record;

end Orka.Rendering.Debug.Coordinate_Axes;
