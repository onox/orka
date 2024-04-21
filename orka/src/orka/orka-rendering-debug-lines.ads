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

package Orka.Rendering.Debug.Lines is
   pragma Preelaborate;

   package Transforms renames Orka.Transforms.Singles.Matrices;

   type Line (Context : not null access constant Orka.Contexts.Context'Class) is tagged limited private;

   function Create_Line (Context : aliased Orka.Contexts.Context'Class) return Line;

   function Create_Graph
     (Object       : Line;
      Color, Depth : Orka.Rendering.Textures.Texture_Description) return Orka.Frame_Graphs.Frame_Graph;

   procedure Set_Data
     (Object     : in out Line;
      View, Proj : Transforms.Matrix4;
      Transforms, Colors, Points : Rendering.Buffers.Buffer)
   with Pre => Transforms.Length in 1 | Points.Length / 2
                 and Colors.Length in 1 | Points.Length / 2
                 and Points.Length mod 2 = 0;
   --  Render lines between pairs of points
   --
   --  The buffer Transforms, containing the transform matrices, must
   --  contain one or n matrices for n lines. If all lines exist
   --  in the same world space, then one matrix transform is sufficient.
   --
   --  The buffer Colors must contain one or n vectors.
   --
   --  The buffer Points must contain 2 * n points. This buffers controls
   --  how many lines are rendered.

private

   package LE renames Orka.Rendering.Textures.LE;

   type Line_Hidden_Program_Callback (Data : not null access Line) is limited new Orka.Frame_Graphs.Program_Callback with null record;
   type Line_Visible_Program_Callback (Data : not null access Line) is limited new Orka.Frame_Graphs.Program_Callback with null record;

   overriding procedure Run (Object : Line_Hidden_Program_Callback);
   overriding procedure Run (Object : Line_Visible_Program_Callback);

   type Line (Context : not null access constant Orka.Contexts.Context'Class) is tagged limited record
      Program : Rendering.Shaders.Objects.Shader_Objects;

      Uniform_Visible : Shaders.Uniforms.Uniform (LE.Bool_Type);

      Uniform_View    : Shaders.Uniforms.Uniform (LE.Single_Matrix4);
      Uniform_Proj    : Shaders.Uniforms.Uniform (LE.Single_Matrix4);

      Callback_Hidden  : aliased Line_Hidden_Program_Callback (Line'Access);
      Callback_Visible : aliased Line_Visible_Program_Callback (Line'Access);

      Transforms : Rendering.Buffers.Buffer (Orka.Types.Single_Matrix_Type);
      Colors     : Rendering.Buffers.Buffer (Orka.Types.Single_Vector_Type);
      Points     : Rendering.Buffers.Buffer (Orka.Types.Single_Vector_Type);
   end record;

end Orka.Rendering.Debug.Lines;
