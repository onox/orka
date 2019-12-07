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

package Orka.Rendering.Debug.Lines is
   pragma Preelaborate;

   package Transforms renames Orka.Transforms.Singles.Matrices;

   type Line is tagged private;

   function Create_Line
     (Location : Resources.Locations.Location_Ptr) return Line;

   procedure Render
     (Object     : in out Line;
      View, Proj : Transforms.Matrix4;
      Transforms, Colors, Points : Rendering.Buffers.Bindable_Buffer'Class)
   with Pre => Transforms.Length in 1 | Points.Length / 2
                 and Colors.Length in 1 | Points.Length / 2
                 and Points.Length mod 2 = 0;
   --  Render lines between pairs of points
   --
   --  The buffer Transforms, containing the transform matrices, must
   --  contain one or n matrices for n lines. If all lines exist
   --  in the same world space, then one matrix transform is sufficient.
   --
   --  The buffer Colors must contain between one or n vectors.
   --
   --  The buffer Points must contain 2 * n points.

private

   package LE renames GL.Low_Level.Enums;

   type Line is tagged record
      Program : Rendering.Programs.Program;

      Uniform_Visible : Programs.Uniforms.Uniform (LE.Bool_Type);

      Uniform_View    : Programs.Uniforms.Uniform (LE.Single_Matrix4);
      Uniform_Proj    : Programs.Uniforms.Uniform (LE.Single_Matrix4);
   end record;

end Orka.Rendering.Debug.Lines;
