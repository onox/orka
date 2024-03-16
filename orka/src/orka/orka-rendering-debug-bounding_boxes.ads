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

with Orka.Frame_Graphs;
with Orka.Rendering.Buffers;
with Orka.Rendering.Textures;
with Orka.Resources.Locations;
with Orka.Transforms.Singles.Matrices;

private with Orka.Rendering.Programs.Uniforms;
private with Orka.Types;

package Orka.Rendering.Debug.Bounding_Boxes is
   pragma Preelaborate;

   package Transforms renames Orka.Transforms.Singles.Matrices;

   type Bounding_Box is tagged limited private;

   function Create_Bounding_Box
     (Location : Resources.Locations.Location_Ptr;
      Color    : Transforms.Vector4 := (1.0, 1.0, 1.0, 1.0)) return Bounding_Box;

   function Create_Graph
     (Object       : Bounding_Box;
      Color, Depth : Orka.Rendering.Textures.Texture_Description) return Orka.Frame_Graphs.Frame_Graph;

   procedure Set_Data
     (Object     : in out Bounding_Box;
      View, Proj : Transforms.Matrix4;
      Transforms, Bounds : Rendering.Buffers.Buffer)
   with Pre => 2 * Transforms.Length = Bounds.Length;
   --  Render a bounding box for each transform and bounds
   --
   --  The bounds of a bounding box consists of two vectors.

private

   package LE renames Orka.Rendering.Textures.LE;

   type BBox_Hidden_Program_Callback (Data : not null access Bounding_Box) is limited new Orka.Frame_Graphs.Program_Callback with null record;
   type BBox_Visible_Program_Callback (Data : not null access Bounding_Box) is limited new Orka.Frame_Graphs.Program_Callback with null record;

   overriding procedure Run (Object : BBox_Hidden_Program_Callback; Program : Rendering.Programs.Program);
   overriding procedure Run (Object : BBox_Visible_Program_Callback; Program : Rendering.Programs.Program);

   type Bounding_Box is tagged limited record
      Program : Rendering.Programs.Program;

      Uniform_Visible : Programs.Uniforms.Uniform (LE.Bool_Type);
      Uniform_View    : Programs.Uniforms.Uniform (LE.Single_Matrix4);
      Uniform_Proj    : Programs.Uniforms.Uniform (LE.Single_Matrix4);

      Callback_Hidden  : aliased BBox_Hidden_Program_Callback (Bounding_Box'Access);
      Callback_Visible : aliased BBox_Visible_Program_Callback (Bounding_Box'Access);

      Transforms : Rendering.Buffers.Buffer (Orka.Types.Single_Matrix_Type);
      Bounds     : Rendering.Buffers.Buffer (Orka.Types.Single_Vector_Type);
   end record;

end Orka.Rendering.Debug.Bounding_Boxes;
