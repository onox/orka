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

package Orka.Rendering.Debug.Bounding_Boxes is
   pragma Preelaborate;

   package Transforms renames Orka.Transforms.Singles.Matrices;

   type Bounding_Box is tagged private;

   function Create_Bounding_Box
     (Location : Resources.Locations.Location_Ptr;
      Color    : Transforms.Vector4 := (1.0, 1.0, 1.0, 1.0)) return Bounding_Box;

   procedure Render
     (Object     : in out Bounding_Box;
      View, Proj : Transforms.Matrix4;
      Transforms, Bounds : Rendering.Buffers.Bindable_Buffer'Class)
   with Pre => 2 * Transforms.Length = Bounds.Length;
   --  Render a bounding box for each transform and bounds
   --
   --  The bounds of a bounding box consists of two vectors.

private

   package LE renames GL.Low_Level.Enums;

   type Bounding_Box is tagged record
      Program : Rendering.Programs.Program;

      Uniform_Visible : Programs.Uniforms.Uniform (LE.Bool_Type);
      Uniform_View    : Programs.Uniforms.Uniform (LE.Single_Matrix4);
      Uniform_Proj    : Programs.Uniforms.Uniform (LE.Single_Matrix4);
   end record;

end Orka.Rendering.Debug.Bounding_Boxes;
