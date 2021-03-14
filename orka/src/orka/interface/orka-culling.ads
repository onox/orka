--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 onox <denkpadje@gmail.com>
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

private with GL.Low_Level.Enums;

with Orka.Rendering.Buffers;
with Orka.Resources.Locations;
with Orka.Transforms.Singles.Matrices;
with Orka.Types;

private with Orka.Rendering.Programs.Uniforms;
private with Orka.Algorithms.Prefix_Sums;

package Orka.Culling is
   pragma Preelaborate;

   package Transforms renames Orka.Transforms.Singles.Matrices;

   type Culler is tagged private;

   type Culler_Ptr is not null access Culler'Class;

   procedure Bind (Object : in out Culler; View_Projection : Transforms.Matrix4);

   function Create_Culler
     (Location : Resources.Locations.Location_Ptr) return Culler;

   -----------------------------------------------------------------------------

   type Cull_Instance is tagged private;

   procedure Cull
     (Object : in out Cull_Instance;
      Transforms : Rendering.Buffers.Bindable_Buffer'Class;
      Bounds, Commands : Rendering.Buffers.Buffer;
      Compacted_Transforms, Compacted_Commands : out Rendering.Buffers.Buffer;
      Instances : Natural);

   function Create_Instance
     (Culler : Culler_Ptr; Transforms, Commands : Natural) return Cull_Instance
   with Pre => Transforms mod 4 = 0;

private

   package LE renames GL.Low_Level.Enums;
   package Programs renames Rendering.Programs;

   type Culler is tagged record
      Program_Frustum : Programs.Program;
      Program_Compact : Programs.Program;
      PS_Factory      : Algorithms.Prefix_Sums.Factory;

      Uniform_VP : Programs.Uniforms.Uniform (LE.Single_Matrix4);
      Uniform_CF_Instances : Programs.Uniforms.Uniform (LE.UInt_Type);
      Uniform_CC_Instances : Programs.Uniforms.Uniform (LE.UInt_Type);
   end record;

   type Cull_Instance is tagged record
      Buffer_Visibles : Rendering.Buffers.Buffer (Types.UInt_Type);
      Buffer_Indices  : Rendering.Buffers.Buffer (Types.UInt_Type);

      Culler          : Culler_Ptr;
      Prefix_Sum      : Algorithms.Prefix_Sums.Prefix_Sum;

      Work_Groups : Natural;

      Compacted_Transforms : Rendering.Buffers.Buffer (Types.Single_Matrix_Type);
      Compacted_Commands   : Rendering.Buffers.Buffer (Types.Elements_Command_Type);
   end record;

end Orka.Culling;
