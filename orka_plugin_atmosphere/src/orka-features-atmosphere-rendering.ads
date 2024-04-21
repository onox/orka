--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2020 onox <denkpadje@gmail.com>
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

with Orka.Behaviors;
with Orka.Cameras;
with Orka.Contexts;
with Orka.Frame_Graphs;
with Orka.Rendering.Buffers;
with Orka.Rendering.Shaders.Modules;
with Orka.Rendering.Shaders.Objects;
with Orka.Rendering.Textures;
with Orka.Types;

private with Orka.Rendering.Shaders.Uniforms;

package Orka.Features.Atmosphere.Rendering is
   pragma Preelaborate;

   Sun_Radius_Meter : constant := 695_700_000.0;

   type Model_Parameters is record
      Semi_Major_Axis : Cameras.Distance;
      Flattening      : Cameras.Distance := 0.0;
      Axial_Tilt      : Cameras.Angle;
      Star_Radius     : Cameras.Distance := Sun_Radius_Meter;
   end record;
   --  Semi-major axis and the axial tilt are only used if flattening > 0.0

   type Atmosphere (Context : not null access constant Orka.Contexts.Context'Class) is tagged limited private;

   function Create_Atmosphere
     (Context    : aliased Orka.Contexts.Context'Class;
      Data       : aliased Model_Data;
      Textures   : Precomputed_Textures;
      Parameters : Model_Parameters := (others => <>)) return Atmosphere;

   function Shader_Module (Object : Atmosphere) return Orka.Rendering.Shaders.Modules.Shader_Module;
   --  Return the shader module for use by other features like terrain
   --  rendering

   function Create_Graph
     (Object       : Atmosphere;
      Color, Depth : Orka.Rendering.Textures.Texture_Description) return Orka.Frame_Graphs.Frame_Graph;

   procedure Set_Data
     (Object : in out Atmosphere;
      Camera : Cameras.Camera_Ptr;
      Planet : Behaviors.Behavior_Ptr;
      Star   : Behaviors.Behavior_Ptr);
   --  Render the atmosphere on the far plane
   --
   --  This procedure assumes that the precomputed textures have been
   --  binded with procedure Orka.Features.Atmosphere.Bind_Textures.

private

   package LE renames Orka.Rendering.Textures.LE;

   type Atmosphere_Program_Callback (Data : not null access Atmosphere'Class) is
     limited new Orka.Frame_Graphs.Program_Callback with null record;

   overriding procedure Run (Object : Atmosphere_Program_Callback);

   type Atmosphere (Context : not null access constant Orka.Contexts.Context'Class) is tagged limited record
      Program : Orka.Rendering.Shaders.Objects.Shader_Objects;
      Module  : Orka.Rendering.Shaders.Modules.Shader_Module;

      Textures : Precomputed_Textures;

      Parameters : Model_Parameters;

      Bottom_Radius  : Float_64;
      Distance_Scale : Float_64;

      Buffer_Matrices : Orka.Rendering.Buffers.Buffer (Types.Single_Matrix_Type);
      Buffer_Metadata : Orka.Rendering.Buffers.Buffer (Types.Single_Vector_Type);

      Uniform_Ground_Hack   : Orka.Rendering.Shaders.Uniforms.Uniform (LE.Bool_Type);
      Uniform_Camera_Offset : Orka.Rendering.Shaders.Uniforms.Uniform (LE.Single_Vec4);

      Callback : aliased Atmosphere_Program_Callback (Atmosphere'Access);
   end record;

end Orka.Features.Atmosphere.Rendering;
