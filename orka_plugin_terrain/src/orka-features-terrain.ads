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

--  Based on Jonathan Dupuy's C++ LEB demo [1]. GLSL shaders of LEB library
--  and shaders for rendering of terrain are licensed under the MIT license.
--
--  Contributions:
--
--    * Initialize LEB buffers in a compute shader instead of on the CPU
--
--    * Avoid C implementation of LEB library and reuse the LEB GLSL code
--      to create a static meshlet on the GPU when rendering the terrain
--
--    * Update and render multiple terrain tiles. Use heuristics to
--      determine on the CPU which tiles need to be updated and rendered.
--
--    * Support flattened spheroids with warping to reduce RMSE when
--      projecting cubes on spheres.
--
--  [1] https://github.com/jdupuy/LongestEdgeBisectionDemos

with GL.Objects.Textures;

with Orka.Cameras;
with Orka.Rendering.Buffers;
with Orka.Rendering.Programs.Modules;
with Orka.Resources.Locations;
with Orka.Timers;

private with GL.Low_Level.Enums;
private with GL.Objects.Samplers;

private with Orka.Rendering.Programs.Uniforms;
private with Orka.Types;

package Orka.Features.Terrain is
   pragma Preelaborate;

   type Subdivision_Depth is range 0 .. 29;

   type Meshlet_Subdivision_Depth is range 0 .. 3;

   type Length_Target is range 2 .. 32;

   type LoD_Deviation is digits 4 range 0.0 .. 1.0;

   type Height_Scale is digits 4 range 0.0 .. 1.0;

   type Subdivision_Parameters is record
      Meshlet_Subdivision  : Meshlet_Subdivision_Depth;
      Edge_Length_Target   : Length_Target;
      Min_LoD_Standard_Dev : LoD_Deviation;
   end record;

   subtype Spheroid_Parameters is Float_32_Array (1 .. 4);

   type Visible_Tile_Array is array (Positive range <>) of Boolean;

   -----------------------------------------------------------------------------

   type Terrain (Count : Positive) is tagged limited private;

   function Create_Terrain
     (Count                : Positive;
      Min_Depth, Max_Depth : Subdivision_Depth;
      Scale                : Height_Scale;
      Wireframe            : Boolean;
      Location             : Resources.Locations.Location_Ptr;
      Render_Modules       : Rendering.Programs.Modules.Module_Array;
      Initialize_Render    : access procedure
        (Program : Rendering.Programs.Program)) return Terrain
   with Pre => Min_Depth <= Max_Depth and Max_Depth >= 5;
   --  Create and return a Terrain object that can generate
   --  one or more terrain tiles
   --
   --  If Wireframe is True, then optionally a wireframe can be displayed
   --  when rendering the terrain. In production, this parameter should
   --  be set to False to avoid invoking a geometry shader.
   --
   --  One of the shaders in the module array must contain the following
   --  function:
   --
   --  vec4 ShadeFragment(vec2 texCoord, vec4 worldPos);

   procedure Render
     (Object        : in out Terrain;
      Transforms, Spheres : Rendering.Buffers.Bindable_Buffer'Class;
      Center        : Cameras.Transforms.Matrix4;
      Camera        : Cameras.Camera_Ptr;
      Parameters    : Subdivision_Parameters;
      Visible_Tiles : Visible_Tile_Array;
      Update_Render : access procedure (Program : Rendering.Programs.Program);
      Height_Map    : GL.Objects.Textures.Texture;
      Freeze, Wires : Boolean;
      Timer_Update, Timer_Render : in out Orka.Timers.Timer)
   with Pre => Transforms.Length = Object.Count
     and Spheres.Length in Spheroid_Parameters'Length | Spheroid_Parameters'Length * Object.Count
     and Visible_Tiles'Length = Object.Count;
   --  Generate and render the terrain tiles
   --
   --  Buffer Transforms should contain matrices which transform the
   --  terrain tiles.
   --
   --  Buffer Spheres should contain n spheroid parameters (each containing
   --  4 singles) for n terrain tiles if the sphere is flattened or 1 spheroid
   --  parameters if all the tiles are part of a non-flattened sphere.
   --
   --  Center should contain a translation from the camera to the center
   --  of the sphere. It should be scaled if the semi-major axis in the
   --  spheroid parameters in Spheres is scaled.
   --
   --  If Freeze is True, then the generated terrain will not be
   --  modified.
   --
   --  If Wires is True and if parameter Wireframe of function Create_Terrain
   --  was True, then a wireframe will be displayed. If Wireframe was False,
   --  then Wires has no effect.

   function Get_Spheroid_Parameters
     (Semi_Major_Axis : Float_32;
      Flattening      : Float_32 := 0.0;
      Side            : Boolean  := True) return Spheroid_Parameters;
   --  Return the spheroid parameters needed to project a flat
   --  terrain tile on a (non-)flattened sphere.
   --
   --  The returned spheroid parameters should be put in a buffer that
   --  is given to the procedure Render above.
   --
   --  If the terrain tiles are part of a non-flattened sphere, then
   --  this function can be called once with Flattening = 0.0.
   --
   --  If the sphere is flattened, this function needs to be called twice.
   --  The returned spheroid parameters must be repeated 4 times for the
   --  tiles on the side and 2 times for the top and bottom tiles.
   --
   --  If the semi-major axis is scaled, then the translation in Center
   --  in the procedure Render should be scaled as well.

private

   package LE renames GL.Low_Level.Enums;

   type UInt_Buffer_Array is array (Positive range <>) of
     Rendering.Buffers.Buffer (Types.UInt_Type);

   type Terrain (Count : Positive) is tagged limited record
      Max_Depth    : Subdivision_Depth;
      Scale        : Height_Scale;

      Split_Update : Boolean;
      Wireframe    : Boolean;

      --  Update and reduction of LEB
      Program_Leb_Update    : Rendering.Programs.Program;
      Program_Leb_Prepass   : Rendering.Programs.Program;
      Program_Leb_Reduction : Rendering.Programs.Program;

      --  Prepare indirect commands and render geometry
      Program_Indirect      : Rendering.Programs.Program;
      Program_Render        : Rendering.Programs.Program;

      Uniform_Update_Split  : Rendering.Programs.Uniforms.Uniform (LE.Bool_Type);
      Uniform_Update_Freeze : Rendering.Programs.Uniforms.Uniform (LE.Bool_Type);

      Uniform_Update_LoD_Var     : Rendering.Programs.Uniforms.Uniform (LE.Single_Type);
      Uniform_Update_LoD_Factor  : Rendering.Programs.Uniforms.Uniform (LE.Single_Type);
      Uniform_Update_DMap_Factor : Rendering.Programs.Uniforms.Uniform (LE.Single_Type);
      Uniform_Render_DMap_Factor : Rendering.Programs.Uniforms.Uniform (LE.Single_Type);

      Uniform_Update_Leb_ID     : Rendering.Programs.Uniforms.Uniform (LE.Int_Type);
      Uniform_Indirect_Leb_ID   : Rendering.Programs.Uniforms.Uniform (LE.Int_Type);
      Uniform_Render_Leb_ID     : Rendering.Programs.Uniforms.Uniform (LE.Int_Type);

      Uniform_Prepass_Pass_ID   : Rendering.Programs.Uniforms.Uniform (LE.Int_Type);
      Uniform_Reduction_Pass_ID : Rendering.Programs.Uniforms.Uniform (LE.Int_Type);

      Uniform_Render_Subdiv     : Rendering.Programs.Uniforms.Uniform (LE.Int_Type);

      --  Sampler for height and slope maps
      Sampler : GL.Objects.Samplers.Sampler;

      --  SSBO
      Buffer_Draw             : Rendering.Buffers.Buffer (Types.Arrays_Command_Type);
      Buffer_Dispatch         : Rendering.Buffers.Buffer (Types.Dispatch_Command_Type);
      Buffer_Leb              : UInt_Buffer_Array (1 .. Count);
      Buffer_Leb_Nodes        : UInt_Buffer_Array (1 .. Count);
      Buffer_Leb_Node_Counter : Rendering.Buffers.Buffer (Types.UInt_Type);

      --  UBO
      Buffer_Matrices : Rendering.Buffers.Buffer (Types.Single_Matrix_Type);
   end record;

end Orka.Features.Terrain;
