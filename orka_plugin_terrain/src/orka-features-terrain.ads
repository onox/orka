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
--    * Update and render multiple terrain tiles. Read back from the GPU
--      which tiles had visible nodes last frame to determine which tiles
--      need to be updated and rendered.
--
--    * Support flattened spheroids with warping to reduce RMSE when
--      projecting cubes on spheres.
--
--  [1] https://github.com/jdupuy/LongestEdgeBisectionDemos

with Orka.Cameras;
with Orka.Frame_Graphs;
with Orka.Rendering.Buffers;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Textures;
with Orka.Resources.Locations;
with Orka.Types;

private with Orka.Rendering.Buffers.Mapped.Persistent;
private with Orka.Rendering.Fences;
private with Orka.Rendering.Programs.Uniforms;
private with Orka.Rendering.Samplers;

package Orka.Features.Terrain is
   pragma Preelaborate;

   type Subdivision_Depth is range 0 .. 29;

   type Meshlet_Subdivision_Depth is range 0 .. 6;

   type Length_Target is range 2 .. 32;

   type LoD_Deviation is digits 4 range 0.0 .. 1.0;

   type Subdivision_Parameters is record
      Meshlet_Subdivision  : Meshlet_Subdivision_Depth;
      Edge_Length_Target   : Length_Target;
      Min_LoD_Standard_Dev : LoD_Deviation;
   end record;

   type Visible_Tile_Array is array (Positive range <>) of Boolean;

   -----------------------------------------------------------------------------

   type Terrain_Kind is (Sphere, Plane);

   type Terrain (Count : Positive) is tagged private;

   function Create_Terrain
     (Kind                 : Terrain_Kind;
      Count                : Positive;
      Min_Depth, Max_Depth : Subdivision_Depth;
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

   procedure Set_Data
     (Object        : in out Terrain;
      Transforms    : Rendering.Buffers.Buffer;
      Height_Map    : Rendering.Textures.Texture;
      Height_Scale  : Float_32;
      Height_Offset : Float_32)
   with Pre => Transforms.Length = Object.Count;
   --  Buffer Transforms should contain matrices which transform the terrain tiles

   procedure Set_Data
     (Object        : in out Terrain;
      Rotation      : Types.Singles.Matrix4;
      Center        : Types.Singles.Matrix4;
      Camera        : Cameras.Camera_Ptr;
      Parameters    : Subdivision_Parameters;
      Freeze, Wires : Boolean);
   --  Generate and render the terrain tiles
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

   function Visible_Tiles (Object : Terrain) return Visible_Tile_Array
     with Post => Visible_Tiles'Result'Length = Object.Count;

   package Flat is

      type Terrain_Flat is tagged limited private;

      function Create_Terrain_Flat (Terrain : aliased in out Orka.Features.Terrain.Terrain) return Terrain_Flat;

      function Create_Graph
        (Object       : Terrain_Flat;
         Color, Depth : Orka.Rendering.Textures.Texture_Description) return Orka.Frame_Graphs.Frame_Graph;

   private

      type Terrain_Program_Callback (Data : not null access Terrain_Flat'Class) is
        limited new Orka.Frame_Graphs.Program_Callback with null record;

      overriding procedure Run (Object : Terrain_Program_Callback);

      type Terrain_Flat is tagged limited record
         Terrain  : not null access Orka.Features.Terrain.Terrain;
         Callback : aliased Terrain_Program_Callback (Terrain_Flat'Access);
      end record;

   end Flat;

private

   procedure Render (Object : in out Terrain);

   function Create_Graph
     (Object       : Terrain;
      Color, Depth : Orka.Rendering.Textures.Texture_Description;
      Callback     : not null Orka.Frame_Graphs.Program_Callback_Access) return Orka.Frame_Graphs.Frame_Graph;

   Regions_Counted_Nodes : constant := 3;

   package LE renames Orka.Rendering.Textures.LE;

   type UInt_Buffer_Array is array (Positive range <>) of
     Rendering.Buffers.Buffer (Types.UInt_Type);

   type Terrain (Count : Positive) is tagged record
      Max_Depth    : Subdivision_Depth;

      Split_Update : Boolean;
      Wireframe    : Boolean;
      Freeze       : Boolean;

      Height_Scale  : Float_32;

      Visible_Tiles : Visible_Tile_Array (1 .. Count);

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
      Uniform_Update_DMap_Factor : Rendering.Programs.Uniforms.Uniform (LE.Single_Vec2);
      Uniform_Render_DMap_Factor : Rendering.Programs.Uniforms.Uniform (LE.Single_Vec2);

      Uniform_Update_Leb_ID     : Rendering.Programs.Uniforms.Uniform (LE.Int_Type);
      Uniform_Indirect_Leb_ID   : Rendering.Programs.Uniforms.Uniform (LE.Int_Type);
      Uniform_Render_Leb_ID     : Rendering.Programs.Uniforms.Uniform (LE.Int_Type);

      Uniform_Prepass_Pass_ID   : Rendering.Programs.Uniforms.Uniform (LE.Int_Type);
      Uniform_Reduction_Pass_ID : Rendering.Programs.Uniforms.Uniform (LE.Int_Type);

      Uniform_Render_Subdiv     : Rendering.Programs.Uniforms.Uniform (LE.Int_Type);

      --  Sampler for height map
      Sampler : Rendering.Samplers.Sampler;

      --  SSBO
      Buffer_Draw             : Rendering.Buffers.Buffer (Types.Arrays_Command_Type);
      Buffer_Dispatch         : Rendering.Buffers.Buffer (Types.Dispatch_Command_Type);
      Buffer_Leb              : UInt_Buffer_Array (1 .. Count);
      Buffer_Leb_Nodes        : UInt_Buffer_Array (1 .. Count);
      Buffer_Leb_Node_Counter : Rendering.Buffers.Buffer (Types.UInt_Type);

      Buffer_Counted_Nodes : Rendering.Buffers.Mapped.Persistent.Persistent_Mapped_Buffer
        (Types.UInt_Type, Rendering.Buffers.Mapped.Read);
      Fence_Counted_Nodes  : Rendering.Fences.Buffer_Fence (Regions => Regions_Counted_Nodes);

      --  UBO
      Buffer_Matrices : Rendering.Buffers.Buffer (Types.Single_Matrix_Type);

      Transforms : Rendering.Buffers.Buffer (Types.Single_Matrix_Type);
      Height_Map : Rendering.Textures.Texture (LE.Texture_2D);
   end record;

   function Visible_Tiles (Object : Terrain) return Visible_Tile_Array is (Object.Visible_Tiles);

end Orka.Features.Terrain;
