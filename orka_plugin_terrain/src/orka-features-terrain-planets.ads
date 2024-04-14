--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2023 onox <denkpadje@gmail.com>
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

with Orka.Features.Atmosphere.Rendering;

package Orka.Features.Terrain.Planets is

   type Terrain_Planet (Context : not null access constant Orka.Contexts.Context'Class) is tagged limited private;

   function Create_Terrain_Planet
     (Context              : aliased Orka.Contexts.Context'Class;
      Min_Depth, Max_Depth : Subdivision_Depth;
      Wireframe            : Boolean;
      Location             : Resources.Locations.Location_Ptr;
      Initialize_Render    : access procedure (Programs : Rendering.Programs.Shaders.Shader_Programs);
      Data          : aliased Orka.Features.Atmosphere.Model_Data;
      Parameters    : Features.Atmosphere.Rendering.Model_Parameters;
      Atmosphere    : Features.Atmosphere.Rendering.Atmosphere;
      Location_Data : Resources.Locations.Location_Ptr;
      Height_Scale  : Orka.Float_32;
      Height_Offset : Orka.Float_32) return Terrain_Planet
   with Pre => Min_Depth <= Max_Depth and Max_Depth >= 5;
   --  Return a Terrain_Planet object which can render the six terrain tiles of a planet
   --
   --  Parameter Location_Data should contain the following two files:
   --
   --    - terrain/terrain-dmap.ktx
   --    - terrain/terrain-render-atmosphere.frag

   function Create_Graph
     (Object       : Terrain_Planet;
      Color, Depth : Orka.Rendering.Textures.Texture_Description) return Orka.Frame_Graphs.Frame_Graph;

   procedure Set_Data
     (Object     : in out Terrain_Planet;
      Rotation   : Types.Singles.Matrix4;
      Center     : Types.Singles.Matrix4;
      Camera     : Cameras.Camera_Ptr;
      Star       : Orka.Types.Singles.Vector4;
      Parameters : Features.Terrain.Subdivision_Parameters;
      Freeze     : Boolean;
      Wires      : Boolean);

   function Visible_Tiles (Object : Terrain_Planet) return Visible_Tile_Array
     with Post => Visible_Tiles'Result'Length = 6;

private

   type Terrain_Program_Callback (Data : not null access Terrain_Planet'Class) is
     limited new Orka.Frame_Graphs.Program_Callback with null record;

   overriding procedure Run (Object : Terrain_Program_Callback);

   type Terrain_Planet (Context : not null access constant Orka.Contexts.Context'Class) is tagged limited record
      Terrain : aliased Orka.Features.Terrain.Terrain (Context => Context, Count => 6);

      Callback : aliased Terrain_Program_Callback (Terrain_Planet'Access);

      Spheres : Rendering.Buffers.Buffer (Types.Single_Type);
      --  Buffer Spheres should contain n spheroid parameters (each containing
      --  4 singles) for n terrain tiles if the sphere is flattened or 1 spheroid
      --  parameters if all the tiles are part of a non-flattened sphere.

      Planet_Radius      : Orka.Float_32;
      Planet_Unit_Length : Orka.Float_32;
   end record;

   function Visible_Tiles (Object : Terrain_Planet) return Visible_Tile_Array is (Object.Terrain.Visible_Tiles);

end Orka.Features.Terrain.Planets;
