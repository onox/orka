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

with Orka.Behaviors;
with Orka.Cameras;
with Orka.Features.Atmosphere;
with Orka.Resources.Locations;
with Orka.Timers;
with Orka.Types;

with Orka.Rendering.Buffers;
with Orka.Rendering.Programs.Modules;
with Orka.Features.Atmosphere.Cache;
with Orka.Features.Atmosphere.Rendering;

with GL.Objects.Textures;

private with GL.Low_Level.Enums;

package Orka.Features.Terrain.Helpers is

   type Terrain_Planet is tagged limited private;

   function Height_Map (Object : Terrain_Planet) return GL.Objects.Textures.Texture;
   function Slope_Map  (Object : Terrain_Planet) return GL.Objects.Textures.Texture;

   function Render_Modules (Object : Terrain_Planet)
     return Rendering.Programs.Modules.Module_Array;

   function Create_Terrain_Planet
     (Data             : aliased Orka.Features.Atmosphere.Model_Data;
      Parameters       : Features.Atmosphere.Rendering.Model_Parameters;
      Atmosphere       : Features.Atmosphere.Cache.Cached_Atmosphere;
      Location_Data    : Resources.Locations.Location_Ptr;
      Location_Shaders : Resources.Locations.Location_Ptr) return Terrain_Planet;

   procedure Render
     (Object        : in out Terrain_Planet;
      Terrain       : in out Features.Terrain.Terrain;
      Parameters    : Features.Terrain.Subdivision_Parameters;
      Visible_Tiles : out Visible_Tile_Array;
      Camera        : Cameras.Camera_Ptr;
      Planet, Star  : Behaviors.Behavior_Ptr;
      Rotation      : Types.Singles.Matrix4;
      Center        : Cameras.Transforms.Matrix4;
      Freeze        : Boolean;
      Wires         : Boolean;
      Timer_Update  : in out Timers.Timer;
      Timer_Render  : in out Timers.Timer);

private

   use Orka.Cameras;

   package LE renames GL.Low_Level.Enums;

   type Terrain_Planet is tagged limited record
      Terrain_Transforms    : Rendering.Buffers.Buffer (Orka.Types.Single_Matrix_Type);
      Terrain_Sphere_Params : Rendering.Buffers.Buffer (Orka.Types.Single_Type);

      Terrain_Spheroid_Parameters : Features.Terrain.Spheroid_Parameters;

      Modules_Terrain_Render : Rendering.Programs.Modules.Module_Array (1 .. 2);

      Rotate_90      : Transforms.Matrix4;
      Rotate_180     : Transforms.Matrix4;
      Rotate_270     : Transforms.Matrix4;
      Rotate_90_Up   : Transforms.Matrix4;
      Rotate_90_Down : Transforms.Matrix4;

      Planet_Radius      : Float_64;
      Planet_Unit_Length : Float_64;

      DMap : GL.Objects.Textures.Texture (LE.Texture_2D);
      SMap : GL.Objects.Textures.Texture (LE.Texture_2D);
   end record;

end Orka.Features.Terrain.Helpers;
