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

with Ada.Characters.Latin_1;
with Ada.Numerics;

with Orka.Rendering.Textures;
with Orka.Resources.Textures.KTX;
with Orka.Transforms.Doubles.Matrices;
with Orka.Transforms.Doubles.Matrix_Conversions;
with Orka.Transforms.Doubles.Quaternions;
with Orka.Transforms.Doubles.Vectors;
with Orka.Transforms.Doubles.Vector_Conversions;

package body Orka.Features.Terrain.Helpers is

   Count : constant := 6;

   function Render_Modules (Object : Terrain_Planet)
     return Rendering.Programs.Modules.Module_Array
   is (Object.Modules_Terrain_Render);

   function Height_Map (Object : Terrain_Planet) return GL.Objects.Textures.Texture is
     (Object.DMap);

   function Slope_Map (Object : Terrain_Planet) return GL.Objects.Textures.Texture is
     (Object.SMap);

   function Create_Terrain_Planet
     (Data             : aliased Orka.Features.Atmosphere.Model_Data;
      Parameters       : Features.Atmosphere.Rendering.Model_Parameters;
      Atmosphere       : Orka.Features.Atmosphere.Cache.Cached_Atmosphere;
      Location_Data    : Orka.Resources.Locations.Location_Ptr;
      Location_Shaders : Orka.Resources.Locations.Location_Ptr) return Terrain_Planet
   is
      use Orka.Rendering.Buffers;

      Planet_Radius : constant Orka.Float_64 :=
        Parameters.Semi_Major_Axis / Data.Length_Unit_In_Meters;

      Terrain_Sphere_Side : constant Orka.Features.Terrain.Spheroid_Parameters :=
        Orka.Features.Terrain.Get_Spheroid_Parameters
          (Orka.Float_32 (Planet_Radius),
           Orka.Float_32 (Parameters.Flattening), True);

      Terrain_Sphere_Top : constant Orka.Features.Terrain.Spheroid_Parameters :=
        Orka.Features.Terrain.Get_Spheroid_Parameters
          (Orka.Float_32 (Planet_Radius),
           Orka.Float_32 (Parameters.Flattening), False);

      Terrain_Spheres : constant Orka.Float_32_Array :=
        Terrain_Sphere_Side &
        Terrain_Sphere_Side &
        Terrain_Sphere_Side &
        Terrain_Sphere_Side &
        Terrain_Sphere_Top &
        Terrain_Sphere_Top;

      -------------------------------------------------------------------------

      package MC renames Orka.Transforms.Doubles.Matrix_Conversions;
      package Quaternions renames Orka.Transforms.Doubles.Quaternions;

      Q_Rotate_90 : constant Quaternions.Quaternion :=
        Quaternions.R (Orka.Transforms.Doubles.Vectors.Normalize ((0.0, 0.0, 1.0, 0.0)),
        -2.0 * Ada.Numerics.Pi * 0.25);

      Q_Rotate_180 : constant Quaternions.Quaternion :=
        Quaternions.R (Orka.Transforms.Doubles.Vectors.Normalize ((0.0, 0.0, 1.0, 0.0)),
        -2.0 * Ada.Numerics.Pi * 0.50);

      Q_Rotate_270 : constant Quaternions.Quaternion :=
        Quaternions.R (Orka.Transforms.Doubles.Vectors.Normalize ((0.0, 0.0, 1.0, 0.0)),
        -2.0 * Ada.Numerics.Pi * 0.75);

      Q_Rotate_90_Up : constant Quaternions.Quaternion :=
        Quaternions.R (Orka.Transforms.Doubles.Vectors.Normalize ((0.0, 1.0, 0.0, 0.0)),
        2.0 * Ada.Numerics.Pi * 0.25);

      Q_Rotate_90_Down : constant Quaternions.Quaternion :=
        Quaternions.R (Orka.Transforms.Doubles.Vectors.Normalize ((0.0, 1.0, 0.0, 0.0)),
        -2.0 * Ada.Numerics.Pi * 0.25);

      package Matrices renames Orka.Transforms.Doubles.Matrices;
      package Transforms renames Orka.Cameras.Transforms;

      Rotate_90 : constant Transforms.Matrix4 :=
        MC.Convert (Matrices.R (Matrices.Vector4 (Q_Rotate_90)));
      Rotate_180 : constant Transforms.Matrix4 :=
        MC.Convert (Matrices.R (Matrices.Vector4 (Q_Rotate_180)));
      Rotate_270 : constant Transforms.Matrix4 :=
        MC.Convert (Matrices.R (Matrices.Vector4 (Q_Rotate_270)));

      Rotate_90_Up : constant Transforms.Matrix4 :=
        MC.Convert (Matrices.R (Matrices.Vector4 (Q_Rotate_90_Up)));
      Rotate_90_Down : constant Transforms.Matrix4 :=
        MC.Convert (Matrices.R (Matrices.Vector4 (Q_Rotate_90_Down)));

      -------------------------------------------------------------------------

      DMap : constant GL.Objects.Textures.Texture :=
        Orka.Resources.Textures.KTX.Read_Texture (Location_Data, "terrain/texture4k-dmap.ktx");
      SMap : constant GL.Objects.Textures.Texture :=
        Orka.Resources.Textures.KTX.Read_Texture (Location_Data, "terrain/texture4k-smap.ktx");
      --  FIXME Should be separate textures for each tile

      Terrain_GLSL : constant String
        := Orka.Resources.Convert (Orka.Resources.Byte_Array'(Location_Data.Read_Data
             ("terrain/terrain-render-atmosphere.frag").Get));

      use Ada.Characters.Latin_1;
      use Orka.Rendering.Programs;
      use Orka.Features.Atmosphere;

      Terrain_FS_Shader : constant String :=
        "#version 420" & LF &
        "#extension GL_ARB_shader_storage_buffer_object : require" & LF &
        (if Data.Luminance /= Orka.Features.Atmosphere.None then
           "#define USE_LUMINANCE" & LF
         else "") &
        "const float kLengthUnitInMeters = " &
          Data.Length_Unit_In_Meters'Image & ";" & LF &
        Terrain_GLSL & LF;

      Modules_Terrain_Render : constant Modules.Module_Array := Modules.Module_Array'
        (Atmosphere.Shader_Module,
         Modules.Create_Module_From_Sources (FS => Terrain_FS_Shader));
   begin
      return
        (Terrain_Transforms =>
          Create_Buffer
            ((Dynamic_Storage => True, others => False), Orka.Types.Single_Matrix_Type,
             Length => Count),
         Terrain_Sphere_Params =>
           Create_Buffer ((others => False), Terrain_Spheres),
         Terrain_Spheroid_Parameters => Terrain_Sphere_Side,

         Rotate_90      => Rotate_90,
         Rotate_180     => Rotate_180,
         Rotate_270     => Rotate_270,
         Rotate_90_Up   => Rotate_90_Up,
         Rotate_90_Down => Rotate_90_Down,

         Planet_Radius      => Planet_Radius,
         Planet_Unit_Length => Data.Length_Unit_In_Meters,

         Modules_Terrain_Render => Modules_Terrain_Render,
         DMap => DMap,
         SMap => SMap);
   end Create_Terrain_Planet;

   procedure Render
     (Object        : in out Terrain_Planet;
      Terrain       : in out Orka.Features.Terrain.Terrain;
      Parameters    : Orka.Features.Terrain.Subdivision_Parameters;
      Visible_Tiles : out Visible_Tile_Array;
      Camera        : Orka.Cameras.Camera_Ptr;
      Planet, Star  : Orka.Behaviors.Behavior_Ptr;
      Rotation      : Orka.Types.Singles.Matrix4;
      Center        : Orka.Cameras.Transforms.Matrix4;
      Freeze        : Boolean;
      Wires         : Boolean;
      Timer_Update  : in out Orka.Timers.Timer;
      Timer_Render  : in out Orka.Timers.Timer)
   is
      procedure Update_Atmosphere_Terrain
        (Program : Orka.Rendering.Programs.Program)
      is
         use Orka.Transforms.Doubles.Vectors;

         package VC renames Orka.Transforms.Doubles.Vector_Conversions;

         CP : constant Orka.Types.Singles.Vector4 :=
           VC.Convert (Camera.View_Position * (1.0 / Object.Planet_Unit_Length));

         Binding_Texture_SMap : constant := 5;
      begin
         Program.Uniform ("camera_pos").Set_Vector (CP);
         Program.Uniform ("earth_radius").Set_Single
           (Orka.Float_32 (Object.Planet_Radius));

         Program.Uniform ("sun_direction").Set_Vector
           (Orka.Types.Singles.Vector4'(VC.Convert
              (Normalize (Star.Position - Planet.Position))));

         Orka.Rendering.Textures.Bind
           (Object.SMap,
            Orka.Rendering.Textures.Texture, Binding_Texture_SMap);
      end Update_Atmosphere_Terrain;

      use Orka.Cameras.Transforms;

      Tile_Transforms : constant Orka.Types.Singles.Matrix4_Array :=
        (1 => Rotation,
         2 => Rotation * Object.Rotate_90,
         3 => Rotation * Object.Rotate_180,
         4 => Rotation * Object.Rotate_270,
         5 => Rotation * Object.Rotate_90_Up,
         6 => Rotation * Object.Rotate_90_Down);
   begin
      Object.Terrain_Transforms.Set_Data (Tile_Transforms);

      Terrain.Render
        (Transforms    => Object.Terrain_Transforms,
         Spheres       => Object.Terrain_Sphere_Params,
         Center        => Center,
         Camera        => Camera,
         Parameters    => Parameters,
         Visible_Tiles => Visible_Tiles,
         Update_Render => Update_Atmosphere_Terrain'Access,
         Height_Map    => Object.DMap,
         Freeze        => Freeze,
         Wires         => Wires,
         Timer_Update  => Timer_Update,
         Timer_Render  => Timer_Render);
   end Render;

end Orka.Features.Terrain.Helpers;
