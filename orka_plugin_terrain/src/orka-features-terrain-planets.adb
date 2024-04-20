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

with Orka.Resources.Textures.KTX;
with Orka.Transforms.Doubles.Vectors;
with Orka.Transforms.Doubles.Vector_Conversions;
with Orka.Transforms.Singles.Matrices;

package body Orka.Features.Terrain.Planets is

   Binding_Buffer_Spheres : constant := 4;

   subtype Spheroid_Parameters is Float_32_Array (1 .. 4);

   function Get_Spheroid_Parameters
     (Semi_Major_Axis : Float_32;
      Flattening      : Float_32 := 0.0;
      Side            : Boolean  := True) return Spheroid_Parameters;
   --  Return the spheroid parameters needed to project a flat
   --  terrain tile on a (non-)flattened sphere.
   --
   --  If the terrain tiles are part of a non-flattened sphere, then
   --  this function can be called once with Flattening = 0.0.
   --
   --  If the sphere is flattened, this function needs to be called twice.
   --  The returned spheroid parameters must be repeated 4 times for the
   --  tiles on the side and 2 times for the top and bottom tiles.
   --
   --  If the semi-major axis is scaled, then the translation in Center
   --  in the procedure Set_Data should be scaled as well.

   function Get_Spheroid_Parameters
     (Semi_Major_Axis : Float_32;
      Flattening      : Float_32 := 0.0;
      Side            : Boolean  := True) return Spheroid_Parameters
   is
      --  Convert from geodetic coordinates to geocentric coordinates
      --  using the semi-major axis and the flattening of the sphere.
      --  See https://en.wikipedia.org/wiki/Geographic_coordinate_conversion

      --  Semi-major axis and flattening (semi-minor axis B = A * (1.0 - F))
      A : Float_32 := Semi_Major_Axis;
      F : Float_32 := Flattening;
      --  F = (A - B) / A
      B : constant Float_32 := A - F * A;
   begin
      if not Side then
         --  Recompute F with swapped A and B
         F := (B - A) / B;
         --  Use semi-minor axis instead
         A := B;
      end if;

      declare
         E2 : constant Float_32 := 2.0 * F - F * F;
         --  E is eccentricity. See https://en.wikipedia.org/wiki/Flattening
      begin
         return [A, E2] & (if Side then [0.0, 1.0] else [1.0, 1.0]);
      end;
   end Get_Spheroid_Parameters;

   function Create_Terrain_Planet
     (Context              : aliased Orka.Contexts.Context'Class;
      Min_Depth, Max_Depth : Subdivision_Depth;
      Wireframe            : Boolean;
      Location             : Resources.Locations.Location_Ptr;
      Initialize_Render    : access procedure (Shaders : Rendering.Shaders.Objects.Shader_Objects);
      Data          : aliased Orka.Features.Atmosphere.Model_Data;
      Parameters    : Features.Atmosphere.Rendering.Model_Parameters;
      Atmosphere    : Features.Atmosphere.Rendering.Atmosphere;
      Location_Data : Orka.Resources.Locations.Location_Ptr;
      Height_Scale  : Orka.Float_32;
      Height_Offset : Orka.Float_32) return Terrain_Planet
   is
      use Orka.Rendering.Buffers;

      Planet_Radius : constant Orka.Float_32 :=
        Orka.Float_32 (Parameters.Semi_Major_Axis / Data.Length_Unit_In_Meters);

      Terrain_Sphere_Side : constant Spheroid_Parameters :=
        Get_Spheroid_Parameters (Planet_Radius, Orka.Float_32 (Parameters.Flattening), True);

      Terrain_Sphere_Top : constant Spheroid_Parameters :=
        Get_Spheroid_Parameters (Planet_Radius, Orka.Float_32 (Parameters.Flattening), False);

      Terrain_Spheres : constant Orka.Float_32_Array :=
        Terrain_Sphere_Side &
        Terrain_Sphere_Side &
        Terrain_Sphere_Side &
        Terrain_Sphere_Side &
        Terrain_Sphere_Top &
        Terrain_Sphere_Top;

      package Matrices renames Orka.Transforms.Singles.Matrices;

      Terrain_Transforms : constant Orka.Types.Singles.Matrix4_Array :=
        [Matrices.Identity_Matrix,
         Matrices.Ry (+0.5 * Ada.Numerics.Pi),
         Matrices.Ry (+1.0 * Ada.Numerics.Pi),
         Matrices.Ry (+1.5 * Ada.Numerics.Pi),
         Matrices.Rx (-0.5 * Ada.Numerics.Pi),
         Matrices.Rx (+0.5 * Ada.Numerics.Pi)];

      -------------------------------------------------------------------------

      Height_Map : constant Rendering.Textures.Texture :=
        Orka.Resources.Textures.KTX.Read_Texture (Location_Data, "terrain/terrain-dmap.ktx");

      Terrain_GLSL : constant String
        := Orka.Resources.Convert (Orka.Resources.Byte_Array'(Location_Data.Read_Data
             ("terrain/terrain-render-atmosphere.frag").Get));

      use Ada.Characters.Latin_1;
      use Orka.Rendering.Shaders;
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

      Modules_Terrain_Render : constant Modules.Shader_Module_Array :=
        [Atmosphere.Shader_Module,
         Modules.Create_Module_From_Source (Fragment_Shader, Terrain_FS_Shader)];

      procedure Initialize_Program_Render (Programs : Orka.Rendering.Shaders.Objects.Shader_Objects) is
      begin
         Programs (Fragment_Shader).Value.Uniform_Sampler ("u_DmapSampler").Verify_Compatibility (Height_Map);

         if Initialize_Render /= null then
            Initialize_Render.all (Programs);
         end if;
      end Initialize_Program_Render;
   begin
      return Result : Terrain_Planet :=
        (Terrain            => Orka.Features.Terrain.Create_Terrain
           (Context, Sphere, 6, Min_Depth, Max_Depth, Wireframe, Location, Modules_Terrain_Render,
            Initialize_Program_Render'Access),
         Spheres            => Create_Buffer ((others => False), Terrain_Spheres),
         Planet_Radius      => Planet_Radius,
         Planet_Unit_Length => Orka.Float_32 (Data.Length_Unit_In_Meters),
         Context            => Context'Access,
         others             => <>)
      do
         Result.Terrain.Set_Data
           (Transforms    => Create_Buffer ((others => False), Terrain_Transforms),
            Height_Map    => Height_Map,
            Height_Scale  => Height_Scale / Result.Planet_Unit_Length,
            Height_Offset => Height_Offset / Result.Planet_Unit_Length);
      end return;
   end Create_Terrain_Planet;

   procedure Set_Data
     (Object        : in out Terrain_Planet;
      Rotation      : Types.Singles.Matrix4;
      Center        : Types.Singles.Matrix4;
      Camera        : Cameras.Camera_Ptr;
      Star          : Orka.Types.Singles.Vector4;
      Parameters    : Subdivision_Parameters;
      Freeze, Wires : Boolean)
   is
      use Orka.Transforms.Doubles.Vectors;

      package VC renames Orka.Transforms.Doubles.Vector_Conversions;

      CP : constant Orka.Types.Singles.Vector4 :=
        VC.Convert (Camera.View_Position * (1.0 / Orka.Float_64 (Object.Planet_Unit_Length)));

      use all type Rendering.Shaders.Shader_Kind;
   begin
      Object.Terrain.Set_Data
        (Rotation   => Rotation,
         Center     => Center,
         Camera     => Camera,
         Parameters => Parameters,
         Freeze     => Freeze,
         Wires      => Wires);

      Object.Terrain.Program_Render (Fragment_Shader).Value.Uniform ("camera_pos").Set_Vector (CP);
      Object.Terrain.Program_Render (Fragment_Shader).Value.Uniform ("earth_radius").Set_Single (Object.Planet_Radius);
      Object.Terrain.Program_Render (Fragment_Shader).Value.Uniform ("sun_direction").Set_Vector (Star);
   end Set_Data;

   overriding procedure Run (Object : Terrain_Program_Callback) is
      use all type Rendering.Buffers.Indexed_Buffer_Target;
   begin
      Object.Data.Spheres.Bind (Shader_Storage, Binding_Buffer_Spheres);
      Object.Data.Terrain.Render;
   end Run;

   function Create_Graph
     (Object       : Terrain_Planet;
      Color, Depth : Orka.Rendering.Textures.Texture_Description) return Orka.Frame_Graphs.Frame_Graph is
   begin
      return Object.Terrain.Create_Graph (Color, Depth, Object.Callback'Unchecked_Access);
   end Create_Graph;

end Orka.Features.Terrain.Planets;
