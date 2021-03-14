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

with Ada.Numerics.Generic_Elementary_Functions;

with GL.Types;

with Orka.Transforms.Singles.Matrices;

package body Orka.Features.Terrain.Spheres is

   function Plane_To_Sphere
     (Vertex     : Orka.Transforms.Singles.Vectors.Vector4;
      Parameters : Orka.Features.Terrain.Spheroid_Parameters)
     return Orka.Transforms.Singles.Vectors.Vector4
   is
      package EF is new Ada.Numerics.Generic_Elementary_Functions (GL.Types.Single);

      use Orka.Transforms.Singles.Vectors;

      Axis   : GL.Types.Single renames Parameters (1);
      E2     : GL.Types.Single renames Parameters (2);
      Y_Mask : GL.Types.Single renames Parameters (3);
      Z_Mask : GL.Types.Single renames Parameters (4);

      Unit : Vector4 := Vertex * (2.0, 2.0, 1.0, 1.0) - (1.0, 1.0, 0.0, 0.0);
      --  Centers the plane
   begin
      --  World matrix assumes ECEF, meaning:
      --
      --    z
      --    |
      --    o--y
      --   /
      --  x
      --
      --  So the 3rd element (1.0) must be moved to the 'x' position,
      --  and the first two elements (H and V) must be moved to 'y' and 'z'.
      Unit := Normalize ((Unit (Z), Unit (X), Unit (Y), 0.0));
      Unit (W) := 1.0;

      declare
         Height : constant GL.Types.Single :=
           Length ((Unit (Y), Unit (Z), 0.0, 0.0) * (Y_Mask, Z_Mask, 0.0, 0.0));

         N : constant GL.Types.Single :=
           Axis / EF.Sqrt (1.0 - E2 * Height * Height);

         Scale : Vector4 := ((1.0, 1.0, 1.0, 1.0) - E2 * (0.0, Y_Mask, Z_Mask, 0.0)) * N;
      begin
         Scale (W) := 1.0;
         return Scale * Unit;
      end;
   end Plane_To_Sphere;

   function Get_Sphere_Visibilities
     (Parameters : Spheroid_Parameters;
      Front, Back, World, View : Orka.Types.Singles.Matrix4)
   return GL.Types.Single_Array is
      use Orka.Transforms.Singles.Matrices;

      World_View : constant Orka.Types.Singles.Matrix4 := View * World;

      Vertices : constant array (GL.Types.Size range 0 .. 3) of Orka.Types.Singles.Vector4 :=
        (Plane_To_Sphere ((0.0, 0.0, 1.0, 1.0), Parameters),
         Plane_To_Sphere ((1.0, 0.0, 1.0, 1.0), Parameters),
         Plane_To_Sphere ((0.0, 1.0, 1.0, 1.0), Parameters),
         Plane_To_Sphere ((1.0, 1.0, 1.0, 1.0), Parameters));

      Faces : constant array (GL.Types.Size range 0 .. 1) of Orka.Types.Singles.Matrix4 :=
        (Front, Back);

      Result : GL.Types.Single_Array (0 .. 7);

      use Orka.Transforms.Singles.Vectors;
      use all type GL.Types.Int;
   begin
      for Index in Result'Range loop
         declare
            V : Orka.Types.Singles.Vector4 renames Vertices (Index mod 4);
            F : Orka.Types.Singles.Matrix4 renames Faces (Index / 4);

            --  Vector pointing to vertex from camera or sphere
            V_From_C : constant Orka.Types.Singles.Vector4 := World_View * (F * V);
            V_From_S : constant Orka.Types.Singles.Vector4 := View * (F * V);
         begin
            Result (Index) := Dot (Normalize (V_From_C), Normalize (V_From_S));
         end;
      end loop;
      return Result;
   end Get_Sphere_Visibilities;

   -----------------------------------------------------------------------------

   subtype Tile_Index is Positive range 1 .. 6;
   subtype Vertex_Index is GL.Types.Size range 0 .. 7;
   type Edge_Index is range 1 .. 12;

   type Edge_Index_Array   is array (Positive range 1 .. 4) of Edge_Index;
   type Vertex_Index_Array is array (Positive range 1 .. 2) of Vertex_Index;
   type Tile_Index_Array   is array (Positive range <>)     of Tile_Index;

   type Tile_3_Array is array (Vertex_Index) of Tile_Index_Array (1 .. 3);
   type Tile_2_Array is array (Edge_Index)   of Tile_Index_Array (1 .. 2);

   type Edges_Array    is array (Tile_Index) of Edge_Index_Array;
   type Vertices_Array is array (Edge_Index) of Vertex_Index_Array;

   --  The three tiles that are visible when a particular
   --  vertex is visible
   Vertex_Buffer_Indices : constant Tile_3_Array :=
     (0 => (1, 4, 6),
      1 => (1, 2, 6),
      2 => (1, 4, 5),
      3 => (1, 2, 5),
      4 => (2, 3, 6),
      5 => (3, 4, 6),
      6 => (2, 3, 5),
      7 => (3, 4, 5));

   --  The vertices that form each edge
   Edge_Vertex_Indices : constant Vertices_Array :=
     (1 => (0, 2),
      2 => (1, 3),
      3 => (2, 3),
      4 => (0, 1),

      5 => (4, 6),
      6 => (5, 7),
      7 => (6, 7),
      8 => (4, 5),

      9 => (2, 7),
      10 => (3, 6),
      11 => (0, 5),
      12 => (1, 4));

   --  The two tiles to which each edge belongs
   Edge_Tiles_Indices : constant Tile_2_Array :=
     (1 => (1, 4),
      2 => (1, 2),
      3 => (1, 5),
      4 => (1, 6),

      5 => (3, 2),
      6 => (3, 4),
      7 => (3, 5),
      8 => (3, 6),

      9 => (4, 5),
      10 => (2, 5),
      11 => (4, 6),
      12 => (2, 6));

   --  The four edges of each tile
   Tile_Edge_Indices : constant Edges_Array :=
     (1 => (1, 2, 3, 4),
      2 => (2, 5, 10, 12),
      3 => (5, 6, 7, 8),
      4 => (1, 6, 9, 11),
      5 => (3, 7, 9, 10),
      6 => (4, 8, 11, 12));

   Threshold_A : constant := 0.55;
   Threshold_B : constant := 0.25;

   function Get_Visible_Tiles
     (Visibilities : GL.Types.Single_Array) return Visible_Tile_Array
   is
      Visible_Tile_Count : array (Tile_Index) of Natural := (others => 0);

      Vertex_Visible : Boolean := False;

      Result : Visible_Tile_Array := (Tile_Index => False);
   begin
      --  Heuristic 1: a tile is visible if it surrounds a vertex that is
      --  pointing towards the camera
      for Vertex in Vertex_Buffer_Indices'Range loop
         if Visibilities (Vertex) < 0.0 then
            for Tile of Vertex_Buffer_Indices (Vertex) loop
               Result (Tile) := True;
            end loop;
            Vertex_Visible := True;
         end if;
      end loop;

      --  If all vertices point away from the camera, the camera is usually
      --  close to some of the tiles
      if not Vertex_Visible then
         --  Heuristic 2: an edge is visible if the maximum vertex visibility
         --  is less than some threshold
         for Edge in Edge_Vertex_Indices'Range loop
            if (for all Vertex of Edge_Vertex_Indices (Edge) =>
                  Visibilities (Vertex) < Threshold_A)
            then
               for Tile of Edge_Tiles_Indices (Edge) loop
                  Visible_Tile_Count (Tile) := Visible_Tile_Count (Tile) + 1;
               end loop;
            end if;
         end loop;

         declare
            Max_Count : Natural := 0;

            function Average_Visibility (Vertices : Vertex_Index_Array) return GL.Types.Single is
               Sum : GL.Types.Single := 0.0;
            begin
               for Vertex of Vertices loop
                  Sum := Sum + Visibilities (Vertex);
               end loop;

               return Sum / GL.Types.Single (Vertices'Length);
            end Average_Visibility;
         begin
            for Tile in Visible_Tile_Count'Range loop
               Max_Count := Natural'Max (Max_Count, Visible_Tile_Count (Tile));
            end loop;

            --  A tile is visible if it has the highest number (1, 2, or 4)
            --  of visible edges
            --
            --  For example, tile 1 might have a count of 4, while its surrounding
            --  tiles (2, 4, 5, and 6) have a count of 1. In that case choose to
            --  display tile 1.
            for Tile in Visible_Tile_Count'Range loop
               if Visible_Tile_Count (Tile) = Max_Count then
                  Result (Tile) := True;
               end if;
            end loop;

            --  Sometimes the camera might be positioned above a tile with count 4
            --  and looking at some of its edges. In that case we should render the
            --  adjacent tiles as well if those tiles are 'likely' to be visible.
            if Max_Count in 2 | 4 then
               for Tile in Tile_Edge_Indices'Range loop
                  if Result (Tile) then
                     --  Heuristic 3: all tiles that surround an edge of a visible tile
                     --  with an average vertex visibility less than some threshold
                     --  are visible as well
                     for Edge of Tile_Edge_Indices (Tile) loop
                        if Average_Visibility (Edge_Vertex_Indices (Edge)) < Threshold_B then
                           for Tile of Edge_Tiles_Indices (Edge) loop
                              Result (Tile) := True;
                           end loop;
                        end if;
                     end loop;
                  end if;
               end loop;
            end if;
         end;
      end if;

      return Result;
   end Get_Visible_Tiles;

end Orka.Features.Terrain.Spheres;
