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
--  See package spec for a list of contributions.
--
--  [1] https://github.com/jdupuy/LongestEdgeBisectionDemos

with Ada.Numerics.Generic_Elementary_Functions;

with GL.Barriers;
with GL.Compute;
with GL.Types.Indirect;

with Orka.Rendering.Drawing;
with Orka.Rendering.Textures;

package body Orka.Features.Terrain is

   Maximum_Nodes : constant := 2 ** 20;
   --  4 MiB of LEB nodes

   --  Textures
   --  (Lower bindings points used for atmosphere textures)
   Binding_Texture_DMap : constant := 4;
   Binding_Texture_SMap : constant := 5;

   --  SSBOs
   Binding_Buffer_Leb              : constant := 0;
   Binding_Buffer_Leb_Nodes        : constant := 1;
   Binding_Buffer_Leb_Node_Counter : constant := 2;
   Binding_Buffer_Transform        : constant := 3;
   Binding_Buffer_Spheres          : constant := 4;
   Binding_Buffer_Draw             : constant := 5;
   Binding_Buffer_Dispatch         : constant := 6;

   --  UBOs
   Binding_Buffer_Matrices : constant := 0;

   --  Used for displacement and slope map textures
   function Create_Sampler return GL.Objects.Samplers.Sampler is
      Result : GL.Objects.Samplers.Sampler;

      use all type GL.Objects.Samplers.Wrapping_Mode;
      use all type GL.Objects.Samplers.Minifying_Function;
   begin
      Result.Set_X_Wrapping (Clamp_To_Edge);
      Result.Set_Y_Wrapping (Clamp_To_Edge);

      Result.Set_Minifying_Filter (Linear_Mipmap_Linear);

      return Result;
   end Create_Sampler;

   function Create_Terrain
     (Count                : Positive;
      Min_Depth, Max_Depth : Subdivision_Depth;
      Scale                : Height_Scale;
      Wireframe            : Boolean;
      Location             : Resources.Locations.Location_Ptr;
      Render_Modules       : Rendering.Programs.Modules.Module_Array;
      Initialize_Render    : access procedure
        (Program : Rendering.Programs.Program)) return Terrain
   is
      use Rendering.Buffers;
      use Rendering.Programs;

      use GL.Types.Indirect;

      Number_Of_Buffers : constant Size := Size (Count);

      Draw_Commands     : constant Arrays_Indirect_Command_Array   :=
        (1 .. Number_Of_Buffers => (0, 0, 0, 0));
      Dispatch_Commands : constant Dispatch_Indirect_Command_Array :=
        (1 .. Number_Of_Buffers => (2, 1, 1));

      Nodes_Counter : constant Unsigned_32_Array := (1 .. Number_Of_Buffers => 0);

      Heap_Elements : constant Natural := 2 + 2 ** (Natural (Max_Depth) + 2 - 5);
      --  Minimum and maximum depth, and the the heap elements

      Module_LEB : constant Modules.Module :=
        Modules.Create_Module (Location, CS => "terrain/leb.comp");

      use Rendering.Programs.Modules;
   begin
      return Result : Terrain :=
        (Count        => Count,
         Max_Depth    => Max_Depth,
         Scale        => Scale,
         Wireframe    => Wireframe,
         Split_Update => True,
         Program_Leb_Update    => Create_Program (Modules.Module_Array'
           (Module_LEB,
            Modules.Create_Module (Location, CS => "terrain/terrain-render-common.glsl"),
            Modules.Create_Module (Location, CS => "terrain/terrain-render-sphere.glsl"),
            Modules.Create_Module (Location, CS => "terrain/terrain-update-lod.comp"),
            Modules.Create_Module (Location, CS => "terrain/terrain-update.comp"))),
         Program_Render        => Create_Program (Modules.Module_Array'(Render_Modules &
           (Modules.Create_Module (Location, VS => "terrain/leb.comp"),
            Modules.Create_Module (Location, VS => "terrain/terrain-render-common.glsl"),
            Modules.Create_Module (Location, VS => "terrain/terrain-render-sphere.glsl"),
            (if Wireframe then
               Modules.Create_Module (Location,
                 VS => "terrain/terrain-render.vert",
                 GS => "terrain/terrain-render-wires.geom",
                 FS => "terrain/terrain-render-wires.frag")
             else
               Modules.Create_Module (Location,
                 VS => "terrain/terrain-render.vert",
                 FS => "terrain/terrain-render.frag")
            )))),
         Program_Leb_Prepass   => Create_Program (Modules.Module_Array'
           (Module_LEB,
            Modules.Create_Module (Location, CS => "terrain/leb-sum-reduction-prepass.comp"))),
         Program_Leb_Reduction => Create_Program (Modules.Module_Array'
           (Module_LEB,
            Modules.Create_Module (Location, CS => "terrain/leb-sum-reduction.comp"))),
         Program_Indirect      => Create_Program (Modules.Module_Array'
           (Module_LEB,
            Modules.Create_Module (Location, CS => "terrain/terrain-prepare-indirect.comp"))),
         Sampler                 => Create_Sampler,
         Buffer_Leb              => (others => Create_Buffer
           ((others => False), Orka.Types.UInt_Type, Heap_Elements)),
         Buffer_Leb_Nodes        => (others => Create_Buffer
           ((others => False), Orka.Types.UInt_Type, Maximum_Nodes)),
         Buffer_Leb_Node_Counter => Create_Buffer ((others => False), Nodes_Counter),
         Buffer_Draw             => Create_Buffer ((others => False), Draw_Commands),
         Buffer_Dispatch         => Create_Buffer ((others => False), Dispatch_Commands),
         Buffer_Matrices         => Create_Buffer
           ((Dynamic_Storage => True, others => False), Orka.Types.Single_Matrix_Type, 2),
         others => <>)
      do
         Result.Uniform_Prepass_Pass_ID   := Result.Program_Leb_Prepass.Uniform   ("u_PassID");
         Result.Uniform_Reduction_Pass_ID := Result.Program_Leb_Reduction.Uniform ("u_PassID");

         Result.Uniform_Update_Freeze := Result.Program_Leb_Update.Uniform ("u_Freeze");
         Result.Uniform_Update_Split  := Result.Program_Leb_Update.Uniform ("u_Split");
         Result.Uniform_Update_Leb_ID := Result.Program_Leb_Update.Uniform ("u_LebID");

         Result.Uniform_Update_LoD_Var    := Result.Program_Leb_Update.Uniform ("u_MinLodVariance");
         Result.Uniform_Update_LoD_Factor := Result.Program_Leb_Update.Uniform ("u_LodFactor");

         Result.Uniform_Update_DMap_Factor := Result.Program_Leb_Update.Uniform ("u_DmapFactor");

         Result.Uniform_Indirect_Leb_ID := Result.Program_Indirect.Uniform ("u_LebID");
         Result.Uniform_Indirect_Subdiv := Result.Program_Indirect.Uniform ("u_MeshletSubdivision");

         Result.Uniform_Render_Leb_ID := Result.Program_Render.Uniform ("u_LebID");
         Result.Uniform_Render_Subdiv := Result.Program_Render.Uniform ("u_MeshletSubdivision");
         Result.Uniform_Render_DMap_Factor := Result.Program_Render.Uniform ("u_DmapFactor");

         declare
            Program_Init : Program := Create_Program (Modules.Module_Array'
              (Module_LEB,
               Modules.Create_Module (Location, CS => "terrain/leb-init.comp")));
         begin
            Program_Init.Uniform ("u_MinDepth").Set_Int (Size (Min_Depth));
            Program_Init.Uniform ("u_MaxDepth").Set_Int (Size (Max_Depth));

            Program_Init.Use_Program;

            for ID in Result.Buffer_Leb'Range loop
               Result.Buffer_Leb (ID).Bind (Shader_Storage, Binding_Buffer_Leb);
               GL.Compute.Dispatch_Compute (X => 1, Y => 1, Z => 1);
            end loop;

            GL.Barriers.Memory_Barrier ((Shader_Storage => True, others => False));
         end;

         if Initialize_Render /= null then
            Initialize_Render (Result.Program_Render);
         end if;
      end return;
   end Create_Terrain;

   use all type Rendering.Buffers.Buffer_Target;
   use all type Rendering.Buffers.Indexed_Buffer_Target;

   procedure Update
     (Object  : in out Terrain;
      Visible : Visible_Tile_Array) is
   begin
      Object.Program_Leb_Update.Use_Program;
      Object.Uniform_Update_Split.Set_Boolean (Object.Split_Update);
      Object.Split_Update := not Object.Split_Update;

      Object.Buffer_Dispatch.Bind (Dispatch_Indirect);
      Object.Buffer_Leb_Node_Counter.Bind (Shader_Storage, Binding_Buffer_Leb_Node_Counter);
      for ID in Object.Buffer_Leb'Range loop
         declare
            Offset : constant Size := Size (ID - 1);
         begin
            if Visible (ID) then
               Object.Buffer_Leb (ID).Bind (Shader_Storage, Binding_Buffer_Leb);
               Object.Buffer_Leb_Nodes (ID).Bind (Shader_Storage, Binding_Buffer_Leb_Nodes);

               Object.Uniform_Update_Leb_ID.Set_Int (Offset);
               GL.Compute.Dispatch_Compute_Indirect (Offset => Offset);
            end if;
         end;
      end loop;
      GL.Barriers.Memory_Barrier ((Shader_Storage => True, others => False));
   end Update;

   procedure Reduce
     (Object  : in out Terrain;
      Visible : Visible_Tile_Array)
   is
      Depth : Integer := Integer (Object.Max_Depth);

      Count     : constant Integer := 2 ** Depth / 2 ** 5;
      Num_Group : constant Integer := (if Count >= 256 then Count / 2 ** 8 else 1);
   begin
      --  Reduction prepass
      Object.Program_Leb_Prepass.Use_Program;
      Object.Uniform_Prepass_Pass_ID.Set_Int (Integer_32 (Depth));

      for ID in Object.Buffer_Leb'Range loop
         if Visible (ID) then
            Object.Buffer_Leb (ID).Bind (Shader_Storage, Binding_Buffer_Leb);
            GL.Compute.Dispatch_Compute (X => Unsigned_32 (Num_Group));
         end if;
      end loop;
      GL.Barriers.Memory_Barrier ((Shader_Storage => True, others => False));

      --  Reduction
      Depth := Depth - 5;
      Object.Program_Leb_Reduction.Use_Program;

      while Depth > 0 loop
         Depth := Depth - 1;

         declare
            Count     : constant Integer := 2 ** Depth;
            Num_Group : constant Integer := (if Count >= 256 then Count / 2 ** 8 else 1);
         begin
            Object.Uniform_Reduction_Pass_ID.Set_Int (Integer_32 (Depth));
            for ID in Object.Buffer_Leb'Range loop
               if Visible (ID) then
                  Object.Buffer_Leb (ID).Bind (Shader_Storage, Binding_Buffer_Leb);
                  GL.Compute.Dispatch_Compute (X => Unsigned_32 (Num_Group));
               end if;
            end loop;
            GL.Barriers.Memory_Barrier ((Shader_Storage => True, others => False));
         end;
      end loop;
   end Reduce;

   procedure Prepare_Indirect (Object : in out Terrain) is
   begin
      Object.Program_Indirect.Use_Program;

      Object.Buffer_Leb_Node_Counter.Bind (Shader_Storage, Binding_Buffer_Leb_Node_Counter);
      Object.Buffer_Draw.Bind (Shader_Storage, Binding_Buffer_Draw);
      Object.Buffer_Dispatch.Bind (Shader_Storage, Binding_Buffer_Dispatch);

      for ID in Object.Buffer_Leb'Range loop
         Object.Buffer_Leb (ID).Bind (Shader_Storage, Binding_Buffer_Leb);
         Object.Uniform_Indirect_Leb_ID.Set_Int (Size (ID - 1));
         GL.Compute.Dispatch_Compute (X => 1, Y => 1, Z => 1);
      end loop;
      GL.Barriers.Memory_Barrier ((By_Region => False, others => True));
   end Prepare_Indirect;

   procedure Render
     (Object        : in out Terrain;
      Transforms, Spheres : Rendering.Buffers.Bindable_Buffer'Class;
      Center        : Cameras.Transforms.Matrix4;
      Camera        : Cameras.Camera_Ptr;
      Parameters    : Subdivision_Parameters;
      Visible_Tiles : Visible_Tile_Array;
      Update_Render : access procedure
        (Program : Rendering.Programs.Program);
      Height_Map    : GL.Objects.Textures.Texture;
      Freeze, Wires : Boolean;
      Timer_Update, Timer_Render : in out Orka.Timers.Timer)
   is
      package EF is new Ada.Numerics.Generic_Elementary_Functions (Float_32);
      use all type GL.Types.Connection_Mode;

      Subdivision  : constant Size     := Size (Parameters.Meshlet_Subdivision);
      Height_Scale : constant Float_32 := Float_32 (Object.Scale);

      Meshlet_Subdivision : constant := 0;

      LoD_Variance : constant Float_32 :=
        (if Height_Scale > 0.0 then
           (Float_32 (Parameters.Min_LoD_Standard_Dev) / 64.0 / Height_Scale) ** 2
         else 0.0);
      LoD_Factor   : constant Float_32 :=
        -2.0 * EF.Log (2.0 * EF.Tan (Camera.Lens.FOV / 2.0)
          / Float_32 (Camera.Lens.Height) * 2.0 ** Meshlet_Subdivision
          * Float_32 (Parameters.Edge_Length_Target), 2.0) + 2.0;
      --  For perspective projection

      use Cameras.Transforms;
   begin
      Timer_Update.Start;

      Object.Uniform_Update_LoD_Var.Set_Single (LoD_Variance);
      Object.Uniform_Update_LoD_Factor.Set_Single (LoD_Factor);

      Object.Uniform_Update_Freeze.Set_Boolean (Freeze);

      Object.Uniform_Update_DMap_Factor.Set_Single (Height_Scale);
      Object.Uniform_Render_DMap_Factor.Set_Single (Height_Scale);

      Object.Uniform_Indirect_Subdiv.Set_Int (Subdivision);
      Object.Uniform_Render_Subdiv.Set_Int (Subdivision);

      if Object.Wireframe then
         Object.Program_Render.Uniform ("u_ScreenResolution").Set_Vector
           (Types.Singles.Vector4'
             (Float_32 (Camera.Lens.Width), Float_32 (Camera.Lens.Height), 0.0, 0.0));
         Object.Program_Render.Uniform ("u_ShowWires").Set_Boolean (Wires);
      end if;

      Object.Sampler.Bind (Binding_Texture_DMap);
      Object.Sampler.Bind (Binding_Texture_SMap);

      --  Textures
      Orka.Rendering.Textures.Bind
        (Height_Map, Orka.Rendering.Textures.Texture, Binding_Texture_DMap);

      --  UBOs
      Object.Buffer_Matrices.Set_Data (Orka.Types.Singles.Matrix4_Array'
        (Camera.View_Matrix * Center, Camera.Projection_Matrix));
      Object.Buffer_Matrices.Bind (Uniform, Binding_Buffer_Matrices);

      --  SSBOs
      Transforms.Bind (Shader_Storage, Binding_Buffer_Transform);
      Spheres.Bind (Shader_Storage, Binding_Buffer_Spheres);

      Object.Update (Visible_Tiles);
      Object.Reduce (Visible_Tiles);
      Object.Prepare_Indirect;

      Timer_Update.Stop;
      Timer_Render.Start;

      Object.Program_Render.Use_Program;
      if Update_Render /= null then
         Update_Render (Object.Program_Render);
      end if;

      for ID in Object.Buffer_Leb_Nodes'Range loop
         if Visible_Tiles (ID) then
            Object.Buffer_Leb_Nodes (ID).Bind (Shader_Storage, Binding_Buffer_Leb_Nodes);
            Object.Uniform_Render_Leb_ID.Set_Int (Size (ID - 1));
            Orka.Rendering.Drawing.Draw_Indirect (Triangles, Object.Buffer_Draw, ID - 1, 1);
         end if;
      end loop;

      Timer_Render.Stop;
   end Render;

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
         return (A, E2) & (if Side then (0.0, 1.0) else (1.0, 1.0));
      end;
   end Get_Spheroid_Parameters;

end Orka.Features.Terrain;
