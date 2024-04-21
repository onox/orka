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
with Orka.Rendering.States;

package body Orka.Features.Terrain is

   Maximum_Nodes : constant := 2 ** 20;
   --  4 MiB of LEB nodes

   --  Textures
   --  (Lower bindings points used for atmosphere textures)
   Binding_Texture_DMap : constant := 4;

   --  SSBOs
   Binding_Buffer_Leb              : constant := 0;
   Binding_Buffer_Leb_Nodes        : constant := 1;
   Binding_Buffer_Leb_Node_Counter : constant := 2;
   Binding_Buffer_Transforms       : constant := 3;
   --  Binding point 4 is used by a buffer storing spheroid parameters in child package Planets
   Binding_Buffer_Draw             : constant := 5;
   Binding_Buffer_Dispatch         : constant := 6;
   Binding_Buffer_Counted_Nodes    : constant := 7;

   --  UBOs
   Binding_Buffer_Matrices : constant := 0;
   Binding_Buffer_Metadata : constant := 1;

   Shader_Leb                    : aliased constant String := "orka-terrain:leb.comp";
   Shader_Leb_Init               : aliased constant String := "orka-terrain:leb-init.comp";

   Shader_Leb_Sum_Reduction_Prepass : aliased constant String := "orka-terrain:leb-sum-reduction-prepass.comp";
   Shader_Leb_Sum_Reduction         : aliased constant String := "orka-terrain:leb-sum-reduction.comp";
   Shader_Terrain_Prepare_Indirect  : aliased constant String := "orka-terrain:terrain-prepare-indirect.comp";

   Shader_Terrain_Render_Common  : aliased constant String := "orka-terrain:terrain-render-common.glsl";
   Shader_Terrain_Render_Normals : aliased constant String := "orka-terrain:terrain-render-normals.frag";
   Shader_Terrain_Render_Sphere  : aliased constant String := "orka-terrain:terrain-render-sphere.glsl";
   Shader_Terrain_Render_Plane   : aliased constant String := "orka-terrain:terrain-render-plane.glsl";
   Shader_Terrain_Update_Lod     : aliased constant String := "orka-terrain:terrain-update-lod.comp";
   Shader_Terrain_Update         : aliased constant String := "orka-terrain:terrain-update.comp";

   Shader_Render_Vertex          : aliased constant String := "orka-terrain:terrain-render.vert";
   Shader_Render_Tess_Control    : aliased constant String := "orka-terrain:terrain-render.tesc";
   Shader_Render_Tess_Evaluation : aliased constant String := "orka-terrain:terrain-render.tese";
   Shader_Render_Wires_Geometry  : aliased constant String := "orka-terrain:terrain-render-wires.geom";
   Shader_Render_Wires_Fragment  : aliased constant String := "orka-terrain:terrain-render-wires.frag";
   Shader_Render_Fragment        : aliased constant String := "orka-terrain:terrain-render.frag";

   function Create_Terrain
     (Context              : aliased Orka.Contexts.Context'Class;
      Kind                 : Terrain_Kind;
      Count                : Positive;
      Min_Depth, Max_Depth : Subdivision_Depth;
      Wireframe            : Boolean;
      Render_Modules       : Rendering.Shaders.Modules.Shader_Module_Array;
      Initialize_Render    : access procedure (Shaders : Rendering.Shaders.Objects.Shader_Objects)) return Terrain
   is
      use Rendering.Buffers;
      use Rendering.Samplers;
      use Rendering.Fences;

      use GL.Types.Indirect;

      use all type Wrapping_Mode;
      use all type Minifying_Function;

      Number_Of_Buffers : constant Size := Size (Count);

      Draw_Commands     : constant Arrays_Indirect_Command_Array   :=
        [1 .. Number_Of_Buffers => (0, 0, 0, 0)];
      Dispatch_Commands : constant Dispatch_Indirect_Command_Array :=
        [1 .. Number_Of_Buffers => (2, 1, 1)];

      Nodes_Counter : constant Unsigned_32_Array := [1 .. Number_Of_Buffers => 0];

      Heap_Elements : constant Natural := 2 + 2 ** (Natural (Max_Depth) + 2 - 5);
      --  Minimum and maximum depth, and the the heap elements

      use Rendering.Shaders.Objects;
      use Rendering.Shaders.Modules;
      use all type Rendering.Shaders.Shader_Kind;

      Modules_Render_Fragment_Shader : constant Shader_Module_Array :=
        Create_Modules (Fragment_Shader,
          [Shader_Terrain_Render_Common'Access,
           Shader_Terrain_Render_Normals'Access,
           (if Wireframe then Shader_Render_Wires_Fragment'Access else Shader_Render_Fragment'Access)])
        & Render_Modules;
   begin
      return Result : Terrain :=
        (Context      => Context'Access,
         Count        => Count,
         Max_Depth    => Max_Depth,
         Wireframe    => Wireframe,
         Split_Update => True,
         Visible_Tiles => [others => True],

         Program_Leb_Update =>
           [Compute_Shader => Create_Shader_From_Files (Compute_Shader, Paths =>
              [Shader_Leb'Access,
               Shader_Terrain_Render_Common'Access,
               (case Kind is
                  when Sphere => Shader_Terrain_Render_Sphere'Access,
                  when Plane  => Shader_Terrain_Render_Plane'Access),
               Shader_Terrain_Update_Lod'Access,
               Shader_Terrain_Update'Access]),
            others         => Empty],

         Program_Render =>
           [Vertex_Shader          => Create_Shader_From_Files (Vertex_Shader,
              [Shader_Leb'Access,
               Shader_Terrain_Render_Common'Access,
               Shader_Render_Vertex'Access]),
            Tess_Control_Shader    => Create_Shader_From_Files (Tess_Control_Shader,
              [Shader_Leb'Access,
               Shader_Terrain_Render_Common'Access,
               (case Kind is
                  when Sphere => Shader_Terrain_Render_Sphere'Access,
                  when Plane => Shader_Terrain_Render_Plane'Access),
               Shader_Render_Tess_Control'Access]),
            Tess_Evaluation_Shader => Create_Shader_From_Files (Tess_Evaluation_Shader,
              [Shader_Terrain_Render_Common'Access,
               (case Kind is
                  when Sphere => Shader_Terrain_Render_Sphere'Access,
                  when Plane => Shader_Terrain_Render_Plane'Access),
               Shader_Render_Tess_Evaluation'Access]),
            Geometry_Shader        => Create_Shader_From_Files (Geometry_Shader,
              (if Wireframe then [Shader_Render_Wires_Geometry'Access] else [])),
            Fragment_Shader        => Create_Shader (Modules_Render_Fragment_Shader),
            others                 => Empty],

         Program_Leb_Prepass =>
           [Compute_Shader => Create_Shader_From_Files (Compute_Shader, [Shader_Leb'Access, Shader_Leb_Sum_Reduction_Prepass'Access]),
            others         => Empty],
         Program_Leb_Reduction =>
           [Compute_Shader => Create_Shader_From_Files (Compute_Shader, [Shader_Leb'Access, Shader_Leb_Sum_Reduction'Access]),
            others         => Empty],
         Program_Indirect =>
           [Compute_Shader => Create_Shader_From_Files (Compute_Shader, [Shader_Leb'Access, Shader_Terrain_Prepare_Indirect'Access]),
            others         => Empty],

         Sampler                 => Create_Sampler
           ((Wrapping         => [Clamp_To_Edge, Clamp_To_Edge, Repeat],
             Minifying_Filter => Linear_Mipmap_Linear,
             others           => <>)),
         Buffer_Leb              => [others => Create_Buffer ((others => False), Orka.Types.UInt_Type, Heap_Elements)],
         Buffer_Leb_Nodes        => [others => Create_Buffer ((others => False), Orka.Types.UInt_Type, Maximum_Nodes)],
         Buffer_Leb_Node_Counter => Create_Buffer ((others => False), Nodes_Counter),
         Buffer_Draw             => Create_Buffer ((others => False), Draw_Commands),
         Buffer_Dispatch         => Create_Buffer ((others => False), Dispatch_Commands),
         Buffer_Matrices         => Create_Buffer ((Dynamic_Storage => True, others => False), Orka.Types.Single_Matrix_Type, 3),
         Buffer_Metadata         => Create_Buffer ((Dynamic_Storage => True, others => False), Orka.Types.Single_Type, 4),
         Buffer_Counted_Nodes    => Mapped.Persistent.Create_Buffer
           (Orka.Types.UInt_Type, Integer (Number_Of_Buffers), Mapped.Read, Regions => Regions_Counted_Nodes),
         Fence_Counted_Nodes     => Create_Buffer_Fence (Regions => Regions_Counted_Nodes, Maximum_Wait => 0.01),
         others => <>)
      do
         Result.Uniform_Prepass_Pass_ID   := Result.Program_Leb_Prepass (Compute_Shader).Value.Uniform   ("u_PassID");
         Result.Uniform_Reduction_Pass_ID := Result.Program_Leb_Reduction (Compute_Shader).Value.Uniform ("u_PassID");

         Result.Uniform_Update_Freeze := Result.Program_Leb_Update (Compute_Shader).Value.Uniform ("u_Freeze");
         Result.Uniform_Update_Split  := Result.Program_Leb_Update (Compute_Shader).Value.Uniform ("u_Split");
         Result.Uniform_Update_Leb_ID := Result.Program_Leb_Update (Compute_Shader).Value.Uniform ("u_LebID");

         Result.Uniform_Indirect_Leb_ID := Result.Program_Indirect (Compute_Shader).Value.Uniform ("u_LebID");

         Result.Uniform_Render_Leb_ID_Tesc := Result.Program_Render (Tess_Control_Shader).Value.Uniform ("u_LebID");
         Result.Uniform_Render_Leb_ID_Tese := Result.Program_Render (Tess_Evaluation_Shader).Value.Uniform ("u_LebID");
         Result.Uniform_Render_Subdiv := Result.Program_Render (Tess_Control_Shader).Value.Uniform ("u_MeshletSubdivision");

         declare
            Program_Init : constant Shader_Objects :=
              [Compute_Shader => Create_Shader_From_Files (Compute_Shader, [Shader_Leb'Access, Shader_Leb_Init'Access]),
               others         => Empty];
         begin
            Program_Init (Compute_Shader).Value.Uniform ("u_MinDepth").Set_Int (Size (Min_Depth));
            Program_Init (Compute_Shader).Value.Uniform ("u_MaxDepth").Set_Int (Size (Max_Depth));

            Context.Bind_Shaders (Program_Init);

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

   procedure Update (Object : in out Terrain) is
   begin
      Object.Context.Bind_Shaders (Object.Program_Leb_Update);
      Object.Uniform_Update_Split.Set_Boolean (Object.Split_Update);
      Object.Split_Update := not Object.Split_Update;

      Object.Buffer_Dispatch.Bind (Dispatch_Indirect);
      Object.Buffer_Leb_Node_Counter.Bind (Shader_Storage, Binding_Buffer_Leb_Node_Counter);

      for ID in Object.Buffer_Leb'Range loop
         declare
            Offset : constant Size := Size (ID - 1);
         begin
            Object.Buffer_Leb (ID).Bind (Shader_Storage, Binding_Buffer_Leb);
            Object.Buffer_Leb_Nodes (ID).Bind (Shader_Storage, Binding_Buffer_Leb_Nodes);

            Object.Uniform_Update_Leb_ID.Set_Int (Offset);
            GL.Compute.Dispatch_Compute_Indirect (Offset => Offset);
         end;
      end loop;
      GL.Barriers.Memory_Barrier ((Shader_Storage => True, others => False));
   end Update;

   procedure Reduce (Object : in out Terrain) is
      Depth : Integer := Integer (Object.Max_Depth);

      Count     : constant Integer := 2 ** Depth / 2 ** 5;
      Num_Group : constant Integer := (if Count >= 256 then Count / 256 else 1);
   begin
      --  Reduction prepass
      Object.Context.Bind_Shaders (Object.Program_Leb_Prepass);
      Object.Uniform_Prepass_Pass_ID.Set_Int (Integer_32 (Depth));

      for ID in Object.Buffer_Leb'Range loop
         if Object.Visible_Tiles (ID) then
            Object.Buffer_Leb (ID).Bind (Shader_Storage, Binding_Buffer_Leb);
            GL.Compute.Dispatch_Compute (X => Unsigned_32 (Num_Group));
         end if;
      end loop;
      GL.Barriers.Memory_Barrier ((Shader_Storage => True, others => False));

      --  Reduction
      Depth := Depth - 5;
      Object.Context.Bind_Shaders (Object.Program_Leb_Reduction);

      while Depth > 0 loop
         Depth := Depth - 1;

         declare
            Count     : constant Integer := 2 ** Depth;
            Num_Group : constant Integer := (if Count >= 256 then Count / 256 else 1);
         begin
            Object.Uniform_Reduction_Pass_ID.Set_Int (Integer_32 (Depth));
            for ID in Object.Buffer_Leb'Range loop
               if Object.Visible_Tiles (ID) then
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
      Object.Context.Bind_Shaders (Object.Program_Indirect);

      Object.Buffer_Leb_Node_Counter.Bind (Shader_Storage, Binding_Buffer_Leb_Node_Counter);
      Object.Buffer_Draw.Bind (Shader_Storage, Binding_Buffer_Draw);
      Object.Buffer_Dispatch.Bind (Shader_Storage, Binding_Buffer_Dispatch);

      Object.Buffer_Counted_Nodes.Bind (Shader_Storage, Binding_Buffer_Counted_Nodes);

      for ID in Object.Buffer_Leb'Range loop
         Object.Buffer_Leb (ID).Bind (Shader_Storage, Binding_Buffer_Leb);
         Object.Uniform_Indirect_Leb_ID.Set_Int (Size (ID - 1));
         GL.Compute.Dispatch_Compute (X => 1, Y => 1, Z => 1);
      end loop;
      GL.Barriers.Memory_Barrier ((By_Region => False, Shader_Storage | Command => True, others => False));
   end Prepare_Indirect;

   procedure Set_Data
     (Object        : in out Terrain;
      Transforms    : Rendering.Buffers.Buffer;
      Height_Map    : Rendering.Textures.Texture;
      Height_Scale  : Float_32;
      Height_Offset : Float_32) is
   begin
      Object.Transforms := Transforms;
      Object.Height_Map := Height_Map;

      Object.Height_Scale := Height_Scale;
      Object.Height_Offset := Height_Offset;
   end Set_Data;

   procedure Set_Data
     (Object        : in out Terrain;
      Rotation      : Types.Singles.Matrix4;
      Center        : Types.Singles.Matrix4;
      Camera        : Cameras.Camera_Ptr;
      Parameters    : Subdivision_Parameters;
      Freeze, Wires : Boolean)
   is
      package EF is new Ada.Numerics.Generic_Elementary_Functions (Float_32);

      Subdivision  : constant Size := Size (Parameters.Meshlet_Subdivision);

      Meshlet_Subdivision : constant := 0;

      LoD_Variance : constant Float_32 :=
        (if Object.Height_Scale > 0.0 then
           (Float_32 (Parameters.Min_LoD_Standard_Dev) / 64.0 / Object.Height_Scale) ** 2
         else 0.0);
      LoD_Factor   : constant Float_32 :=
        -2.0 * EF.Log (2.0 * EF.Tan (Camera.Lens.FOV / 2.0)
          / Float_32 (Camera.Lens.Height) * 2.0 ** Meshlet_Subdivision
          * Float_32 (Parameters.Edge_Length_Target), 2.0) + 2.0;
      --  For perspective projection

      use Cameras.Transforms;
      use all type Rendering.Shaders.Shader_Kind;
   begin
      Object.Uniform_Update_Freeze.Set_Boolean (Freeze);
      Object.Freeze := Freeze;

      Object.Uniform_Render_Subdiv.Set_Int (Subdivision);

      if Object.Wireframe then
         Object.Program_Render (Geometry_Shader).Value.Uniform ("u_ScreenResolution").Set_Vector
           (Types.Singles.Vector4'
             (Float_32 (Camera.Lens.Width), Float_32 (Camera.Lens.Height), 0.0, 0.0));
         Object.Program_Render (Fragment_Shader).Value.Uniform ("u_ShowWires").Set_Boolean (Wires);
      end if;

      Object.Buffer_Matrices.Set_Data (Orka.Types.Singles.Matrix4_Array'
        (Rotation, Camera.View_Matrix * (Center * Rotation), Camera.Projection_Matrix));

      Object.Buffer_Metadata.Set_Data (Float_32_Array'(Object.Height_Scale, Object.Height_Offset, LoD_Variance, LoD_Factor));
   end Set_Data;

   procedure Render (Object : in out Terrain) is
      use all type GL.Types.Connection_Mode;
   begin
      Object.Sampler.Bind (Binding_Texture_DMap);

      --  Textures
      Object.Height_Map.Bind (Binding_Texture_DMap);

      --  UBOs
      Object.Buffer_Matrices.Bind (Uniform, Binding_Buffer_Matrices);
      Object.Buffer_Metadata.Bind (Uniform, Binding_Buffer_Metadata);

      --  SSBOs
      Object.Transforms.Bind (Shader_Storage, Binding_Buffer_Transforms);

      Object.Update;
      Object.Reduce;

      declare
         Status : Rendering.Fences.Fence_Status;
      begin
         Object.Fence_Counted_Nodes.Prepare_Index (Status);
      end;

      if not Object.Freeze then
         declare
            Data : Unsigned_32_Array (1 .. Integer_32 (Object.Count));
         begin
            GL.Barriers.Memory_Barrier ((By_Region => False, Buffer_Update => True, others => False));
            Object.Buffer_Counted_Nodes.Read_Data (Data);

            for Index in Data'Range loop
               Object.Visible_Tiles (Integer (Index)) := Data (Index) > 0;
            end loop;
         end;
      end if;

      Object.Prepare_Indirect;

      Object.Buffer_Counted_Nodes.Advance_Index;
      Object.Fence_Counted_Nodes.Advance_Index;

      Object.Context.Bind_Shaders (Object.Program_Render);

      for ID in Object.Buffer_Leb_Nodes'Range loop
         if Object.Visible_Tiles (ID) then
            Object.Buffer_Leb_Nodes (ID).Bind (Shader_Storage, Binding_Buffer_Leb_Nodes);
            Object.Uniform_Render_Leb_ID_Tesc.Set_Int (Size (ID - 1));
            Object.Uniform_Render_Leb_ID_Tese.Set_Int (Size (ID - 1));
            Orka.Rendering.Drawing.Draw_Indirect (Patches, Object.Buffer_Draw, ID - 1, 1);
         end if;
      end loop;
   end Render;

   function Create_Graph
     (Object       : Terrain;
      Color, Depth : Orka.Rendering.Textures.Texture_Description;
      Callback     : not null Orka.Frame_Graphs.Program_Callback_Access) return Orka.Frame_Graphs.Frame_Graph
   is
      use Orka.Frame_Graphs;

      Graph : aliased Orka.Frame_Graphs.Frame_Graph
        (Maximum_Passes    => 1,
         Maximum_Handles   => 2,
         Maximum_Resources => 4);

      State : constant Orka.Rendering.States.State := (others => <>);
      Pass  : Render_Pass'Class := Graph.Add_Pass ("terrain", State, Callback);

      Resource_Color_V1 : constant Orka.Frame_Graphs.Resource :=
        (Name        => +"terrain-color",
         Description => Color,
         others      => <>);

      Resource_Depth_V1 : constant Orka.Frame_Graphs.Resource :=
        (Name        => +"terrain-depth",
         Description => Depth,
         others      => <>);

      Resource_Color_V2 : constant Orka.Frame_Graphs.Resource :=
        Pass.Add_Input_Output (Resource_Color_V1, Framebuffer_Attachment, 0);
      Resource_Depth_V2 : constant Orka.Frame_Graphs.Resource :=
        Pass.Add_Input_Output (Resource_Depth_V1, Framebuffer_Attachment, 1);
   begin
      Graph.Import ([Resource_Color_V1, Resource_Depth_V1]);
      Graph.Export ([Resource_Color_V2, Resource_Depth_V2]);

      return Graph;
   end Create_Graph;

   package body Flat is

      overriding procedure Run (Object : Terrain_Program_Callback) is
      begin
         Object.Data.Terrain.Render;
      end Run;

      function Create_Terrain_Flat (Terrain : aliased in out Orka.Features.Terrain.Terrain) return Terrain_Flat is
      begin
         return (Terrain => Terrain'Access, others => <>);
      end Create_Terrain_Flat;

      function Create_Graph
        (Object       : Terrain_Flat;
         Color, Depth : Orka.Rendering.Textures.Texture_Description) return Orka.Frame_Graphs.Frame_Graph is
      begin
         return Object.Terrain.Create_Graph (Color, Depth, Object.Callback'Unchecked_Access);
      end Create_Graph;

   end Flat;

end Orka.Features.Terrain;
