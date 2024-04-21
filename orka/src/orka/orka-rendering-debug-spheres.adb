--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2019 onox <denkpadje@gmail.com>
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

with GL.Blending;
with GL.Rasterization;
with GL.Types;

with Orka.Rendering.Drawing;
with Orka.Rendering.States;

package body Orka.Rendering.Debug.Spheres is

   function Create_Sphere
     (Context : aliased Orka.Contexts.Context'Class;
      Color   : Transforms.Vector4 := [1.0, 1.0, 1.0, 1.0];
      Normals : Boolean := False;
      Cells_Horizontal : Positive := 36;
      Cells_Vertical   : Positive := 18) return Sphere
   is
      use Rendering.Shaders;
      use Rendering.Shaders.Objects;
   begin
      return Result : Sphere :=
        (Program => [Vertex_Shader   => Create_Shader (Vertex_Shader, "orka:debug/sphere.vert"),
                     Fragment_Shader => Create_Shader (Fragment_Shader, "orka:debug/sphere.frag"),
                     others          => Empty],
         Context => Context'Access,
         Cells_Horizontal => Cells_Horizontal,
         Cells_Vertical   => Cells_Vertical,
         others  => <>)
      do
         Result.Program (Fragment_Shader).Value.Uniform ("color").Set_Vector (Color);
         Result.Program (Fragment_Shader).Value.Uniform ("useNormal").Set_Boolean (Normals);

         Result.Program (Vertex_Shader).Value.Uniform ("cellsHorizontal").Set_Int (GL.Types.Int (Cells_Horizontal));
         Result.Program (Vertex_Shader).Value.Uniform ("cellsVertical").Set_Int (GL.Types.Int (Cells_Vertical));

         Result.Uniform_Visible  := Result.Program (Fragment_Shader).Value.Uniform ("visible");
         Result.Uniform_View    := Result.Program (Vertex_Shader).Value.Uniform ("view");
         Result.Uniform_Proj    := Result.Program (Vertex_Shader).Value.Uniform ("proj");
      end return;
   end Create_Sphere;

   procedure Set_Data
     (Object     : in out Sphere;
      View, Proj : Transforms.Matrix4;
      Transforms, Spheres : Rendering.Buffers.Buffer) is
   begin
      Object.Uniform_View.Set_Matrix (View);
      Object.Uniform_Proj.Set_Matrix (Proj);

      Object.Transforms := Transforms;
      Object.Spheres := Spheres;
   end Set_Data;

   procedure Render (Object : Sphere) is
      use all type Rendering.Buffers.Indexed_Buffer_Target;

      Vertex_Count : constant Positive :=
        (Object.Cells_Horizontal + 1) * Object.Cells_Vertical * 6 - 2;
   begin
      Object.Transforms.Bind (Shader_Storage, 0);
      Object.Spheres.Bind (Shader_Storage, 1);

      Orka.Rendering.Drawing.Draw (GL.Types.Triangle_Strip, 0,
        Vertex_Count, Instances => Object.Transforms.Length);
   end Render;

   overriding procedure Run (Object : Spheres_Hidden_Program_Callback) is
   begin
      Object.Data.Context.Bind_Shaders (Object.Data.Program);
      Object.Data.Uniform_Visible.Set_Boolean (False);
      Object.Data.Render;
   end Run;

   overriding procedure Run (Object : Spheres_Visible_Program_Callback) is
   begin
      Object.Data.Context.Bind_Shaders (Object.Data.Program);
      Object.Data.Uniform_Visible.Set_Boolean (True);
      Object.Data.Render;
   end Run;

   function Create_Graph
     (Object       : Sphere;
      Color, Depth : Orka.Rendering.Textures.Texture_Description) return Orka.Frame_Graphs.Frame_Graph
   is
      use Orka.Frame_Graphs;

      Graph : aliased Orka.Frame_Graphs.Frame_Graph
        (Maximum_Passes    => 2,   --  1 pass for hidden lines and 1 pass for visible lines
         Maximum_Handles   => 4,   --  Reading/Writing 2x color + depth buffers
         Maximum_Resources => 6);  --  3 resources for color buffer and 2 for depth buffer (+ 1 for implicit)
      --  RC1 --> PH -> RC2 -> PV --> RC3;
      --  RD1 -/           /     \--> RD3;
      --     \------------/

      use all type GL.Blending.Blend_Factor;

      --  Set up blending for the hidden lines
      State_Hidden : constant Orka.Rendering.States.State :=
        (Depth_Func      => GL.Types.LEqual,
         Polygon_Mode    => GL.Rasterization.Line,
         Blend_Functions => [others => (Src_Alpha, One_Minus_Src_Alpha, One, Zero)],
         Blending        => True,
         others          => <>);

      State_Visible : constant Orka.Rendering.States.State := (State_Hidden with delta Depth_Func => GL.Types.Greater);

      Pass_Hidden  : Render_Pass'Class := Graph.Add_Pass ("spheres-hidden", State_Hidden, Object.Callback_Hidden'Unchecked_Access);
      Pass_Visible : Render_Pass'Class := Graph.Add_Pass ("spheres-visible", State_Visible, Object.Callback_Visible'Unchecked_Access);

      Resource_Color_V1 : constant Orka.Frame_Graphs.Resource :=
        (Name        => +"spheres-color",
         Description => Color,
         others      => <>);

      Resource_Depth_V1 : constant Orka.Frame_Graphs.Resource :=
        (Name        => +"spheres-depth",
         Description => Depth,
         others      => <>);

      Resource_Color_V2 : constant Orka.Frame_Graphs.Resource :=
        Pass_Hidden.Add_Input_Output (Resource_Color_V1, Framebuffer_Attachment, 0);
   begin
      Pass_Hidden.Add_Input (Resource_Depth_V1, Framebuffer_Attachment, 1);

      declare
         Resource_Color_V3 : constant Orka.Frame_Graphs.Resource :=
           Pass_Visible.Add_Input_Output (Resource_Color_V2, Framebuffer_Attachment, 0);
         Resource_Depth_V3 : constant Orka.Frame_Graphs.Resource :=
           Pass_Visible.Add_Input_Output (Resource_Depth_V1, Framebuffer_Attachment, 1);
      begin
         Graph.Import ([Resource_Color_V1, Resource_Depth_V1]);
         Graph.Export ([Resource_Color_V3, Resource_Depth_V3]);
      end;

      return Graph;
   end Create_Graph;

end Orka.Rendering.Debug.Spheres;
