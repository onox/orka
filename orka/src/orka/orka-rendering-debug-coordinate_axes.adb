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

with GL.Types;

with Orka.Rendering.Drawing;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.States;

package body Orka.Rendering.Debug.Coordinate_Axes is

   function Create_Coordinate_Axes
     (Location : Resources.Locations.Location_Ptr) return Coordinate_Axes
   is
      use Rendering.Programs;
   begin
      return Result : Coordinate_Axes :=
        (Program => Create_Program (Modules.Create_Module
           (Location,
            VS => "debug/axes.vert",
            GS => "debug/axes.geom",
            FS => "debug/axes.frag")),
         others  => <>)
      do
         Result.Uniform_View := Result.Program.Uniform ("view");
         Result.Uniform_Proj := Result.Program.Uniform ("proj");
         Result.Uniform_Size := Result.Program.Uniform ("size");
         Result.Uniform_Axis := Result.Program.Uniform ("axisLength");
      end return;
   end Create_Coordinate_Axes;

   procedure Set_Data
     (Object        : in out Coordinate_Axes;
      Width, Height : Positive;
      Axis_Size     : Transforms.Vector4 := (4.0, 100.0, 16.0, 32.0);
      View, Proj    : Transforms.Matrix4;
      Transforms    : Rendering.Buffers.Buffer) is
   begin
      Object.Uniform_View.Set_Matrix (View);
      Object.Uniform_Proj.Set_Matrix (Proj);
      Object.Uniform_Size.Set_Vector (Unsigned_32_Array'(Unsigned_32 (Width), Unsigned_32 (Height)));
      Object.Uniform_Axis.Set_Vector (Axis_Size);

      Object.Transforms := Transforms;
   end Set_Data;

   overriding procedure Run (Object : Axes_Program_Callback; Program : Rendering.Programs.Program) is
      use all type Rendering.Buffers.Indexed_Buffer_Target;
   begin
      Object.Data.Transforms.Bind (Shader_Storage, 0);
      Orka.Rendering.Drawing.Draw (GL.Types.Lines, 0, 6, Instances => Object.Data.Transforms.Length);
   end Run;

   function Create_Graph
     (Object       : Coordinate_Axes;
      Color, Depth : Orka.Rendering.Textures.Texture_Description) return Orka.Frame_Graphs.Frame_Graph
   is
      use Orka.Frame_Graphs;

      Graph : aliased Orka.Frame_Graphs.Frame_Graph
        (Maximum_Passes    => 1,   --  1 pass for axes
         Maximum_Handles   => 2,   --  Reading/Writing 2x color + depth buffers
         Maximum_Resources => 4);  --  2 resources for color buffer and 1 for depth buffer (+ 1 for implicit)
      --  RC1 --> P --> RC2
      --  RD1 -/
      --       \------> [RD1]

      State : constant Orka.Rendering.States.State := (Depth_Func => GL.Types.Always, others => <>);
      Pass  : Render_Pass'Class := Graph.Add_Pass ("axes", State, Object.Program, Object.Callback'Unchecked_Access);

      Resource_Color_V1 : constant Orka.Frame_Graphs.Resource :=
        (Name        => +"axes-color",
         Description => Color,
         others      => <>);

      Resource_Depth_V1 : constant Orka.Frame_Graphs.Resource :=
        (Name        => +"axes-depth",
         Description => Depth,
         others      => <>);

      Resource_Color_V2 : constant Orka.Frame_Graphs.Resource :=
        Pass.Add_Input_Output (Resource_Color_V1, Framebuffer_Attachment, 0);
   begin
      Pass.Add_Input (Resource_Depth_V1, Framebuffer_Attachment, 1);

      Graph.Import ([Resource_Color_V1, Resource_Depth_V1]);
      Graph.Export ([Resource_Color_V2, Resource_Depth_V1]);

      return Graph;
   end Create_Graph;

end Orka.Rendering.Debug.Coordinate_Axes;
