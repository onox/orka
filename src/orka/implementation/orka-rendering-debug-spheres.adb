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
with GL.Buffers;
with GL.Rasterization;
with GL.Toggles;
with GL.Types;

with Orka.Rendering.Drawing;
with Orka.Rendering.Programs.Modules;

package body Orka.Rendering.Debug.Spheres is

   function Create_Sphere
     (Location : Resources.Locations.Location_Ptr;
      Color    : Transforms.Vector4 := (1.0, 1.0, 1.0, 1.0);
      Normals  : Boolean := False;
      Cells_Horizontal : Positive := 36;
      Cells_Vertical   : Positive := 18) return Sphere
   is
      use Rendering.Programs;
   begin
      return Result : Sphere :=
        (Program => Create_Program (Modules.Create_Module
                      (Location, VS => "sphere.vert", FS => "sphere.frag")),
         Cells_Horizontal => Cells_Horizontal,
         Cells_Vertical   => Cells_Vertical,
         others  => <>)
      do
         Result.Program.Uniform ("color").Set_Vector (Color);
         Result.Program.Uniform ("useNormal").Set_Boolean (Normals);

         Result.Program.Uniform ("cellsHorizontal").Set_Int (GL.Types.Int (Cells_Horizontal));
         Result.Program.Uniform ("cellsVertical").Set_Int (GL.Types.Int (Cells_Vertical));

         Result.Uniform_Visible  := Result.Program.Uniform ("visible");
         Result.Uniform_View     := Result.Program.Uniform ("view");
         Result.Uniform_Proj     := Result.Program.Uniform ("proj");
      end return;
   end Create_Sphere;

   procedure Render
     (Object     : in out Sphere;
      View, Proj : Transforms.Matrix4;
      Transforms, Sizes : Rendering.Buffers.Bindable_Buffer'Class)
   is
      use all type GL.Blending.Blend_Factor;
      use all type GL.Blending.Equation;

      use all type GL.Types.Compare_Function;
      use all type Rendering.Buffers.Buffer_Target;

      Reverse_Function : constant array (GL.Types.Compare_Function) of GL.Types.Compare_Function :=
        (Never     => Always,
         Always    => Never,
         Less      => GEqual,
         Greater   => LEqual,
         LEqual    => Greater,
         GEqual    => Less,
         Equal     => Not_Equal,
         Not_Equal => Equal);

      Original_Function : constant GL.Types.Compare_Function := GL.Buffers.Depth_Function;
      Original_Mask     : constant Boolean                   := GL.Buffers.Depth_Mask;

      Original_Polygon_Mode : constant GL.Rasterization.Polygon_Mode_Type
        := GL.Rasterization.Polygon_Mode;

      Original_Blend_Func  : constant GL.Blending.Blend_Factors   := GL.Blending.Blend_Func;
      Original_Blend_Eq    : constant GL.Blending.Blend_Equations := GL.Blending.Blend_Equation;
      Original_Blend_State : constant GL.Toggles.Toggle_State
        := GL.Toggles.State (GL.Toggles.Blend, 0);

      Vertex_Count : constant Positive :=
        (Object.Cells_Horizontal + 1) * Object.Cells_Vertical * 6 - 2;
   begin
      Object.Uniform_View.Set_Matrix (View);
      Object.Uniform_Proj.Set_Matrix (Proj);

      Object.Program.Use_Program;

      Transforms.Bind_Base (Shader_Storage, 0);
      Sizes.Bind_Base (Shader_Storage, 1);

      GL.Rasterization.Set_Polygon_Mode (GL.Rasterization.Line);

      --  Set up blending for the hidden lines
      GL.Blending.Set_Blend_Func ((Src_Alpha, One_Minus_Src_Alpha, One, Zero));
      GL.Blending.Set_Blend_Equation ((Func_Add, Func_Add));
      GL.Toggles.Enable (GL.Toggles.Blend, 0);

      --  Hidden lines of sphere
      GL.Buffers.Set_Depth_Function (Reverse_Function (Original_Function));
      GL.Buffers.Set_Depth_Mask (Enabled => False);

      Object.Uniform_Visible.Set_Boolean (False);
      Orka.Rendering.Drawing.Draw (GL.Types.Triangle_Strip, 0,
        Vertex_Count, Instances => Transforms.Length);

      GL.Buffers.Set_Depth_Function (Original_Function);
      GL.Buffers.Set_Depth_Mask (Enabled => Original_Mask);

      --  Visible lines of sphere 
      Object.Uniform_Visible.Set_Boolean (True);
      Orka.Rendering.Drawing.Draw (GL.Types.Triangle_Strip, 0,
        Vertex_Count, Instances => Transforms.Length);

      GL.Blending.Set_Blend_Func (Original_Blend_Func);
      GL.Blending.Set_Blend_Equation (Original_Blend_Eq);
      GL.Toggles.Set (GL.Toggles.Blend, 0, Original_Blend_State);

      GL.Rasterization.Set_Polygon_Mode (Original_Polygon_Mode);
   end Render;

end Orka.Rendering.Debug.Spheres;
