--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2016 onox <denkpadje@gmail.com>
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

with Orka.Contexts;
with Orka.Rendering.Buffers;
with Orka.Rendering.Drawing;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Vertex_Formats;
with Orka.Resources.Locations.Directories;
with Orka.Types;
with Orka.Windows.GLFW;

procedure Orka_Test.Test_2_Two_Triangles is
   Context : constant Orka.Contexts.Context'Class
     := Orka.Windows.GLFW.Initialize (Major => 4, Minor => 2);
   pragma Unreferenced (Context);

   Window : aliased Orka.Windows.Window'Class
     := Orka.Windows.GLFW.Create_Window (Width => 500, Height => 500, Resizable => False);

   use GL.Types;
   use all type Orka.Types.Element_Type;

   use Orka.Rendering.Buffers;
   use Orka.Rendering.Framebuffers;
   use Orka.Rendering.Vertex_Formats;
   use Orka.Rendering.Programs;

   function Load_Mesh_1 (Program : Orka.Rendering.Programs.Program) return Vertex_Format is
      Vertices : constant Single_Array
        := (-0.3,  0.5, -1.0,
            -0.8, -0.5, -1.0,
             0.2, -0.5, -1.0);

      Color_Vertices : constant Single_Array
        := (1.0, 0.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 0.0, 1.0);

      --  Upload vertices and color data to VBO's
      VBO_1 : constant Buffer := Create_Buffer ((others => False), Vertices);
      VBO_2 : constant Buffer := Create_Buffer ((others => False), Color_Vertices);

      procedure Add_Position_Attribute (Buffer : in out Attribute_Buffer) is
      begin
         Buffer.Add_Attribute (Program.Attribute_Location ("in_Position"), 3);
         Buffer.Set_Buffer (VBO_1);
      end Add_Position_Attribute;

      procedure Add_Color_Attribute (Buffer : in out Attribute_Buffer) is
      begin
         Buffer.Add_Attribute (Program.Attribute_Location ("in_Color"), 3);
         Buffer.Set_Buffer (VBO_2);
      end Add_Color_Attribute;
   begin
      --  Create mesh and its attributes
      return Result : Vertex_Format := Create_Vertex_Format (UInt_Type) do
         Result.Add_Attribute_Buffer (Single_Type, Add_Position_Attribute'Access);
         Result.Add_Attribute_Buffer (Single_Type, Add_Color_Attribute'Access);
      end return;
   end Load_Mesh_1;

   function Load_Mesh_2 (Program : Orka.Rendering.Programs.Program) return Vertex_Format is
      Vertices : constant Single_Array
        := (-0.2,  0.5, -1.0,
             0.3, -0.5, -1.0,
             0.8,  0.5, -1.0);

      Color_Triangle : constant Single_Array
        := (1.0, 0.0, 0.0);

      --  Upload vertices data to VBO
      VBO_3 : constant Buffer := Create_Buffer ((others => False), Vertices);
      VBO_4 : constant Buffer := Create_Buffer ((others => False), Color_Triangle);

      procedure Add_Vertex_Attributes (Buffer : in out Attribute_Buffer) is
      begin
         Buffer.Add_Attribute (Program.Attribute_Location ("in_Position"), 3);
         Buffer.Set_Buffer (VBO_3);
      end Add_Vertex_Attributes;

      procedure Add_Instance_Attribute (Buffer : in out Attribute_Buffer) is
      begin
         Buffer.Add_Attribute (Program.Attribute_Location ("in_Color"), 3);
         Buffer.Set_Buffer (VBO_4);
         Buffer.Set_Per_Instance (True);
      end Add_Instance_Attribute;
   begin
      --  Create mesh and its attributes
      return Result : Vertex_Format := Create_Vertex_Format (UInt_Type) do
         Result.Add_Attribute_Buffer (Single_Type, Add_Vertex_Attributes'Access);
         Result.Add_Attribute_Buffer (Single_Type, Add_Instance_Attribute'Access);
      end return;
   end Load_Mesh_2;

   use Orka.Resources;

   Location_Shaders : constant Locations.Location_Ptr
     := Locations.Directories.Create_Location ("../examples/gl/shaders");

   Program_1 : Program := Create_Program (Modules.Create_Module
     (Location_Shaders, VS => "opengl3.vert", FS => "opengl3.frag"));

   VF_1 : constant Vertex_Format := Load_Mesh_1 (Program_1);
   VF_2 : constant Vertex_Format := Load_Mesh_2 (Program_1);

   FB_D : Framebuffer := Create_Default_Framebuffer (500, 500);
begin
   FB_D.Set_Default_Values ((Color => (0.0, 0.0, 0.0, 1.0), others => <>));

   Program_1.Use_Program;

   while not Window.Should_Close loop
      Window.Process_Input;

      FB_D.Clear ((Color => True, others => False));

      VF_1.Bind;
      Orka.Rendering.Drawing.Draw (GL.Types.Triangles, 0, 3);
      VF_2.Bind;
      Orka.Rendering.Drawing.Draw (GL.Types.Triangles, 0, 3);

      Window.Swap_Buffers;
   end loop;
end Orka_Test.Test_2_Two_Triangles;
