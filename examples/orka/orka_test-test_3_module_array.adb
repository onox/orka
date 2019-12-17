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

procedure Orka_Test.Test_3_Module_Array is
   Library : constant Orka.Contexts.Library'Class
     := Orka.Windows.GLFW.Initialize (Major => 4, Minor => 2);

   Window : aliased Orka.Windows.Window'Class
     := Library.Create_Window (Width => 500, Height => 500, Resizable => False);

   Context : constant Orka.Contexts.Context'Class := Window.Context;
   pragma Unreferenced (Context);

   use Orka.Rendering.Buffers;
   use Orka.Rendering.Framebuffers;
   use Orka.Rendering.Vertex_Formats;
   use Orka.Rendering.Programs;

   function Load_Mesh (Program : Orka.Rendering.Programs.Program) return Vertex_Format is
      use GL.Types;
      use all type Orka.Types.Element_Type;

      Vertices : constant Single_Array
        := (-0.5, -0.5,
             0.5, -0.5,
             0.0,  0.5);

      --  Upload Vertices data to VBO
      VBO : constant Buffer := Create_Buffer ((others => False), Vertices);

      procedure Add_Vertex_Attributes (Buffer : in out Attribute_Buffer) is
      begin
         Buffer.Add_Attribute (Program.Attribute_Location ("in_Position"), 2);
         Buffer.Set_Buffer (VBO);
      end Add_Vertex_Attributes;
   begin
      --  Create mesh and its attributes
      return Result : Vertex_Format := Create_Vertex_Format (UInt_Type) do
         Result.Add_Attribute_Buffer (Single_Type, Add_Vertex_Attributes'Access);
      end return;
   end Load_Mesh;

   use Orka.Resources;

   Location_Shaders : constant Locations.Location_Ptr
     := Locations.Directories.Create_Location ("../examples/orka/shaders");

   Program_1 : Program := Create_Program (Modules.Module_Array'(
     Modules.Create_Module (Location_Shaders,
       FS => "test-3-module-1.frag"),
     Modules.Create_Module (Location_Shaders,
       VS => "test-3-module-2.vert",
       FS => "test-3-module-2.frag")
   ));

   VF_1 : constant Vertex_Format := Load_Mesh (Program_1);

   FB_D : Framebuffer := Create_Default_Framebuffer (500, 500);
begin
   FB_D.Set_Default_Values ((Color => (0.0, 0.0, 0.0, 1.0), others => <>));

   VF_1.Bind;
   Program_1.Use_Program;

   while not Window.Should_Close loop
      Window.Process_Input;

      FB_D.Clear ((Color => True, others => False));
      Orka.Rendering.Drawing.Draw (GL.Types.Triangles, 0, 3);

      Window.Swap_Buffers;
   end loop;
end Orka_Test.Test_3_Module_Array;
