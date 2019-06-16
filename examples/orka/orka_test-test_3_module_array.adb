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

with GL.Pixels;
with GL.Types;

with Orka.Rendering.Buffers;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Vertex_Formats;
with Orka.Resources.Locations.Directories;
with Orka.Types;

with GL_Test.Display_Backend;

procedure Orka_Test.Test_3_Module_Array is
   Initialized : constant Boolean := GL_Test.Display_Backend.Init
     (Major => 3, Minor => 2, Width => 500, Height => 500, Resizable => False);
   pragma Unreferenced (Initialized);

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
      return Result : Vertex_Format := Create_Vertex_Format (Triangles, UInt_Type) do
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

   Triangle : constant Vertex_Format := Load_Mesh (Program_1);

   Default_FB : constant Framebuffer := Create_Default_Framebuffer (500, 500);
begin
   Program_1.Use_Program;

   while not GL_Test.Display_Backend.Get_Window.Should_Close loop
      Default_FB.GL_Framebuffer.Clear_Color_Buffer (0, GL.Pixels.Float_Type, (0.0, 0.0, 0.0, 0.0));
      Default_FB.GL_Framebuffer.Clear_Depth_Buffer (1.0);

      Triangle.Draw (0, 3);

      GL_Test.Display_Backend.Swap_Buffers_And_Poll_Events;
   end loop;

   GL_Test.Display_Backend.Shutdown;
end Orka_Test.Test_3_Module_Array;
