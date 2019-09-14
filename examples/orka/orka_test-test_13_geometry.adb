--  Copyright (c) 2015 onox <denkpadje@gmail.com>
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

with Orka.Rendering.Buffers;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Vertex_Formats;
with Orka.Resources.Locations.Directories;
with Orka.Types;

with GL_Test.Display_Backend;

procedure Orka_Test.Test_13_Geometry is
   Initialized : constant Boolean := GL_Test.Display_Backend.Init
     (Major => 3, Minor => 2, Width => 500, Height => 500, Resizable => False);
   pragma Unreferenced (Initialized);

   use Orka.Rendering.Buffers;
   use Orka.Rendering.Vertex_Formats;

   function Load_Data (Program : Orka.Rendering.Programs.Program) return Vertex_Format is
      use GL.Types;
      use all type Orka.Types.Element_Type;

      Vertices : constant Single_Array
        --  Position        Color           Sides
        := (-0.45,  0.45,   1.0, 0.0, 0.0,  4.0,
             0.45,  0.45,   0.0, 1.0, 0.0,  8.0,
             0.45, -0.45,   0.0, 0.0, 1.0,  16.0,
            -0.45, -0.45,   1.0, 1.0, 0.0,  32.0);

      VBO : constant Buffer := Create_Buffer ((others => False), Vertices);

      procedure Add_Vertex_Attributes (Buffer : in out Attribute_Buffer) is
      begin
         Buffer.Add_Attribute (Program.Attribute_Location ("position"), 2);
         Buffer.Add_Attribute (Program.Attribute_Location ("color"), 3);
         Buffer.Add_Attribute (Program.Attribute_Location ("sides"), 1);
         Buffer.Set_Buffer (VBO);
      end Add_Vertex_Attributes;
   begin
      return Result : Vertex_Format := Create_Vertex_Format (Points, UInt_Type) do
         Result.Add_Attribute_Buffer (Single_Type, Add_Vertex_Attributes'Access);
      end return;
   end Load_Data;

   use Orka.Rendering.Framebuffers;
   use Orka.Rendering.Programs;
   use Orka.Resources;

   Location_Shaders : constant Locations.Location_Ptr
     := Locations.Directories.Create_Location ("../examples/gl/shaders");

   Program_1 : Program := Create_Program (Modules.Create_Module
     (Location_Shaders, VS => "geometry.vert", GS => "geometry.geom", FS => "geometry.frag"));

   -- Upload vertices to GPU
   VF_1 : constant Vertex_Format := Load_Data (Program_1);

   FB_D : Framebuffer := Create_Default_Framebuffer (500, 500);
begin
   Program_1.Use_Program;

   FB_D.Set_Default_Values ((Color => (0.0, 0.0, 0.0, 1.0), others => <>));

   while not GL_Test.Display_Backend.Get_Window.Should_Close loop
      FB_D.Clear ((Color => True, others => False));

      VF_1.Draw (0, 4);

      -- Swap front and back buffers and process events
      GL_Test.Display_Backend.Swap_Buffers_And_Poll_Events;
   end loop;

   GL_Test.Display_Backend.Shutdown;
end Orka_Test.Test_13_Geometry;
