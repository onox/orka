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

with GL.Buffers;
with GL.Objects.Buffers;
with GL.Types;

with Orka.Buffers;
with Orka.Meshes.Attributes;
with Orka.Programs.Modules;

with GL_Test.Display_Backend;

procedure Orka_Test.Test_1_Triangle is
   Initialized : constant Boolean := GL_Test.Display_Backend.Init
     (Major => 3, Minor => 2, Width => 500, Height => 500, Resizable => False);
   pragma Unreferenced (Initialized);

   use Orka.Meshes;
   use Orka.Programs;

   function Load_Mesh (Program : Orka.Programs.Program) return Vertex_Format is
      use GL.Types;
      use GL.Objects.Buffers;

      Vertices : constant Single_Array
        := (-0.5, -0.5,     1.0, 0.0, 0.0,
             0.5, -0.5,     0.0, 1.0, 0.0,
             0.0,  0.5,     0.0, 0.0, 1.0);

      --  Upload Vertices data to VBO
      VBO : constant Orka.Buffers.Buffer := Orka.Buffers.Create_Buffer (Storage_Bits'(others => False), Vertices);
   begin
      --  Create mesh and its attributes
      return Result : Vertex_Format := Orka.Meshes.Create_Vertex_Format (Triangles) do
         declare
            Attributes_All : Attributes.Attribute_Buffer := Result.Add_Attribute_Buffer (Single_Type);
         begin
            Attributes_All.Add_Attribute (Program.Attribute_Location ("in_Position"), 2);
            Attributes_All.Add_Attribute (Program.Attribute_Location ("in_Color"), 3);
            Attributes_All.Set_Buffer (VBO);
         end;
      end return;
   end Load_Mesh;

   use GL.Buffers;

   Program_1 : Program := Orka.Programs.Create_Program (Modules.Create_Module
     (VS => "../examples/gl/shaders/opengl3.vert",
      FS => "../examples/gl/shaders/opengl3.frag"));

   Triangle : constant Vertex_Format := Load_Mesh (Program_1);
begin
   Program_1.Use_Program;

   while not GL_Test.Display_Backend.Get_Window.Should_Close loop
      Clear (Buffer_Bits'(Color => True, Depth => True, others => False));

      Triangle.Draw (0, 3);

      GL_Test.Display_Backend.Swap_Buffers_And_Poll_Events;
   end loop;

   GL_Test.Display_Backend.Shutdown;
end Orka_Test.Test_1_Triangle;
