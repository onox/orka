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
with Orka.Meshes.Buffers;
with Orka.Programs.Modules;

with GL_Test.Display_Backend;

procedure Orka_Test.Test_3_Module_Array is
   use Orka.Meshes;
   use Orka.Programs;

   function Load_Mesh (Program : Orka.Programs.Program) return Mesh is
      use GL.Types;
      use GL.Objects.Buffers;

      VBO : Orka.Buffers.Buffer := Orka.Buffers.Create_Buffer (Static_Draw);

      Vertices : constant Single_Array
        := (-0.5, -0.5,
             0.5, -0.5,
             0.0,  0.5);
   begin
      --  Upload Vertices data to VBO
      VBO.Set_Data (Vertices);

      --  Create mesh and its attributes
      return Result : Mesh := Orka.Meshes.Create_Mesh (Triangles) do
         declare
            Attributes : Buffers.Attribute_Buffer := Result.Add_Attribute_Buffer (Single_Type);
         begin
            Attributes.Add_Attribute (Program.Attribute_Location ("in_Position"), 2);
            Attributes.Set_Buffer (VBO);
         end;
      end return;
   end Load_Mesh;
begin
   GL_Test.Display_Backend.Init (Major => 3, Minor => 2);
   GL_Test.Display_Backend.Set_Not_Resizable;
   GL_Test.Display_Backend.Open_Window (Width => 500, Height => 500);

   declare
      use GL.Buffers;

      Program_1 : constant Program := Orka.Programs.Create_Program (Modules.Module_Array'(
        Modules.Create_Module
          (FS => "../examples/orka/shaders/test-3-module-1.frag"),
        Modules.Create_Module
          (VS => "../examples/orka/shaders/test-3-module-2.vert",
           FS => "../examples/orka/shaders/test-3-module-2.frag")
      ));

      Triangle : constant Mesh := Load_Mesh (Program_1);
   begin
      Program_1.Use_Program;

      while not GL_Test.Display_Backend.Get_Window.Should_Close loop
         Clear (Buffer_Bits'(Color => True, Depth => True, others => False));

         Triangle.Draw (0, 3);

         GL_Test.Display_Backend.Swap_Buffers_And_Poll_Events;
      end loop;
   end;

   GL_Test.Display_Backend.Shutdown;
end Orka_Test.Test_3_Module_Array;
