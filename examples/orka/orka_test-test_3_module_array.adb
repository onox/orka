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
with Orka.Resources.Locations.Directories;
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
   use Orka.Rendering.Programs;

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

   FB_D : Framebuffer := Create_Default_Framebuffer (500, 500);

   use GL.Types;

   Vertices : constant Single_Array
     := (-0.5, -0.5,
          0.5, -0.5,
          0.0,  0.5);

   --  Upload Vertices data to VBO
   Buffer_1 : constant Buffer := Create_Buffer ((others => False), Vertices);
begin
   FB_D.Set_Default_Values ((Color => (0.0, 0.0, 0.0, 1.0), others => <>));

   Program_1.Use_Program;
   Buffer_1.Bind (Shader_Storage, 0);

   while not Window.Should_Close loop
      Window.Process_Input;

      FB_D.Clear ((Color => True, others => False));
      Orka.Rendering.Drawing.Draw (Triangles, 0, 3);

      Window.Swap_Buffers;
   end loop;
end Orka_Test.Test_3_Module_Array;
