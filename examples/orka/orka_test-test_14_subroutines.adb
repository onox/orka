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

with Ada.Text_IO;

with GL.Objects.Shaders;
with GL.Types;

with Orka.Contexts;
with Orka.Debug;
with Orka.Rendering.Buffers;
with Orka.Rendering.Drawing;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Programs.Uniforms;
with Orka.Resources.Locations.Directories;
with Orka.Windows.GLFW;

--  This example renders a triangle. The position of the triangle is
--  determined by the chosen subroutine function in the shader.

procedure Orka_Test.Test_14_Subroutines is
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
     := Locations.Directories.Create_Location ("../examples/gl/shaders");

   Program_1 : Program := Create_Program (Modules.Create_Module
     (Location_Shaders, VS => "subroutines.vert", FS => "subroutines.frag"));

   Uniform_1 : Uniforms.Uniform_Subroutine := Program_1.Uniform_Subroutine
     (GL.Objects.Shaders.Vertex_Shader, "update_pos");

   Function_X : constant Subroutine_Index := Uniform_1.Index ("update_x");
   Function_Y : constant Subroutine_Index := Uniform_1.Index ("update_y");

   FB_D : Framebuffer := Create_Default_Framebuffer (500, 500);

   use GL.Types;

   Vertices : constant Single_Array
     := (-0.5, -0.5, 0.0, 1.0,     1.0, 0.0, 0.0, 0.0,
          0.5, -0.5, 0.0, 1.0,     0.0, 1.0, 0.0, 0.0,
          0.0,  0.5, 0.0, 1.0,     0.0, 0.0, 1.0, 0.0);

   Buffer_1 : constant Buffer := Create_Buffer ((others => False), Vertices);
begin
   Orka.Debug.Set_Log_Messages (Enable => True);

   FB_D.Set_Default_Values ((Color => (0.0, 0.0, 0.0, 1.0), Depth => 1.0, others => <>));
   Buffer_1.Bind (Shader_Storage, 0);

   Ada.Text_IO.Put_Line ("Usage: Press space key to cycle between subroutines.");

   while not Window.Should_Close loop
      Window.Process_Input;

      FB_D.Clear ((Color => True, others => False));

      --  TODO Toggle between Function_X and Function_Y via keyboard
      Uniform_1.Set_Function (Function_X);
      Program_1.Use_Program;

      Orka.Rendering.Drawing.Draw (Triangles, 0, 3);

      --  Swap front and back buffers and process events
      Window.Swap_Buffers;
   end loop;
end Orka_Test.Test_14_Subroutines;
