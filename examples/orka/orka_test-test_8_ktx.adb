--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

with Ada.Command_Line;
with Ada.Text_IO;

with GL.Buffers;
with GL.Debug.Logs;
with GL.Drawing;
with GL.Objects.Textures;
with GL.Objects.Vertex_Arrays;
with GL.Types;

with Orka.Debug;
with Orka.Programs.Modules;
with Orka.Programs.Uniforms;
with Orka.Resources.Textures.KTX;
with Orka.Windows.GLFW;

procedure Orka_Test.Test_8_KTX is
   Width   : constant := 1280;
   Height  : constant := 720;

   Initialized : constant Orka.Windows.GLFW.Active_GLFW'Class
     := Orka.Windows.GLFW.Initialize (Major => 3, Minor => 2, Debug => True);
   pragma Unreferenced (Initialized);

   W : constant Orka.Windows.Window'Class := Orka.Windows.GLFW.Create_Window
     (Width, Height, Resizable => False);
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("Usage: <path to .ktx file>");
      return;
   end if;

   Ada.Text_IO.Put_Line ("Flushing" & GL.Types.Size'Image (GL.Debug.Logs.Logged_Messages) & " messages in the debug log:");
   Orka.Debug.Flush_Log;

   Orka.Debug.Enable_Print_Callback;
   Ada.Text_IO.Put_Line ("Set callback for debug messages");

   declare
      package Textures renames Orka.Resources.Textures;

      Texture_1 : constant Textures.Texture := Textures.KTX.Load_Texture (Ada.Command_Line.Argument (1));
      --  TODO Handle non-Texture_2D textures

      use Orka.Programs;
      use GL.Objects.Textures;
      use GL.Types;

      Program_1 : Program := Create_Program (Modules.Create_Module
        (VS => "../examples/orka/shaders/test-8-module-1.vert",
         FS => "../examples/orka/shaders/test-8-module-1.frag"));

      Uni_Texture : constant Uniforms.Uniform_Sampler := Program_1.Uniform_Sampler ("colorTexture");

      Screen_Size : constant Uniforms.TS.Vector4
        := (Single (Width), Single (Height), 0.0, 0.0);

      Uni_Screen : constant Uniforms.Uniform := Program_1.Uniform ("screenSize");

      VAO_1 : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   begin
      --  Bind an empty VAO. Vertex shader contains the data needed
      --  to generate a quad
      VAO_1.Bind;

      Texture_1.Element.Set_X_Wrapping (Clamp_To_Edge);
      Texture_1.Element.Set_X_Wrapping (Clamp_To_Edge);

      Texture_1.Element.Set_Minifying_Filter (Nearest);
      Texture_1.Element.Set_Magnifying_Filter (Nearest);

      Uni_Texture.Set_Texture (Texture_1.Element, 0);
      Uni_Screen.Set_Vector (Screen_Size);

      Program_1.Use_Program;

      while not W.Should_Close loop
         W.Process_Input;

         GL.Buffers.Clear (GL.Buffers.Buffer_Bits'(Color => True, Depth => True, others => False));
         GL.Drawing.Draw_Arrays (GL.Types.Triangles, 0, 6);

         W.Swap_Buffers;
      end loop;
   end;
end Orka_Test.Test_8_KTX;
