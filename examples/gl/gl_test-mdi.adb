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

with GL.Drawing;
with GL.Files;
with GL.Objects.Buffers;
with GL.Objects.Framebuffers;
with GL.Objects.Shaders;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Pixels;
with GL.Types.Indirect;

with GL_Test.Display_Backend;

procedure GL_Test.MDI is
   Initialized : constant Boolean := Display_Backend.Init
     (Major => 3, Minor => 2, Width => 500, Height => 500, Resizable => False);
   pragma Unreferenced (Initialized);

   use GL.Types;
   use GL.Objects.Vertex_Arrays;

   Commands : Indirect.Elements_Indirect_Command_Array (1 .. 2);

   procedure Load_Data (VAO : Vertex_Array_Object;
                        Buffer1, Buffer2, Buffer3, Buffer4 : GL.Objects.Buffers.Buffer;
                        Program : GL.Objects.Programs.Program) is
      use GL.Objects.Buffers;

      package Single_Pointers is new GL.Objects.Buffers.Buffer_Pointers
        (Single_Pointers);

      package UInt_Pointers is new GL.Objects.Buffers.Buffer_Pointers
        (UInt_Pointers);

      package Command_Pointers is new GL.Objects.Buffers.Buffer_Pointers
        (Indirect.Elements_Indirect_Command_Pointers);

      Vertices : constant Single_Array
        := (-0.3,  0.5, -1.0,
            -0.8, -0.5, -1.0,
             0.2, -0.5, -1.0,
            -0.2,  0.5, -1.0,
             0.3, -0.5, -1.0,
             0.8,  0.5, -1.0);

      Indices : constant UInt_Array
        := (0, 1, 2, 0, 1, 2);

      --  A blue triangle on the left and a yellow triangle on the right
      Instances_IDs : constant UInt_Array
        := (2, 3);

      Attrib_Position : constant Attribute := Program.Attrib_Location ("in_Position");
      Attrib_Instance : constant Attribute := Program.Attrib_Location ("in_InstanceID");
   begin
      --  Upload vertices to Buffer1
      Single_Pointers.Load_To_Immutable_Buffer (Buffer1, Vertices, Storage_Bits'(others => False));

      --  Upload indices to Buffer2
      UInt_Pointers.Load_To_Immutable_Buffer (Buffer2, Indices, Storage_Bits'(others => False));
      VAO.Bind_Element_Buffer (Buffer2);

      --  Upload commands to Buffer3
      Commands (1) := (Count => 3, Instances => 1, First_Index => 0, Base_Vertex => 0, Base_Instance => 0);
      Commands (2) := (Count => 3, Instances => 1, First_Index => 3, Base_Vertex => 3, Base_Instance => 1);
      Command_Pointers.Load_To_Immutable_Buffer (Buffer3, Commands, Storage_Bits'(others => False));

      --  Upload instance ID's to Buffer4
      UInt_Pointers.Load_To_Immutable_Buffer (Buffer4, Instances_IDs, Storage_Bits'(others => False));

      --  Enable and set attributes for the VAO
      VAO.Enable_Attribute (Attrib_Position);
      VAO.Enable_Attribute (Attrib_Instance);

      VAO.Set_Attribute_Format (Attrib_Position, 3, Single_Type, 0);
      VAO.Set_Attribute_Format (Attrib_Instance, 1, UInt_Type, 0);

      VAO.Set_Attribute_Binding (Attrib_Position, 0);
      VAO.Set_Attribute_Binding (Attrib_Instance, 1);

      VAO.Set_Attribute_Binding_Divisor (1, 1);

      VAO.Bind_Vertex_Buffer (0, Buffer1, Single_Type, 0, 3);
      VAO.Bind_Vertex_Buffer (1, Buffer4, UInt_Type, 0, 1);
   end Load_Data;

   procedure Load_Shaders (Vertex_Shader, Fragment_Shader : GL.Objects.Shaders.Shader;
                           Program : GL.Objects.Programs.Program) is
   begin
      --  Load shader sources and compile shaders
      GL.Files.Load_Shader_Source_From_File
        (Vertex_Shader, "../examples/gl/shaders/mdi.vert");
      GL.Files.Load_Shader_Source_From_File
        (Fragment_Shader, "../examples/gl/shaders/mdi.frag");

      Vertex_Shader.Compile;
      Fragment_Shader.Compile;

      if not Vertex_Shader.Compile_Status then
         Ada.Text_IO.Put_Line ("Compilation of vertex shader failed. Log:");
         Ada.Text_IO.Put_Line (Vertex_Shader.Info_Log);
      end if;
      if not Fragment_Shader.Compile_Status then
         Ada.Text_IO.Put_Line ("Compilation of fragment shader failed. Log:");
         Ada.Text_IO.Put_Line (Fragment_Shader.Info_Log);
      end if;

      --  Set up program
      Program.Attach (Vertex_Shader);
      Program.Attach (Fragment_Shader);

      Program.Link;
      if not Program.Link_Status then
         Ada.Text_IO.Put_Line ("Program linking failed. Log:");
         Ada.Text_IO.Put_Line (Program.Info_Log);
         return;
      end if;
      Program.Use_Program;
   end Load_Shaders;

   Vertex_Shader   : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Vertex_Shader);
   Fragment_Shader : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Fragment_Shader);
   Program         : GL.Objects.Programs.Program;

   Vertex_Buffer, Index_Buffer     : GL.Objects.Buffers.Buffer;
   Command_Buffer, Instance_Buffer : GL.Objects.Buffers.Buffer;
   Array1 : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

   Default_Framebuffer : constant GL.Objects.Framebuffers.Framebuffer
     := GL.Objects.Framebuffers.Default_Framebuffer;
begin
   Load_Shaders (Vertex_Shader, Fragment_Shader, Program);
   Ada.Text_IO.Put_Line ("Loaded shaders");

   Load_Data (Array1, Vertex_Buffer, Index_Buffer, Command_Buffer, Instance_Buffer, Program);
   Ada.Text_IO.Put_Line ("Loaded data");

   Array1.Bind;
   GL.Objects.Buffers.Draw_Indirect_Buffer.Bind (Command_Buffer);

   while not Display_Backend.Get_Window.Should_Close loop
      Default_Framebuffer.Clear_Color_Buffer (0, GL.Pixels.Float_Type, (0.0, 0.0, 0.0, 1.0));
      Default_Framebuffer.Clear_Depth_Buffer (1.0);

      GL.Drawing.Draw_Multiple_Elements_Indirect (Triangles, UInt_Type, Commands'Length);

      --  Swap front and back buffers and process events
      Display_Backend.Swap_Buffers_And_Poll_Events;
   end loop;

   Display_Backend.Shutdown;
end GL_Test.MDI;
