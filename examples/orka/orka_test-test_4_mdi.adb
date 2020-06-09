--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2020 onox <denkpadje@gmail.com>
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
with Orka.Rendering.Buffers.MDI;
with Orka.Rendering.Drawing;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Programs.Uniforms;
with Orka.Resources.Locations.Directories;
with Orka.Types;
with Orka.Windows.GLFW;

procedure Orka_Test.Test_4_MDI is
   Library : constant Orka.Contexts.Library'Class
     := Orka.Windows.GLFW.Initialize (Major => 4, Minor => 2);

   Window : aliased Orka.Windows.Window'Class
     := Library.Create_Window (Width => 500, Height => 500, Resizable => False);

   Context : constant Orka.Contexts.Context'Class := Window.Context;
   pragma Unreferenced (Context);

   use Orka.Resources;
   use Orka.Rendering.Buffers;
   use Orka.Rendering.Framebuffers;
   use Orka.Rendering.Programs;

   use GL.Types;

   Vertices_1 : constant Single_Array
     := (-0.25,  0.5,
         -0.75, -0.5,
          0.25, -0.5);

   Vertices_2 : constant Single_Array
     := (-0.25,  0.5,
          0.25, -0.5,
          0.75,  0.5);

   Indices_1 : constant UInt_Array := (0, 1, 2);
   Indices_2 : constant UInt_Array := (0, 1, 2);

   Batch_1 : MDI.Batch := MDI.Create_Batch
     (Orka.Types.Single_Type, Orka.Types.UInt_Type, 2,
      Vertices_1'Length + Vertices_2'Length,
      Indices_1'Length + Indices_2'Length);

   procedure Append_Draw_Call
     (Instances : Natural; Vertices : Single_Array; Indices : UInt_Array)
   is
      Vertex_Elements : constant := 2;

      procedure Append_Vertices (Offset, Count : Natural) is
      begin
         Batch_1.Data.Write_Data (Vertices, Offset => Offset * Vertex_Elements);
      end Append_Vertices;

      procedure Append_Indices (Offset, Count : Natural) is
      begin
         Batch_1.Indices.Write_Data (Indices, Offset => Offset);
      end Append_Indices;
   begin
      Batch_1.Append (Instances, Vertices'Length / Vertex_Elements, Indices'Length,
        Append_Vertices'Access, Append_Indices'Access);
   end Append_Draw_Call;

   Location_Shaders : constant Locations.Location_Ptr
     := Locations.Directories.Create_Location ("../examples/orka/shaders");

   Program_1 : Program := Create_Program (Modules.Create_Module
     (Location_Shaders, VS => "test-4-module-1.vert", FS => "test-4-module-1.frag"));

   Uniform_Mode : constant Uniforms.Uniform := Program_1.Uniform ("mode");

   FB_D : Framebuffer := Get_Default_Framebuffer (Window);

   type Color_Mode is (Draw_ID, Instance_ID, Object_ID);

   Mode : constant Color_Mode := Object_ID;
begin
   FB_D.Set_Default_Values ((Color => (0.0, 0.0, 0.0, 1.0), others => <>));

   Append_Draw_Call (2, Vertices_1, Indices_1);
   Append_Draw_Call (3, Vertices_2, Indices_2);
   Batch_1.Finish_Batch;

   FB_D.Use_Framebuffer;
   Program_1.Use_Program;

   Batch_1.Data.Bind (Shader_Storage, 0);

   while not Window.Should_Close loop
      Window.Process_Input;

      Uniform_Mode.Set_Integer (Color_Mode'Pos (Mode));
      Window.Set_Title ("Color mode: " & Mode'Image);

      FB_D.Clear ((Color => True, others => False));

      Orka.Rendering.Drawing.Draw_Indexed_Indirect
        (Mode         => Triangles,
         Index_Buffer => Batch_1.Indices.Buffer,
         Buffer       => Batch_1.Commands.Buffer);

      Window.Swap_Buffers;
   end loop;
end Orka_Test.Test_4_MDI;
