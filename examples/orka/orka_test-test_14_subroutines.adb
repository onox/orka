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

with Orka.Debug;
with Orka.Rendering.Buffers;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Programs.Uniforms;
with Orka.Rendering.Vertex_Formats;
with Orka.Resources.Locations.Directories;
with Orka.Types;

with GL_Test.Display_Backend;

procedure Orka_Test.Test_14_Subroutines is
   Initialized : constant Boolean := GL_Test.Display_Backend.Init
     (Major => 3, Minor => 2, Width => 500, Height => 500, Resizable => False, Debug => True);
   pragma Unreferenced (Initialized);

   use Orka.Rendering.Buffers;
   use Orka.Rendering.Vertex_Formats;

   function Load_Data (Program : Orka.Rendering.Programs.Program) return Vertex_Format is
      use GL.Types;
      use all type Orka.Types.Element_Type;

      Vertices : constant Single_Array
        --  Position        Color
        := (-0.5, -0.5,     1.0, 0.0, 0.0,
             0.5, -0.5,     0.0, 1.0, 0.0,
             0.0,  0.5,     0.0, 0.0, 1.0);

      VBO : constant Buffer := Create_Buffer ((others => False), Vertices);

      procedure Add_Vertex_Attributes (Buffer : in out Attribute_Buffer) is
      begin
         Buffer.Add_Attribute (Program.Attribute_Location ("in_Position"), 2);
         Buffer.Add_Attribute (Program.Attribute_Location ("in_Color"), 3);
         Buffer.Set_Buffer (VBO);
      end Add_Vertex_Attributes;
   begin
      return Result : Vertex_Format := Create_Vertex_Format (Triangles, UInt_Type) do
         Result.Add_Attribute_Buffer (Single_Type, Add_Vertex_Attributes'Access);
      end return;
   end Load_Data;

   use Orka.Rendering.Framebuffers;
   use Orka.Rendering.Programs;
   use Orka.Resources;

   Location_Shaders : constant Locations.Location_Ptr
     := Locations.Directories.Create_Location ("../examples/gl/shaders");

   Program_1 : Program := Create_Program (Modules.Create_Module
     (Location_Shaders, VS => "subroutines.vert", FS => "subroutines.frag"));

   Uniform_1 : Uniforms.Uniform_Subroutine := Program_1.Uniform_Subroutine
     (GL.Objects.Shaders.Vertex_Shader, "update_pos");
   Uniform_2 : Uniforms.Uniform_Subroutine := Program_1.Uniform_Subroutine
     (GL.Objects.Shaders.Vertex_Shader, "no_update_pos");

   Function_X : constant Subroutine_Index := Uniform_1.Index ("update_x");
   Function_Y : constant Subroutine_Index := Uniform_1.Index ("update_y");
   Function_Z : constant Subroutine_Index := Uniform_2.Index ("update_z");

   VF_1 : constant Vertex_Format := Load_Data (Program_1);

   FB_D : Framebuffer := Create_Default_Framebuffer (500, 500);
begin
   Orka.Debug.Flush_Log;
   Orka.Debug.Enable_Print_Callback;

   FB_D.Set_Default_Values ((Color => (0.0, 0.0, 0.0, 1.0), Depth => 1.0, others => <>));

   Ada.Text_IO.Put_Line ("Usage: Press space key to cycle between subroutines.");

   while not GL_Test.Display_Backend.Get_Window.Should_Close loop
      FB_D.Clear ((Color | Depth => True, others => False));

      if GL_Test.Display_Backend.Get_Effect (2) = 0 then
         Uniform_1.Set_Function (Function_X);
      else
         Uniform_1.Set_Function (Function_Y);
      end if;
      Uniform_2.Set_Function (Function_Z);

      Program_1.Use_Program;

      VF_1.Draw (0, 3);

      -- Swap front and back buffers and process events
      GL_Test.Display_Backend.Swap_Buffers_And_Poll_Events;
   end loop;

   GL_Test.Display_Backend.Shutdown;
end Orka_Test.Test_14_Subroutines;
