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

with GL.Attributes;
with GL.Buffers;
with GL.Drawing;
with GL.Files;
with GL.Objects.Buffers;
with GL.Objects.Shaders;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Types;

with GL_Test.Display_Backend;

procedure GL_Test.Subroutines is
   Initialized : constant Boolean := Display_Backend.Init
     (Major => 3, Minor => 2, Width => 500, Height => 500, Resizable => False);
   pragma Unreferenced (Initialized);

   use GL.Buffers;
   use GL.Types;
   use GL.Objects.Vertex_Arrays;
   use GL.Objects.Shaders;

   package Single_Pointers is new GL.Objects.Buffers.Buffer_Pointers
     (Single_Pointers);

   procedure Load_Data (VAO  : Vertex_Array_Object;
                        VBO : GL.Objects.Buffers.Buffer;
                        Program : GL.Objects.Programs.Program) is
      use GL.Objects.Buffers;
      use GL.Attributes;

      Vertices : constant Single_Array
        := (-0.5, -0.5,     1.0, 0.0, 0.0,
             0.5, -0.5,     0.0, 1.0, 0.0,
             0.0,  0.5,     0.0, 0.0, 1.0);

      Attrib_Pos   : constant Attribute := Program.Attrib_Location ("in_Position");
      Attrib_Color : constant Attribute := Program.Attrib_Location ("in_Color");
   begin
      --  Upload Vertices data to VBO
      Single_Pointers.Load_To_Immutable_Buffer (VBO, Vertices, Storage_Bits'(others => False));

      --  Enable and set attributes for VAO VAO
      VAO.Enable_Attribute (Attrib_Pos);
      VAO.Enable_Attribute (Attrib_Color);

      VAO.Set_Attribute_Format (Attrib_Pos, 2, Single_Type, 0);
      VAO.Set_Attribute_Format (Attrib_Color, 3, Single_Type, 2);

      VAO.Set_Attribute_Binding (Attrib_Pos, 0);
      VAO.Set_Attribute_Binding (Attrib_Color, 0);

      VAO.Bind_Vertex_Buffer (0, VBO, Single_Type, 0, 5);
   end Load_Data;

   procedure Load_Shaders (VS, FS : Shader;
                           Program : GL.Objects.Programs.Program) is
   begin
      --  Load shader sources and compile shaders
      GL.Files.Load_Shader_Source_From_File
        (VS, "../examples/gl/shaders/subroutines.vert");
      GL.Files.Load_Shader_Source_From_File
        (FS, "../examples/gl/shaders/subroutines.frag");

      VS.Compile;
      FS.Compile;

      if not VS.Compile_Status then
         Ada.Text_IO.Put_Line ("Compilation of vertex shader failed. Log:");
         Ada.Text_IO.Put_Line (VS.Info_Log);
      end if;
      if not FS.Compile_Status then
         Ada.Text_IO.Put_Line ("Compilation of fragment shader failed. Log:");
         Ada.Text_IO.Put_Line (FS.Info_Log);
      end if;

      --  Set up program
      Program.Attach (VS);
      Program.Attach (FS);
      Program.Link;
      if not Program.Link_Status then
         Ada.Text_IO.Put_Line ("Program linking failed. Log:");
         Ada.Text_IO.Put_Line (Program.Info_Log);
         return;
      end if;
      Program.Use_Program;

      --  Test iteration over program shaders
      Ada.Text_IO.Put_Line ("Listing shaders attached to program...");
      for Shader of Program.Attached_Shaders loop
         Ada.Text_IO.Put_Line ("  Kind: " & Shader_Type'Image (Shader.Kind));
         Ada.Text_IO.Put_Line ("  Status: " & Boolean'Image (Shader.Compile_Status));
      end loop;
   end Load_Shaders;

   VS : Shader (Kind => Vertex_Shader);
   FS : Shader (Kind => Fragment_Shader);
   Program : GL.Objects.Programs.Program;

   Triangle_VBO : GL.Objects.Buffers.Buffer;
   Triangle_VAO : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
begin
   Load_Shaders (VS, FS, Program);
   Ada.Text_IO.Put_Line ("Loaded shaders");

   Load_Data (Triangle_VAO, Triangle_VBO, Program);
   Ada.Text_IO.Put_Line ("Loaded data");

   Ada.Text_IO.Put_Line ("Subroutines:");
   Ada.Text_IO.Put_Line ("  Functions: " & Size'Image (Program.Active_Subroutines (Vertex_Shader)));
   Ada.Text_IO.Put_Line ("  Uniforms: " & Size'Image (Program.Active_Subroutine_Uniforms (Vertex_Shader)));
   Ada.Text_IO.Put_Line ("  Function length: " & Size'Image (Program.Active_Subroutine_Max_Length (Vertex_Shader)));
   Ada.Text_IO.Put_Line ("  Uniform length: " & Size'Image (Program.Active_Subroutine_Uniform_Max_Length (Vertex_Shader)));

   Ada.Text_IO.Put_Line ("Name function 0: " & Program.Subroutine_Name (Vertex_Shader, 0));
   Ada.Text_IO.Put_Line ("Name uniform 0: " & Program.Subroutine_Uniform_Name (Vertex_Shader, 0));

   declare
      subtype Index_Type is GL.Objects.Programs.Subroutine_Index_Type;
      subtype Location_Type is GL.Objects.Programs.Uniform_Location_Type;

      Function_X : constant Index_Type := Program.Subroutine_Index (Vertex_Shader, "update_x");
      Function_Y : constant Index_Type := Program.Subroutine_Index (Vertex_Shader, "update_y");
      Function_Z : constant Index_Type := Program.Subroutine_Index (Vertex_Shader, "update_z");
      Location_U0 : constant Location_Type := Program.Subroutine_Uniform_Location (Vertex_Shader, "update_pos[0]");
      Location_U1 : constant Location_Type := Program.Subroutine_Uniform_Location (Vertex_Shader, "update_pos[1]");
      Location_U2 : constant Location_Type := Program.Subroutine_Uniform_Location (Vertex_Shader, "no_update_pos");

      Num_Uniforms : constant Size := Program.Active_Subroutine_Uniform_Locations (Vertex_Shader);
      Indices : UInt_Array (0 .. Num_Uniforms - 1);
   begin
      Ada.Text_IO.Put_Line ("  Active uniform locations: " & Size'Image (Num_Uniforms));
      Ada.Text_IO.Put_Line ("  Index update_x: " & Index_Type'Image (Function_X));
      Ada.Text_IO.Put_Line ("  Index update_y: " & Index_Type'Image (Function_Y));
      Ada.Text_IO.Put_Line ("  Index update_z: " & Index_Type'Image (Function_Z));
      Ada.Text_IO.Put_Line ("  Location update_pos[0]: " & Location_Type'Image (Location_U0));
      Ada.Text_IO.Put_Line ("  Location update_pos[1]: " & Location_Type'Image (Location_U1));
      Ada.Text_IO.Put_Line ("  Location no_update_pos: " & Location_Type'Image (Location_U2));

      declare
         Total_Uniforms : constant Size := Program.Active_Subroutine_Uniforms (Vertex_Shader);
      begin
         Ada.Text_IO.Put_Line ("Number of subroutine uniforms: " & Size'Image (Total_Uniforms));
         for Index_Uniform in 0 .. Total_Uniforms - 1 loop
            declare
               Index : constant Index_Type := Index_Type (Index_Uniform);
               Name  : constant String := Program.Subroutine_Uniform_Name (Vertex_Shader, Index);
            begin
               Ada.Text_IO.Put_Line ("  (" & Size'Image (Index_Uniform) & ") " & Name);

               for Index_Subroutine of Program.Subroutine_Indices_Uniform (Vertex_Shader, Index) loop
                  declare
                     Index : constant Index_Type := Index_Type (Index_Subroutine);
                     Name  : constant String := Program.Subroutine_Name (Vertex_Shader, Index);
                  begin
                     Ada.Text_IO.Put_Line ("    - " & Index_Type'Image (Index_Subroutine) & ": " & Name);
                  end;
               end loop;
            end;
         end loop;
      end;

      Ada.Text_IO.Put_Line ("Usage: Press space key to cycle between subroutines.");

      while not Display_Backend.Get_Window.Should_Close loop
         Clear (Buffer_Bits'(Color => True, Depth => True, others => False));

         if Display_Backend.Get_Effect (2) = 0 then
            Indices (Location_U0) := Function_X;
         else
            Indices (Location_U0) := Function_Y;
         end if;
         Indices (Location_U1) := Function_Y;
         Indices (Location_U2) := Function_Z;
         GL.Objects.Programs.Set_Uniform_Subroutines (Vertex_Shader, Indices);

         Triangle_VAO.Bind;
         GL.Drawing.Draw_Arrays (Triangles, 0, 3);

         -- Swap front and back buffers and process events
         Display_Backend.Swap_Buffers_And_Poll_Events;
      end loop;
   end;

   Display_Backend.Shutdown;
end GL_Test.Subroutines;
