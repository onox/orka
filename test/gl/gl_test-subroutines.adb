--------------------------------------------------------------------------------
-- Copyright (c) 2016 onox <denkpadje@gmail.com>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

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
   use GL.Buffers;
   use GL.Types;
   use GL.Objects.Vertex_Arrays;
   use GL.Objects.Shaders;

   procedure Load_Vectors is new GL.Objects.Buffers.Load_To_Buffer
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
      Load_Vectors (VBO, Vertices, Static_Draw);

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
        (VS, "../test/gl/shaders/subroutines.vert");
      GL.Files.Load_Shader_Source_From_File
        (FS, "../test/gl/shaders/subroutines.frag");

      VS.Compile;
      FS.Compile;

      if not VS.Compile_Status then
         Ada.Text_IO.Put_Line ("Compilation of vertex shader failed. log:");
         Ada.Text_IO.Put_Line (VS.Info_Log);
      end if;
      if not FS.Compile_Status then
         Ada.Text_IO.Put_Line ("Compilation of fragment shader failed. log:");
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
   Display_Backend.Init (Major => 3, Minor => 2);
   Display_Backend.Set_Not_Resizable;
   Display_Backend.Open_Window (Width => 500, Height => 500);
   Ada.Text_IO.Put_Line ("Initialized GLFW window");

   VS.Initialize_Id;
   FS.Initialize_Id;
   Program.Initialize_Id;

   Triangle_VBO.Initialize_Id;
   Triangle_VAO.Initialize_Id;

   Ada.Text_IO.Put_Line ("Initialized objects");

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
      Location_U : constant Location_Type := Program.Subroutine_Uniform_Location (Vertex_Shader, "update_pos");

      Num_Uniforms : constant Size := Program.Active_Subroutine_Uniform_Locations (Vertex_Shader);
      Indices : UInt_Array (0 .. Num_Uniforms - 1);
   begin
      Ada.Text_IO.Put_Line ("  Active uniforms: " & Size'Image (Num_Uniforms));
      Ada.Text_IO.Put_Line ("  Index update_x: " & Index_Type'Image (Function_X));
      Ada.Text_IO.Put_Line ("  Index update_y: " & Index_Type'Image (Function_Y));
      Ada.Text_IO.Put_Line ("  Location update_pos: " & Location_Type'Image (Location_U));

      Ada.Text_IO.Put_Line ("Usage: Press space key to cycle between subroutines.");

      while not Display_Backend.Get_Window.Should_Close loop
         Clear (Buffer_Bits'(Color => True, Depth => True, others => False));

         if Display_Backend.Get_Effect (2) = 0 then
            Indices (Location_U) := Function_X;
         else
            Indices (Location_U) := Function_Y;
         end if;
         GL.Objects.Programs.Set_Uniform_Subroutines (Vertex_Shader, Indices);

         Triangle_VAO.Bind;
         GL.Drawing.Draw_Arrays (Triangles, 0, 3);

         -- Swap front and back buffers and process events
         Display_Backend.Swap_Buffers_And_Poll_Events;
      end loop;
   end;

   Display_Backend.Shutdown;
end GL_Test.Subroutines;
