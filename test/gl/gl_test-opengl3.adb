--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
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
with GL.Files;
with GL.Objects.Buffers;
with GL.Objects.Shaders;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Types.Colors;

with GL_Test.Display_Backend;

procedure GL_Test.OpenGL3 is
   use GL.Buffers;
   use GL.Types;
   use GL.Objects.Vertex_Arrays;

   procedure Load_Vectors is new GL.Objects.Buffers.Load_To_Buffer
     (Singles.Vector3_Pointers);

   procedure Load_Colors is new GL.Objects.Buffers.Load_To_Buffer
     (Colors.Basic_Color_Pointers);

   procedure Load_Data (Array1, Array2            : Vertex_Array_Object;
                        Buffer1, Buffer2, Buffer3 : GL.Objects.Buffers.Buffer;
                        Program                   : GL.Objects.Programs.Program) is
      use GL.Objects.Buffers;
      use GL.Attributes;
   
      Triangle1 : constant Singles.Vector3_Array
        := ((-0.3,  0.5, -1.0),
            (-0.8, -0.5, -1.0),
             (0.2, -0.5, -1.0));
      Triangle2 : constant Singles.Vector3_Array
        := ((-0.2,  0.5, -1.0),
             (0.3, -0.5, -1.0),
             (0.8,  0.5, -1.0));
      Color_Array : constant Colors.Basic_Color_Array
        := ((1.0, 0.0, 0.0),
            (0.0, 1.0, 0.0),
            (0.0, 0.0, 1.0));

      Attrib_Pos   : constant Attribute := Program.Attrib_Location ("in_Position");
      Attrib_Color : constant Attribute := Program.Attrib_Location ("in_Color");
   begin
      -- First vertex array object: Colored vertices
      Array1.Bind;

      --  Upload Triangle1 data to Buffer1
      Array_Buffer.Bind (Buffer1);
      Load_Vectors (Array_Buffer, Triangle1, Static_Draw);

      --  Upload Color_Array data to Buffer2
      Array_Buffer.Bind (Buffer2);
      Load_Colors (Array_Buffer, Color_Array, Static_Draw);

      --  Enable and set attributes for Array1 VAO
      Array1.Enable_Attribute (Attrib_Pos);
      Array1.Enable_Attribute (Attrib_Color);

      Array1.Set_Attribute_Format (Attrib_Pos, 3, Single_Type, 0);
      Array1.Set_Attribute_Format (Attrib_Color, 3, Single_Type, 0);

      Array1.Set_Attribute_Binding (Attrib_Pos, 0);
      Array1.Set_Attribute_Binding (Attrib_Color, 1);

      Array1.Bind_Vertex_Buffer (0, Buffer1, Single_Type, 0, 3);
      Array1.Bind_Vertex_Buffer (1, Buffer2, Single_Type, 0, 3);

      --  Second vertex array object: Only vertices
      Array2.Bind;

      --  Upload Triangle2 data to Buffer3
      Array_Buffer.Bind (Buffer3);
      Load_Vectors (Array_Buffer, Triangle2, Static_Draw);

      --  Enable and set attributes for Array2 VAO
      Array2.Enable_Attribute (Attrib_Pos);

      Array2.Set_Attribute_Format (Attrib_Pos, 3, Single_Type, 0);
      Array2.Set_Attribute_Binding (Attrib_Pos, 0);

      Array2.Bind_Vertex_Buffer (0, Buffer3, Single_Type, 0, 3);
   end Load_Data;

   procedure Load_Shaders (Vertex_Shader, Fragment_Shader : GL.Objects.Shaders.Shader;
                           Program : GL.Objects.Programs.Program) is
   begin
      -- load shader sources and compile shaders
      GL.Files.Load_Shader_Source_From_File
        (Vertex_Shader, "../test/gl/shaders/opengl3.vert");
      GL.Files.Load_Shader_Source_From_File
        (Fragment_Shader, "../test/gl/shaders/opengl3.frag");

      Vertex_Shader.Compile;
      Fragment_Shader.Compile;

      if not Vertex_Shader.Compile_Status then
         Ada.Text_IO.Put_Line ("Compilation of vertex shader failed. log:");
         Ada.Text_IO.Put_Line (Vertex_Shader.Info_Log);
      end if;
      if not Fragment_Shader.Compile_Status then
         Ada.Text_IO.Put_Line ("Compilation of fragment shader failed. log:");
         Ada.Text_IO.Put_Line (Fragment_Shader.Info_Log);
      end if;
      
      -- set up program
      Program.Attach (Vertex_Shader);
      Program.Attach (Fragment_Shader);
      Program.Bind_Attrib_Location (0, "in_Position");
      Program.Bind_Attrib_Location (1, "in_Color");
      Program.Link;
      if not Program.Link_Status then
         Ada.Text_IO.Put_Line ("Program linking failed. Log:");
         Ada.Text_IO.Put_Line (Program.Info_Log);
         return;
      end if;
      Program.Use_Program;

      -- test iteration over program shaders
      Ada.Text_IO.Put_Line ("Listing shaders attached to program...");
      for Shader of Program.Attached_Shaders loop
         Ada.Text_IO.Put_Line ("  Kind: " & GL.Objects.Shaders.Shader_Type'Image (Shader.Kind));
         Ada.Text_IO.Put_Line ("  Status: " & Boolean'Image (Shader.Compile_Status));
      end loop;
   end Load_Shaders;

   Vertex_Shader   : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Vertex_Shader);
   Fragment_Shader : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Fragment_Shader);
   Program         : GL.Objects.Programs.Program;

   Vector_Buffer1, Vector_Buffer2, Color_Buffer : GL.Objects.Buffers.Buffer;
   Array1, Array2 : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
begin
   Display_Backend.Init;
   Display_Backend.Configure_Minimum_OpenGL_Version (Major => 3, Minor => 2);
   Display_Backend.Set_Not_Resizable;
   Display_Backend.Open_Window (Width => 500, Height => 500);
   Ada.Text_IO.Put_Line ("Initialized GLFW window");

   Vertex_Shader.Initialize_Id;
   Fragment_Shader.Initialize_Id;
   Program.Initialize_Id;
   Vector_Buffer1.Initialize_Id;
   Vector_Buffer2.Initialize_Id;
   Color_Buffer.Initialize_Id;
   Array1.Initialize_Id;
   Array2.Initialize_Id;

   Ada.Text_IO.Put_Line ("Initialized objects");

   Load_Shaders (Vertex_Shader, Fragment_Shader, Program);

   Ada.Text_IO.Put_Line ("Loaded shaders");

   Load_Data (Array1, Array2, Vector_Buffer1, Color_Buffer, Vector_Buffer2, Program);

   Ada.Text_IO.Put_Line ("Loaded data");

   while not Display_Backend.Get_Window.Should_Close loop
      Clear (Buffer_Bits'(Color => True, Depth => True, others => False));

      Array1.Bind;
      GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 3);

      Array2.Bind;
      GL.Attributes.Set_Single (1, 1.0, 0.0, 0.0);
      GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 3);

      GL.Objects.Vertex_Arrays.Null_Array_Object.Bind;

      GL.Flush;
      Display_Backend.Swap_Buffers;

      Display_Backend.Poll_Events;
   end loop;

   Display_Backend.Shutdown;
end GL_Test.OpenGL3;
