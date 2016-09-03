--  Copyright (c) 2015 onox <denkpadje@gmail.com>
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
with GL.Drawing;
with GL.Files;
with GL.Buffers;
with GL.Objects.Shaders;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Types.Colors;

with GL_Test.Display_Backend;

procedure GL_Test.Geometry is
   use GL.Types;
   use GL.Buffers;
   use GL.Types.Colors;
   use GL.Objects.Vertex_Arrays;

   procedure Load_Shaders (Vertex_Source, Geometry_Source, Fragment_Source : String;
                           Vertex_Shader, Geometry_Shader, Fragment_Shader : GL.Objects.Shaders.Shader;
                           Program : GL.Objects.Programs.Program) is
   begin
      -- Load shader sources and compile shaders
      GL.Files.Load_Shader_Source_From_File
        (Vertex_Shader, Vertex_Source);
      GL.Files.Load_Shader_Source_From_File
        (Geometry_Shader, Geometry_Source);
      GL.Files.Load_Shader_Source_From_File
        (Fragment_Shader, Fragment_Source);

      Vertex_Shader.Compile;
      Geometry_Shader.Compile;
      Fragment_Shader.Compile;

      if not Vertex_Shader.Compile_Status then
         Ada.Text_IO.Put_Line ("Compilation of vertex shader failed. log:");
         Ada.Text_IO.Put_Line (Vertex_Shader.Info_Log);
      end if;
      if not Geometry_Shader.Compile_Status then
         Ada.Text_IO.Put_Line ("Compilation of geometry shader failed. log:");
         Ada.Text_IO.Put_Line (Geometry_Shader.Info_Log);
      end if;
      if not Fragment_Shader.Compile_Status then
         Ada.Text_IO.Put_Line ("Compilation of fragment shader failed. log:");
         Ada.Text_IO.Put_Line (Fragment_Shader.Info_Log);
      end if;

      -- Set up program
      Program.Attach (Vertex_Shader);
      Program.Attach (Geometry_Shader);
      Program.Attach (Fragment_Shader);

      Program.Link;
      if not Program.Link_Status then
         Ada.Text_IO.Put_Line ("Program linking failed. Log:");
         Ada.Text_IO.Put_Line (Program.Info_Log);
         return;
      end if;
   end Load_Shaders;

   procedure Load_Data (Array_Points  : Vertex_Array_Object;
                        Buffer_Points : GL.Objects.Buffers.Buffer;
                        Program       : GL.Objects.Programs.Program) is
      use GL.Objects.Buffers;
      use GL.Attributes;

      Vertices : constant Single_Array
        --  Position        Color           Sides
        := (-0.45,  0.45,   1.0, 0.0, 0.0,  4.0,
             0.45,  0.45,   0.0, 1.0, 0.0,  8.0,
             0.45, -0.45,   0.0, 0.0, 1.0,  16.0,
            -0.45, -0.45,   1.0, 1.0, 0.0,  32.0);

      Attrib_Pos : constant Attribute := Program.Attrib_Location ("position");
      Attrib_Col : constant Attribute := Program.Attrib_Location ("color");
      Attrib_Sid : constant Attribute := Program.Attrib_Location ("sides");

      package Single_Pointers is new GL.Objects.Buffers.Buffer_Pointers
        (Single_Pointers);
   begin
      --  Bind VBO and upload vertices to GPU
      Single_Pointers.Load_To_Buffer (Buffer_Points, Vertices, Static_Draw);

      --  Enable and set attributes for Array_Points VAO
      Array_Points.Enable_Attribute (Attrib_Pos);
      Array_Points.Enable_Attribute (Attrib_Col);
      Array_Points.Enable_Attribute (Attrib_Sid);

      Array_Points.Set_Attribute_Format (Attrib_Pos, 2, Single_Type, 0);
      Array_Points.Set_Attribute_Format (Attrib_Col, 3, Single_Type, 2);
      Array_Points.Set_Attribute_Format (Attrib_Sid, 1, Single_Type, 5);

      Array_Points.Set_Attribute_Binding (Attrib_Pos, 0);
      Array_Points.Set_Attribute_Binding (Attrib_Col, 0);
      Array_Points.Set_Attribute_Binding (Attrib_Sid, 0);

      Array_Points.Bind_Vertex_Buffer (0, Buffer_Points, Single_Type, 0, 6);
   end Load_Data;

   Vertex_Shader   : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Vertex_Shader);
   Geometry_Shader : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Geometry_Shader);
   Fragment_Shader : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Fragment_Shader);
   Program         : GL.Objects.Programs.Program;

   Vector_Buffer_Points : GL.Objects.Buffers.Buffer;
   Array_Points : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

   Vertex_Source    : constant String := "../examples/gl/shaders/geometry.vert";
   Geometry_Source  : constant String := "../examples/gl/shaders/geometry.geom";
   Fragment_Source  : constant String := "../examples/gl/shaders/geometry.frag";
begin
   Display_Backend.Init (Major => 3, Minor => 2);
   Display_Backend.Set_Not_Resizable;

   Display_Backend.Open_Window (Width => 500, Height => 500);
   Ada.Text_IO.Put_Line ("Initialized GLFW window");

   -- Generate shaders and program
   Vertex_Shader.Initialize_Id;
   Geometry_Shader.Initialize_Id;
   Fragment_Shader.Initialize_Id;
   Program.Initialize_Id;

   -- Generate VBO and VAO
   Vector_Buffer_Points.Initialize_Id;
   Array_Points.Initialize_Id;

   Ada.Text_IO.Put_Line ("Initialized objects");

   -- Compile shaders and attach them to the programs
   Load_Shaders (Vertex_Source, Geometry_Source, Fragment_Source,
                 Vertex_Shader, Geometry_Shader, Fragment_Shader, Program);
   Program.Use_Program;

   -- Upload vertices to GPU
   Load_Data (Array_Points, Vector_Buffer_Points, Program);

   Ada.Text_IO.Put_Line ("Loaded data");

   while not Display_Backend.Get_Window.Should_Close loop
      Set_Color_Clear_Value (Color'(0.0, 0.0, 0.0, 1.0));
      Clear (Buffer_Bits'(Color => True, others => False));

      Array_Points.Bind;
      GL.Drawing.Draw_Arrays (Points, 0, 4);

      -- Swap front and back buffers and process events
      Display_Backend.Swap_Buffers_And_Poll_Events;
   end loop;

   Display_Backend.Shutdown;
end GL_Test.Geometry;
