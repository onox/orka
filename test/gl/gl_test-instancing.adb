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
with GL.Objects.Programs.Uniforms;
with GL.Objects.Vertex_Arrays;
with GL.Types;
with GL.Transforms;
with GL.Toggles;

with GL_Test.Display_Backend;

procedure GL_Test.Instancing is
   use GL.Buffers;
   use GL.Objects.Vertex_Arrays;
   use GL.Types;
   use GL.Transforms;

   procedure Load_Vectors is new GL.Objects.Buffers.Load_To_Buffer
     (Single_Pointers);
   procedure Load_Indices is new GL.Objects.Buffers.Load_To_Buffer
     (UInt_Pointers);
   procedure Load_Matrices is new GL.Objects.Buffers.Load_To_Buffer
     (Singles.Matrix4_Pointers);

   Instances_Dimension : constant := 40;
   Space_Between_Cubes : constant := 0.2;

   Matrices : Singles.Matrix4_Array (1 .. Instances_Dimension**3) := (others => Singles.Identity4);

   Indices : constant UInt_Array
     := (0, 1, 2,  --  Front
         2, 3, 0,
         0, 1, 5,  --  Top
         5, 0, 4,
         4, 5, 6,  --  Back
         6, 7, 4,
         1, 5, 6,  --  Right
         6, 2, 1,
         0, 4, 7,  --  Left
         7, 0, 3,
         3, 2, 7,  --  Bottom
         7, 6, 2);

   procedure Load_Data (Array1  : Vertex_Array_Object;
                        Buffer1, Buffer2, Buffer3 : GL.Objects.Buffers.Buffer;
                        Program : GL.Objects.Programs.Program) is
      use GL.Objects.Buffers;
      use GL.Attributes;

      Vertices : constant Single_Array
        := (-0.5,  0.5, -0.5, 0.0, 0.0, 1.0,
             0.5,  0.5, -0.5, 1.0, 0.0, 0.0,
             0.5, -0.5, -0.5, 1.0, 1.0, 1.0,
            -0.5, -0.5, -0.5, 0.0, 1.0, 0.0,

            -0.5,  0.5,  0.5, 1.0, 0.0, 0.0,
             0.5,  0.5,  0.5, 0.0, 1.0, 0.0,
             0.5, -0.5,  0.5, 0.0, 0.0, 1.0,
            -0.5, -0.5,  0.5, 1.0, 1.0, 1.0);

      Attrib_Pos   : constant Attribute := Program.Attrib_Location ("in_Position");
      Attrib_Color : constant Attribute := Program.Attrib_Location ("in_Color");
      Attrib_Model : constant Attribute := Program.Attrib_Location ("in_Model");
   begin
      --  Upload Vertices data to Buffer1
      Load_Vectors (Buffer1, Vertices, Static_Draw);

      --  Enable and set attributes for Array1 VAO
      Array1.Enable_Attribute (Attrib_Pos);
      Array1.Enable_Attribute (Attrib_Color);

      Array1.Set_Attribute_Format (Attrib_Pos, 3, Single_Type, 0);
      Array1.Set_Attribute_Format (Attrib_Color, 3, Single_Type, 3);

      Array1.Set_Attribute_Binding (Attrib_Pos, 0);
      Array1.Set_Attribute_Binding (Attrib_Color, 0);

      Array1.Bind_Vertex_Buffer (0, Buffer1, Single_Type, 0, 6);

      --  Upload Indices data to Buffer2
      Load_Indices (Buffer2, Indices, Static_Draw);

      Array1.Bind_Element_Buffer (Buffer2);

      declare
         Index : Int := 1;

         X, Y, Z : Single;
         Distance_Multiplier : constant := 1.0 + Space_Between_Cubes;
      begin
         for Index_X in 1 .. Instances_Dimension loop
            X := Single (Index_X) * Distance_Multiplier;
            for Index_Y in 1 .. Instances_Dimension loop
               Y := Single (Index_Y) * Distance_Multiplier;
               for Index_Z in 1 .. Instances_Dimension loop
                  Z := Single (Index_Z) * Distance_Multiplier;

                  Translate (Matrices (Index), (X, Y, Z));
                  Index := Index + 1;
               end loop;
            end loop;
         end loop;
      end;
      --  Load matrices for all the instances
      Load_Matrices (Buffer3, Matrices, Static_Draw);

      --  Enable and set attributes for Array1 VAO
      Array1.Enable_Attribute (Attrib_Model);
      Array1.Enable_Attribute (Attrib_Model + 1);
      Array1.Enable_Attribute (Attrib_Model + 2);
      Array1.Enable_Attribute (Attrib_Model + 3);

      Array1.Set_Attribute_Format (Attrib_Model, 4, Single_Type, 0);
      Array1.Set_Attribute_Format (Attrib_Model + 1, 4, Single_Type, 4);
      Array1.Set_Attribute_Format (Attrib_Model + 2, 4, Single_Type, 8);
      Array1.Set_Attribute_Format (Attrib_Model + 3, 4, Single_Type, 12);

      Array1.Set_Attribute_Binding (Attrib_Model, 1);
      Array1.Set_Attribute_Binding (Attrib_Model + 1, 1);
      Array1.Set_Attribute_Binding (Attrib_Model + 2, 1);
      Array1.Set_Attribute_Binding (Attrib_Model + 3, 1);

      Array1.Set_Attribute_Binding_Divisor (1, 1);

      Array1.Bind_Vertex_Buffer (1, Buffer3, Single_Type, 0, 16);
   end Load_Data;

   procedure Load_Shaders (Vertex_Shader, Fragment_Shader : GL.Objects.Shaders.Shader;
                           Program : GL.Objects.Programs.Program) is
   begin
      -- load shader sources and compile shaders
      GL.Files.Load_Shader_Source_From_File
        (Vertex_Shader, "../test/gl/shaders/instancing.vert");
      GL.Files.Load_Shader_Source_From_File
        (Fragment_Shader, "../test/gl/shaders/instancing.frag");
      
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
      
      -- Set up program
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
   end Load_Shaders;

   Vertex_Shader   : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Vertex_Shader);
   Fragment_Shader : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Fragment_Shader);
   Program         : GL.Objects.Programs.Program;

   Vector_Buffer1, Vector_Buffer2, Vector_Buffer3 : GL.Objects.Buffers.Buffer;
   Array1 : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

   Uni_View, Uni_Proj : GL.Objects.Programs.Uniforms.Uniform;
begin
   Display_Backend.Init (Major => 3, Minor => 3);
   Display_Backend.Set_Not_Resizable;
   Display_Backend.Open_Window (Width => 500, Height => 500);
   Ada.Text_IO.Put_Line ("Initialized GLFW window");

   Vertex_Shader.Initialize_Id;
   Fragment_Shader.Initialize_Id;
   Program.Initialize_Id;

   Vector_Buffer1.Initialize_Id;
   Vector_Buffer2.Initialize_Id;
   Vector_Buffer3.Initialize_Id;
   Array1.Initialize_Id;

   Ada.Text_IO.Put_Line ("Initialized objects");

   Load_Shaders (Vertex_Shader, Fragment_Shader, Program);
   Ada.Text_IO.Put_Line ("Loaded shaders");

   Load_Data (Array1, Vector_Buffer1, Vector_Buffer2, Vector_Buffer3, Program);
   Ada.Text_IO.Put_Line ("Loaded data");

   declare
      Matrix_View, Matrix_Proj : Singles.Matrix4;
      Mouse_X, Mouse_Y, Mouse_Z, Distance_Center : Single;

      Clamping_Was_Enabled : Boolean := False;
      Clamping_Enabled : Boolean := True;
   begin
      Distance_Center := Single (Instances_Dimension);
      Distance_Center := Distance_Center + (Distance_Center - 1.0) * Space_Between_Cubes;
      Distance_Center := Distance_Center / 2.0 - 0.5 + 1.2;

      Display_Backend.Set_Zoom_Distance (100.0);

      Uni_View  := Program.Uniform_Location ("view");
      Uni_Proj  := Program.Uniform_Location ("proj");

      --  Projection matrix
      Matrix_Proj := Perspective (45.0, 1.0, 10.0, 200.0);
      Uni_Proj.Set_Single_Matrix (Matrix_Proj);

      GL.Toggles.Enable (GL.Toggles.Depth_Test);

      Ada.Text_IO.Put_Line ("Instances of cube: " & Positive'Image (Matrices'Length));
      Ada.Text_IO.Put_Line ("Usage: Press space key to enable/disable depth clamping.");

      while not Display_Backend.Get_Window.Should_Close loop
         Clear (Buffer_Bits'(Color => True, Depth => True, others => False));

         Clamping_Enabled := Display_Backend.Get_Effect (2) = 0;
         if Clamping_Enabled then
            GL.Toggles.Enable (GL.Toggles.Depth_Clamp);
         else
            GL.Toggles.Disable (GL.Toggles.Depth_Clamp);
         end if;

         if Clamping_Enabled /= Clamping_Was_Enabled then
            Ada.Text_IO.Put_Line ("Clamping enabled: " & Boolean'Image (Clamping_Enabled));
         end if;
         Clamping_Was_Enabled := Clamping_Enabled;

         Mouse_X := Single (Display_Backend.Get_Mouse_X);
         Mouse_Y := Single (Display_Backend.Get_Mouse_Y);
         Mouse_Z := Single (Display_Backend.Get_Zoom_Distance);

         --  View matrix
         Matrix_View := Singles.Identity4;
         Translate (Matrix_View, (0.0, 0.0, -Mouse_Z));
         Rotate_X (Matrix_View, Mouse_Y);
         Rotate_Y (Matrix_View, Mouse_X);
         Translate (Matrix_View, (-Distance_Center, -Distance_Center, -Distance_Center));

         --  Set uniform
         Uni_View.Set_Single_Matrix (Matrix_View);

         Array1.Bind;
         GL.Drawing.Draw_Elements (Triangles, Indices'Length, UInt_Type, Matrices'Length);

         -- Swap front and back buffers and process events
         Display_Backend.Swap_Buffers_And_Poll_Events;
      end loop;
   end;

   Display_Backend.Shutdown;
end GL_Test.Instancing;
