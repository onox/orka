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

with Interfaces.C;

with Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Drawing;
with GL.Files;
with GL.Low_Level.Enums;
with GL.Pixels;
with GL.Objects.Buffers;
with GL.Objects.Shaders;
with GL.Objects.Programs.Uniforms;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Textures;
with GL.Objects.Framebuffers;
with GL.Objects.Renderbuffers;
with GL.Types.Colors;
with GL.Toggles;
with GL.Transforms;
with GL.Window;

with GL_Test.Display_Backend;

procedure GL_Test.Buffers is
   Initialized : constant Boolean := Display_Backend.Init
     (Major => 3, Minor => 2, Width => 500, Height => 500, Resizable => False);
   pragma Unreferenced (Initialized);

   use GL.Buffers;
   use GL.Types;
   use GL.Types.Colors;
   use GL.Objects.Vertex_Arrays;

   package Single_Pointers is new GL.Objects.Buffers.Buffer_Pointers
     (Single_Pointers);

   procedure Load_Scene_Data (Array_Cube  : Vertex_Array_Object;
                              Buffer_Cube : GL.Objects.Buffers.Buffer;
                              Program     : GL.Objects.Programs.Program) is
      use GL.Objects.Buffers;
      use GL.Attributes;

      Vertices : constant Single_Array
            -- Top
        := (-0.5, -0.5,  0.5,   1.0, 0.0, 0.0,   0.0, 0.0,
             0.5, -0.5,  0.5,   0.0, 1.0, 0.0,   1.0, 0.0,
             0.5,  0.5,  0.5,   0.0, 0.0, 1.0,   1.0, 1.0,
             0.5,  0.5,  0.5,   0.0, 0.0, 1.0,   1.0, 1.0,
            -0.5,  0.5,  0.5,   0.0, 0.0, 1.0,   0.0, 1.0,
            -0.5, -0.5,  0.5,   1.0, 0.0, 0.0,   0.0, 0.0,

            -- Right
             0.5, -0.5, -0.5,   1.0, 0.0, 0.0,   0.0, 0.0,
             0.5,  0.5, -0.5,   0.0, 1.0, 0.0,   1.0, 0.0,
             0.5,  0.5,  0.5,   0.0, 0.0, 1.0,   1.0, 1.0,
             0.5,  0.5,  0.5,   0.0, 0.0, 1.0,   1.0, 1.0,
             0.5, -0.5,  0.5,   0.0, 0.0, 1.0,   0.0, 1.0,
             0.5, -0.5, -0.5,   1.0, 0.0, 0.0,   0.0, 0.0,

            -- Left
            -0.5, -0.5, -0.5,   0.0, 1.0, 0.0,   1.0, 0.0,
            -0.5,  0.5, -0.5,   1.0, 0.0, 0.0,   1.0, 1.0,
            -0.5,  0.5,  0.5,   0.0, 0.0, 1.0,   0.0, 1.0,
            -0.5,  0.5,  0.5,   0.0, 0.0, 1.0,   0.0, 1.0,
            -0.5, -0.5,  0.5,   0.0, 0.0, 1.0,   0.0, 0.0,
            -0.5, -0.5, -0.5,   0.0, 1.0, 0.0,   1.0, 0.0,

            -- Front
            -0.5, -0.5, -0.5,   1.0, 0.0, 0.0,   0.0, 0.0,
             0.5, -0.5, -0.5,   0.0, 1.0, 0.0,   1.0, 0.0,
             0.5, -0.5,  0.5,   0.0, 0.0, 1.0,   1.0, 1.0,
             0.5, -0.5,  0.5,   0.0, 0.0, 1.0,   1.0, 1.0,
            -0.5, -0.5,  0.5,   0.0, 0.0, 1.0,   0.0, 1.0,
            -0.5, -0.5, -0.5,   1.0, 0.0, 0.0,   0.0, 0.0,

            -- Back
            -0.5,  0.5, -0.5,   0.0, 1.0, 0.0,   1.0, 0.0,
             0.5,  0.5, -0.5,   1.0, 0.0, 0.0,   1.0, 1.0,
             0.5,  0.5,  0.5,   0.0, 0.0, 1.0,   0.0, 1.0,
             0.5,  0.5,  0.5,   0.0, 0.0, 1.0,   0.0, 1.0,
            -0.5,  0.5,  0.5,   0.0, 0.0, 1.0,   0.0, 0.0,
            -0.5,  0.5, -0.5,   0.0, 1.0, 0.0,   1.0, 0.0,

            -- Floor
            -1.0, -1.0, -0.5,    0.2, 0.2, 0.2,   0.0, 0.0,
             1.0, -1.0, -0.5,    0.2, 0.2, 0.2,   0.0, 0.0,
             1.0,  1.0, -0.5,    0.2, 0.2, 0.2,   0.0, 0.0,
             1.0,  1.0, -0.5,    0.2, 0.2, 0.2,   0.0, 0.0,
            -1.0,  1.0, -0.5,    0.2, 0.2, 0.2,   0.0, 0.0,
            -1.0, -1.0, -0.5,    0.2, 0.2, 0.2,   0.0, 0.0);

      Attrib_Pos : constant Attribute := Program.Attrib_Location ("position");
      Attrib_Col : constant Attribute := Program.Attrib_Location ("color");
      Attrib_Tex : constant Attribute := Program.Attrib_Location ("texcoord");
   begin
      --  Upload Vertices data to Buffer_Cube
      Single_Pointers.Load_To_Immutable_Buffer (Buffer_Cube, Vertices, Storage_Bits'(others => False));

      --  Enable and set attributes for Array_Cube VAO
      Array_Cube.Enable_Attribute (Attrib_Pos);
      Array_Cube.Enable_Attribute (Attrib_Col);
      Array_Cube.Enable_Attribute (Attrib_Tex);

      Array_Cube.Set_Attribute_Format (Attrib_Pos, 3, Single_Type, 0);
      Array_Cube.Set_Attribute_Format (Attrib_Col, 3, Single_Type, 3);
      Array_Cube.Set_Attribute_Format (Attrib_Tex, 2, Single_Type, 6);

      Array_Cube.Set_Attribute_Binding (Attrib_Pos, 0);
      Array_Cube.Set_Attribute_Binding (Attrib_Col, 0);
      Array_Cube.Set_Attribute_Binding (Attrib_Tex, 0);

      Array_Cube.Bind_Vertex_Buffer (0, Buffer_Cube, Single_Type, 0, 8);
   end Load_Scene_Data;

   procedure Load_Screen_Data (Array_Quad  : Vertex_Array_Object;
                               Buffer_Quad : GL.Objects.Buffers.Buffer;
                               Program     : GL.Objects.Programs.Program) is
      use GL.Objects.Buffers;
      use GL.Attributes;

      Vertices : constant Single_Array
        := (-1.0,  1.0, 0.0, 1.0,
             1.0,  1.0, 1.0, 1.0,
             1.0, -1.0, 1.0, 0.0,

             1.0, -1.0, 1.0, 0.0,
            -1.0, -1.0, 0.0, 0.0,
            -1.0,  1.0, 0.0, 1.0);

      Attrib_Pos : constant Attribute := Program.Attrib_Location ("position");
      Attrib_Tex : constant Attribute := Program.Attrib_Location ("texcoord");
   begin
      --  Upload Vertices data to Buffer_Quad
      Single_Pointers.Load_To_Immutable_Buffer (Buffer_Quad, Vertices, Storage_Bits'(others => False));

      --  Enable and set attributes for Array_Quad VAO
      Array_Quad.Enable_Attribute (Attrib_Pos);
      Array_Quad.Enable_Attribute (Attrib_Tex);

      Array_Quad.Set_Attribute_Format (Attrib_Pos, 2, Single_Type, 0);
      Array_Quad.Set_Attribute_Format (Attrib_Tex, 2, Single_Type, 2);

      Array_Quad.Set_Attribute_Binding (Attrib_Pos, 0);
      Array_Quad.Set_Attribute_Binding (Attrib_Tex, 0);

      Array_Quad.Bind_Vertex_Buffer (0, Buffer_Quad, Single_Type, 0, 4);
   end Load_Screen_Data;

   procedure Load_Shaders (Vertex_Source, Fragment_Source : String;
                           Vertex_Shader, Fragment_Shader : GL.Objects.Shaders.Shader;
                           Program : GL.Objects.Programs.Program) is
   begin
      -- Load shader sources and compile shaders
      GL.Files.Load_Shader_Source_From_File
        (Vertex_Shader, Vertex_Source);
      GL.Files.Load_Shader_Source_From_File
        (Fragment_Shader, Fragment_Source);
      
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
      
      -- Set up program
      Program.Attach (Vertex_Shader);
      Program.Attach (Fragment_Shader);

      Program.Link;
      if not Program.Link_Status then
         Ada.Text_IO.Put_Line ("Program linking failed. Log:");
         Ada.Text_IO.Put_Line (Program.Info_Log);
         return;
      end if;
   end Load_Shaders;

   procedure Load_Texture (Texture : in out GL.Objects.Textures.Texture_2D) is
      use GL.Objects.Textures;

      Pixels : aliased constant Single_Array
        := (0.1, 0.1, 0.1,   1.0, 1.0, 1.0,   0.1, 0.1, 0.1,   1.0, 1.0, 1.0,
            1.0, 1.0, 1.0,   0.1, 0.1, 0.1,   1.0, 1.0, 1.0,   0.1, 0.1, 0.1,
            0.1, 0.1, 0.1,   1.0, 1.0, 1.0,   0.1, 0.1, 0.1,   1.0, 1.0, 1.0,
            1.0, 1.0, 1.0,   0.1, 0.1, 0.1,   1.0, 1.0, 1.0,   0.1, 0.1, 0.1);
   begin
      Texture.Bind_Texture_Unit (0);

      Texture.Set_X_Wrapping (Clamp_To_Edge);
      Texture.Set_Y_Wrapping (Clamp_To_Edge);

      Texture.Set_Minifying_Filter (Nearest);
      Texture.Set_Magnifying_Filter (Nearest);

      --  Load texture data
      Texture.Allocate_Storage (1, GL.Pixels.RGB32F, 4, 4);
      Texture.Load_From_Data (0, 0, 0, 4, 4, GL.Pixels.RGB, GL.Pixels.Float, Pixels'Address);
   end Load_Texture;

   procedure Load_Color_Texture (Texture : in out GL.Objects.Textures.Texture_2D) is
      use GL.Objects.Textures;
   begin
      Texture.Bind_Texture_Unit (0);

      Texture.Set_Minifying_Filter (Nearest);
      Texture.Set_Magnifying_Filter (Nearest);

      Texture.Allocate_Storage (1, GL.Pixels.RGB8, 500, 500);
      Texture.Load_Empty_Texture (0, 0, 0, 500, 500);
   end Load_Color_Texture;

   Scene_Vertex_Shader   : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Vertex_Shader);
   Scene_Fragment_Shader : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Fragment_Shader);
   Scene_Program         : GL.Objects.Programs.Program;

   Screen_Vertex_Shader   : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Vertex_Shader);
   Screen_Fragment_Shader : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Fragment_Shader);
   Screen_Program         : GL.Objects.Programs.Program;

   Vector_Buffer_Cube, Vector_Buffer_Quad : GL.Objects.Buffers.Buffer;
   Array_Cube, Array_Quad : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

   Scene_Texture, Color_Texture : GL.Objects.Textures.Texture_2D (GL.Low_Level.Enums.Texture_2D);
   FB : GL.Objects.Framebuffers.Framebuffer;
   RB : GL.Objects.Renderbuffers.Renderbuffer;

   Uni_Model, Uni_View, Uni_Proj, Uni_Color, Uni_Effect : GL.Objects.Programs.Uniforms.Uniform;

   Scene_Vertex_Source    : constant String := "../examples/gl/shaders/buffers_scene.vert";
   Scene_Fragment_Source  : constant String := "../examples/gl/shaders/buffers_scene.frag";
   Screen_Vertex_Source   : constant String := "../examples/gl/shaders/buffers_screen.vert";
   Screen_Fragment_Source : constant String := "../examples/gl/shaders/buffers_screen.frag";
begin
   --  Compile shaders and attach them to the programs
   Load_Shaders (Scene_Vertex_Source, Scene_Fragment_Source,
                 Scene_Vertex_Shader, Scene_Fragment_Shader, Scene_Program);
   Load_Shaders (Screen_Vertex_Source, Screen_Fragment_Source,
                 Screen_Vertex_Shader, Screen_Fragment_Shader, Screen_Program);

   Ada.Text_IO.Put_Line ("Loaded shaders");

   --  Upload vertices to GPU
   Load_Scene_Data (Array_Cube, Vector_Buffer_Cube, Scene_Program);
   Load_Screen_Data (Array_Quad, Vector_Buffer_Quad, Screen_Program);

   Ada.Text_IO.Put_Line ("Loaded data");

   --  Load checkerboard texture
   Load_Texture (Scene_Texture);
   Load_Color_Texture (Color_Texture);

   --  Use post-processing program
   Screen_Program.Uniform_Location ("texFrameBuffer").Set_Int (0);

   -- Attach color texture to frame buffer
   FB.Attach_Texture (GL.Objects.Framebuffers.Color_Attachment_0, Color_Texture, 0);

   -- Create render buffer object for depth and stencil buffers
   RB.Allocate (GL.Pixels.Depth24_Stencil8, 500, 500);
   FB.Attach_Renderbuffer (GL.Objects.Framebuffers.Depth_Stencil_Attachment, RB);

   Ada.Text_IO.Put_Line ("Loaded textures and buffers");

   Uni_Model := Scene_Program.Uniform_Location ("model");
   Uni_View  := Scene_Program.Uniform_Location ("view");
   Uni_Proj  := Scene_Program.Uniform_Location ("proj");

   Uni_Color  := Scene_Program.Uniform_Location ("overrideColor");
   Uni_Effect := Screen_Program.Uniform_Location ("effect");

   Ada.Text_IO.Put_Line ("Loaded uniforms");

   Ada.Text_IO.Put_Line ("Usage: Right click and drag mouse to move camera around cube.");
   Ada.Text_IO.Put_Line ("Usage: Use scroll wheel to zoom in and out.");
   Ada.Text_IO.Put_Line ("Usage: Press space key to cycle between post-processing effects.");

   declare
      Width, Height : Interfaces.C.int;
      Matrix_Model, Matrix_View, Matrix_Proj : Singles.Matrix4;

      Mouse_X, Mouse_Y, Mouse_Z : Single;

      use GL.Transforms;
      use type Singles.Matrix4;
   begin
      while not Display_Backend.Get_Window.Should_Close loop
         Display_Backend.Get_Window.Get_Framebuffer_Size (Width, Height);

         Mouse_X := Single (Display_Backend.Get_Mouse_X);
         Mouse_Y := Single (Display_Backend.Get_Mouse_Y);

         if Display_Backend.Get_Zoom_Distance > 10.0 then
            Display_Backend.Set_Zoom_Distance (10.0);
         end if;
         Mouse_Z := Single (Display_Backend.Get_Zoom_Distance);

         --  Model matrix
         Matrix_Model := Singles.Identity4;

         --  View matrix
         Matrix_View := Singles.Identity4;

         Translate (Matrix_View, (0.0, 0.0, -Mouse_Z));
         Rotate_X (Matrix_View, Mouse_Y);
         Rotate_Z (Matrix_View, Mouse_X);

         --  Projection matrix
         Matrix_Proj := Perspective (45.0, 1.0, 0.1, 20.0);

         --  Set uniforms
         Uni_Model.Set_Single_Matrix (Matrix_Model);
         Uni_View.Set_Single_Matrix (Matrix_View);
         Uni_Proj.Set_Single_Matrix (Matrix_Proj);
         Uni_Effect.Set_Int (Int (Display_Backend.Get_Effect (5)));

         GL.Objects.Framebuffers.Draw_Target.Bind (GL.Objects.Framebuffers.Default_Framebuffer);
         GL.Window.Set_Viewport (0, 0, GL.Types.Int (Width), GL.Types.Int (Height));
         Set_Color_Clear_Value (Color'(1.0, 0.0, 0.0, 1.0));
         Clear (Buffer_Bits'(Color => True, Depth => True, others => False));

         --  Bind frame buffer and draw 3D scene
         GL.Objects.Framebuffers.Draw_Target.Bind (FB);
         Set_Color_Clear_Value (Color'(0.0, 0.0, 0.0, 1.0));
         Clear (Buffer_Bits'(Color => True, Depth => True, others => False));

         Array_Cube.Bind;
         GL.Toggles.Enable (GL.Toggles.Depth_Test);

         Scene_Program.Use_Program;
         Scene_Texture.Bind_Texture_Unit (0);

         ---------------------------------------------------------------

         --  Draw cube
         GL.Drawing.Draw_Arrays (Triangles, 0, 30);

         ---------------------------------------------------------------

         --  Draw floor
         GL.Toggles.Enable (GL.Toggles.Stencil_Test);

         Set_Stencil_Function (Always, 1, 16#FF#);  -- Set any stencil to 1
         Set_Stencil_Operation (Keep, Keep, Replace);
         Set_Stencil_Mask (16#FF#);  -- Allow writing to stencil buffer

         -- Disable writing to the depth buffer in order to prevent the
         -- floor from hiding the reflection cube
         Depth_Mask (False);
         Clear (Buffer_Bits'(Stencil => True, others => False));
         GL.Drawing.Draw_Arrays (Triangles, 30, 6);

         Set_Stencil_Function (Equal, 1, 16#FF#);  -- Pass test if stencil value is 1
         Set_Stencil_Mask (16#00#);  -- Don't write anything to stencil buffer
         Depth_Mask (True);

         --  Start drawing reflection cube
         Translate (Matrix_Model, (0.0, 0.0, -1.0));
         Scale (Matrix_Model, (1.0, 1.0, -1.0));

         Uni_Model.Set_Single_Matrix (Matrix_Model);
         Uni_Color.Set_Singles (0.3, 0.3, 0.3);
         GL.Drawing.Draw_Arrays (Triangles, 0, 30);
         --  End drawing reflection cube

         Uni_Color.Set_Singles (1.0, 1.0, 1.0);

         GL.Toggles.Disable (GL.Toggles.Stencil_Test);

         ---------------------------------------------------------------

         --  Bind default frame buffer
         GL.Objects.Framebuffers.Draw_Target.Bind (GL.Objects.Framebuffers.Default_Framebuffer);

         Array_Quad.Bind;
         GL.Toggles.Disable (GL.Toggles.Depth_Test);

         Screen_Program.Use_Program;
         Color_Texture.Bind_Texture_Unit (0);

         GL.Drawing.Draw_Arrays (Triangles, 0, 6);

         --  Swap front and back buffers and process events
         Display_Backend.Swap_Buffers_And_Poll_Events;
      end loop;
   end;

   Display_Backend.Shutdown;
end GL_Test.Buffers;
