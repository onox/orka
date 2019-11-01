--  SPDX-License-Identifier: Apache-2.0
--
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

with GL.Buffers;
with GL.Drawing;
with GL.Low_Level.Enums;
with GL.Objects.Samplers;
with GL.Objects.Textures;
with GL.Pixels;
with GL.Rasterization;
with GL.Types;
with GL.Toggles;
with GL.Viewports;

with Orka.Rendering.Buffers;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Programs.Uniforms;
with Orka.Rendering.Vertex_Formats;
with Orka.Resources.Locations.Directories;
with Orka.Transforms.Singles.Matrices;
with Orka.Types;

with GL_Test.Display_Backend;

procedure Orka_Test.Test_12_Stencil is
   Width  : constant := 500;
   Height : constant := 500;

   Initialized : constant Boolean := GL_Test.Display_Backend.Init
     (Major => 3, Minor => 2, Width => Width, Height => Height, Resizable => False);
   pragma Unreferenced (Initialized);

   package Transforms renames Orka.Transforms.Singles.Matrices;

   use GL.Buffers;
   use GL.Types;

   use Orka.Rendering.Buffers;
   use Orka.Rendering.Programs;
   use Orka.Rendering.Framebuffers;
   use Orka.Rendering.Vertex_Formats;

   Indices_Screen : constant UInt_Array
     := (1, 0, 2, 2, 0, 3);

   function Load_Scene_Data (Program : Orka.Rendering.Programs.Program) return Vertex_Format is
      use all type Orka.Types.Element_Type;

      V1 : constant Single_Array := (-0.5,  0.5,  0.5);
      V2 : constant Single_Array := (+0.5,  0.5,  0.5);
      V3 : constant Single_Array := (-0.5, -0.5,  0.5);
      V4 : constant Single_Array := (+0.5, -0.5,  0.5);

      V5 : constant Single_Array := (-0.5,  0.5, -0.5);
      V6 : constant Single_Array := (+0.5,  0.5, -0.5);
      V7 : constant Single_Array := (-0.5, -0.5, -0.5);
      V8 : constant Single_Array := (+0.5, -0.5, -0.5);

      C1 : constant Single_Array := (0.0, 0.0, 1.0);
      C2 : constant Single_Array := (1.0, 0.0, 1.0);
      C3 : constant Single_Array := (1.0, 0.0, 0.0);
      C4 : constant Single_Array := (0.0, 1.0, 0.0);

      C5 : constant Single_Array := (0.0, 1.0, 0.0);
      C6 : constant Single_Array := (1.0, 1.0, 0.0);
      C7 : constant Single_Array := (1.0, 1.0, 0.0);
      C8 : constant Single_Array := (0.0, 1.0, 1.0);

      Vertices : constant Single_Array
            -- Back
        := (V2 (0), V2 (1), V2 (2),   C2 (0), C2 (1), C2 (2),   1.0, 0.0,
            V6 (0), V6 (1), V6 (2),   C6 (0), C6 (1), C6 (2),   1.0, 1.0,
            V1 (0), V1 (1), V1 (2),   C1 (0), C1 (1), C1 (2),   0.0, 0.0,
            V1 (0), V1 (1), V1 (2),   C1 (0), C1 (1), C1 (2),   0.0, 0.0,
            V6 (0), V6 (1), V6 (2),   C6 (0), C6 (1), C6 (2),   1.0, 1.0,
            V5 (0), V5 (1), V5 (2),   C5 (0), C5 (1), C5 (2),   0.0, 1.0,

            -- Top
            V4 (0), V4 (1), V4 (2),   C4 (0), C4 (1), C4 (2),   1.0, 0.0,
            V2 (0), V2 (1), V2 (2),   C2 (0), C2 (1), C2 (2),   1.0, 1.0,
            V3 (0), V3 (1), V3 (2),   C3 (0), C3 (1), C3 (2),   0.0, 0.0,
            V3 (0), V3 (1), V3 (2),   C3 (0), C3 (1), C3 (2),   0.0, 0.0,
            V2 (0), V2 (1), V2 (2),   C2 (0), C2 (1), C2 (2),   1.0, 1.0,
            V1 (0), V1 (1), V1 (2),   C1 (0), C1 (1), C1 (2),   0.0, 1.0,

            -- Front
            V8 (0), V8 (1), V8 (2),   C8 (0), C8 (1), C8 (2),   1.0, 0.0,
            V4 (0), V4 (1), V4 (2),   C4 (0), C4 (1), C4 (2),   1.0, 1.0,
            V7 (0), V7 (1), V7 (2),   C7 (0), C7 (1), C7 (2),   0.0, 0.0,
            V7 (0), V7 (1), V7 (2),   C7 (0), C7 (1), C7 (2),   0.0, 0.0,
            V4 (0), V4 (1), V4 (2),   C4 (0), C4 (1), C4 (2),   1.0, 1.0,
            V3 (0), V3 (1), V3 (2),   C3 (0), C3 (1), C3 (2),   0.0, 1.0,

            -- Right
            V6 (0), V6 (1), V6 (2),   C6 (0), C6 (1), C6 (2),   1.0, 0.0,
            V2 (0), V2 (1), V2 (2),   C2 (0), C2 (1), C2 (2),   1.0, 1.0,
            V8 (0), V8 (1), V8 (2),   C8 (0), C8 (1), C8 (2),   0.0, 0.0,
            V8 (0), V8 (1), V8 (2),   C8 (0), C8 (1), C8 (2),   0.0, 0.0,
            V2 (0), V2 (1), V2 (2),   C2 (0), C2 (1), C2 (2),   1.0, 1.0,
            V4 (0), V4 (1), V4 (2),   C4 (0), C4 (1), C4 (2),   0.0, 1.0,

            -- Left
            V7 (0), V7 (1), V7 (2),   C7 (0), C7 (1), C7 (2),   1.0, 0.0,
            V3 (0), V3 (1), V3 (2),   C3 (0), C3 (1), C3 (2),   1.0, 1.0,
            V5 (0), V5 (1), V5 (2),   C5 (0), C5 (1), C5 (2),   0.0, 0.0,
            V5 (0), V5 (1), V5 (2),   C5 (0), C5 (1), C5 (2),   0.0, 0.0,
            V3 (0), V3 (1), V3 (2),   C3 (0), C3 (1), C3 (2),   1.0, 1.0,
            V1 (0), V1 (1), V1 (2),   C1 (0), C1 (1), C1 (2),   0.0, 1.0,

            -- Floor
            -1.0, -1.0, -0.5,    0.2, 0.2, 0.2,   0.0, 0.0,
             1.0, -1.0, -0.5,    0.2, 0.2, 0.2,   0.0, 0.0,
             1.0,  1.0, -0.5,    0.2, 0.2, 0.2,   0.0, 0.0,
             1.0,  1.0, -0.5,    0.2, 0.2, 0.2,   0.0, 0.0,
            -1.0,  1.0, -0.5,    0.2, 0.2, 0.2,   0.0, 0.0,
            -1.0, -1.0, -0.5,    0.2, 0.2, 0.2,   0.0, 0.0);

      --  Create buffer containing attributes
      Buffer_1 : constant Buffer := Create_Buffer ((others => False), Vertices);

      procedure Add_Vertex_Attributes (Buffer : in out Attribute_Buffer) is
      begin
         Buffer.Add_Attribute (Program.Attribute_Location ("position"), 3);
         Buffer.Add_Attribute (Program.Attribute_Location ("color"), 3);
         Buffer.Add_Attribute (Program.Attribute_Location ("texcoord"), 2);
         Buffer.Set_Buffer (Buffer_1);
      end Add_Vertex_Attributes;
   begin
      return Result : Vertex_Format := Create_Vertex_Format (Triangles, UInt_Type) do
         Result.Add_Attribute_Buffer (Single_Type, Add_Vertex_Attributes'Access);
      end return;
   end Load_Scene_Data;

   function Load_Screen_Data (Program : Orka.Rendering.Programs.Program) return Vertex_Format is
      use all type Orka.Types.Element_Type;

      Vertices : constant Single_Array
        := (-1.0,  1.0, 0.0, 1.0,
             1.0,  1.0, 1.0, 1.0,
             1.0, -1.0, 1.0, 0.0,
            -1.0, -1.0, 0.0, 0.0);

      --  Create buffers containing attributes and indices
      Buffer_1 : constant Buffer := Create_Buffer ((others => False), Vertices);
      Buffer_2 : constant Buffer := Create_Buffer ((others => False), Indices_Screen);

      procedure Add_Vertex_Attributes (Buffer : in out Attribute_Buffer) is
      begin
         Buffer.Add_Attribute (Program.Attribute_Location ("position"), 2);
         Buffer.Add_Attribute (Program.Attribute_Location ("texcoord"), 2);
         Buffer.Set_Buffer (Buffer_1);
      end Add_Vertex_Attributes;
   begin
      return Result : Vertex_Format := Create_Vertex_Format (Triangles, UInt_Type) do
         Result.Add_Attribute_Buffer (Single_Type, Add_Vertex_Attributes'Access);
         Result.Set_Index_Buffer (Buffer_2);
      end return;
   end Load_Screen_Data;

   procedure Load_Texture (Texture : in out GL.Objects.Textures.Texture) is
      Pixels : aliased constant Single_Array
        := (0.1, 0.1, 0.1,   1.0, 1.0, 1.0,   0.1, 0.1, 0.1,   1.0, 1.0, 1.0,
            1.0, 1.0, 1.0,   0.1, 0.1, 0.1,   1.0, 1.0, 1.0,   0.1, 0.1, 0.1,
            0.1, 0.1, 0.1,   1.0, 1.0, 1.0,   0.1, 0.1, 0.1,   1.0, 1.0, 1.0,
            1.0, 1.0, 1.0,   0.1, 0.1, 0.1,   1.0, 1.0, 1.0,   0.1, 0.1, 0.1);
   begin
      Texture.Allocate_Storage (1, 1, GL.Pixels.RGB32F, 4, 4, 1);
      Texture.Load_From_Data (0, 0, 0, 0, 4, 4, 1,
        GL.Pixels.RGB, GL.Pixels.Float, Pixels'Address);
   end Load_Texture;

   Scene_Texture : GL.Objects.Textures.Texture (GL.Low_Level.Enums.Texture_2D);
   Color_Texture : GL.Objects.Textures.Texture (GL.Low_Level.Enums.Texture_2D);
   Depth_Texture : GL.Objects.Textures.Texture (GL.Low_Level.Enums.Texture_2D);

   Sampler_1 : GL.Objects.Samplers.Sampler;

   use Orka.Resources;

   Location_Shaders : constant Locations.Location_Ptr
     := Locations.Directories.Create_Location ("../examples/gl/shaders");

   Program_Scene : Program := Create_Program (Modules.Create_Module
     (Location_Shaders, VS => "buffers_scene.vert", FS => "buffers_scene.frag"));

   Program_Screen : Program := Create_Program (Modules.Create_Module
     (Location_Shaders, VS => "buffers_screen.vert", FS => "buffers_screen.frag"));

   Uni_Model  : constant Uniforms.Uniform := Program_Scene.Uniform ("model");
   Uni_View   : constant Uniforms.Uniform := Program_Scene.Uniform ("view");
   Uni_Proj   : constant Uniforms.Uniform := Program_Scene.Uniform ("proj");
   Uni_Color  : constant Uniforms.Uniform := Program_Scene.Uniform ("overrideColor");

   Uni_FB     : constant Uniforms.Uniform := Program_Screen.Uniform ("texFrameBuffer");
   Uni_Effect : constant Uniforms.Uniform := Program_Screen.Uniform ("effect");

   VAO_Scene  : constant Vertex_Format := Load_Scene_Data (Program_Scene);
   VAO_Screen : constant Vertex_Format := Load_Screen_Data (Program_Screen);

   FB_1 : constant Framebuffer_Ptr
     := new Framebuffer'(Create_Framebuffer (Width, Height));
   FB_D : constant Framebuffer_Ptr
     := new Framebuffer'(Create_Default_Framebuffer (Width, Height));

   use all type GL.Objects.Textures.Minifying_Function;
   use all type GL.Objects.Textures.Wrapping_Mode;
begin
   FB_1.Set_Default_Values ((Color => (0.0, 0.0, 0.0, 1.0), Depth => 1.0, others => <>));
   FB_D.Set_Default_Values ((Color => (0.0, 0.0, 0.0, 1.0), Depth => 1.0, others => <>));

   GL.Viewports.Set_Clipping (GL.Viewports.Lower_Left, GL.Viewports.Zero_To_One);
   GL.Toggles.Enable (GL.Toggles.Cull_Face);

   Sampler_1.Set_X_Wrapping (Clamp_To_Edge);
   Sampler_1.Set_Y_Wrapping (Clamp_To_Edge);

   Sampler_1.Set_Minifying_Filter (Nearest);
   Sampler_1.Set_Magnifying_Filter (Nearest);

   Sampler_1.Bind (0);

   --  Load checkerboard texture
   Load_Texture (Scene_Texture);

   Color_Texture.Allocate_Storage (1, 1, GL.Pixels.RGBA8, Width, Height, 1);
   Depth_Texture.Allocate_Storage (1, 1, GL.Pixels.Depth32F_Stencil8, Width, Height, 1);

   FB_1.Attach (Color_Texture);
   FB_1.Attach (Depth_Texture);

   --  Use post-processing program
   Uni_FB.Set_Int (0);

   --  Projection matrix
   Uni_Proj.Set_Matrix (Transforms.Infinite_Perspective (45.0, 1.0, 0.1));

   Ada.Text_IO.Put_Line ("Loaded textures and buffers");

   Ada.Text_IO.Put_Line ("Usage: Right click and drag mouse to move camera around cube.");
   Ada.Text_IO.Put_Line ("Usage: Use scroll wheel to zoom in and out.");
   Ada.Text_IO.Put_Line ("Usage: Press space key to cycle between post-processing effects.");

   declare
      Mouse_X, Mouse_Y, Mouse_Z : Single;
   begin
      while not GL_Test.Display_Backend.Get_Window.Should_Close loop
         Mouse_X := Single (GL_Test.Display_Backend.Get_Mouse_X);
         Mouse_Y := Single (GL_Test.Display_Backend.Get_Mouse_Y);

         if GL_Test.Display_Backend.Get_Zoom_Distance > 10.0 then
            GL_Test.Display_Backend.Set_Zoom_Distance (10.0);
         end if;
         Mouse_Z := Single (GL_Test.Display_Backend.Get_Zoom_Distance);

         --  View matrix
         declare
            Matrix_View : Transforms.Matrix4 := Transforms.Identity_Value;
         begin
            Transforms.Rotate_Z_At_Origin (Matrix_View, Mouse_X);
            Transforms.Rotate_X_At_Origin (Matrix_View, Mouse_Y);
            Transforms.Translate (Matrix_View, (0.0, 0.0, -Mouse_Z, 0.0));

            Uni_View.Set_Matrix (Matrix_View);
         end;

         Uni_Effect.Set_Int (Int (GL_Test.Display_Backend.Get_Effect (5)));

         ---------------------------------------------------------------
         --                           Cube                            --
         ---------------------------------------------------------------

         FB_1.Use_Framebuffer;
         FB_1.Clear;

         --  Model matrix
         Uni_Model.Set_Matrix (Transforms.Identity_Value);

         VAO_Scene.GL_Vertex_Array.Bind;
         GL.Toggles.Enable (GL.Toggles.Depth_Test);

         Program_Scene.Use_Program;
         Scene_Texture.Bind_Texture_Unit (0);

         GL.Drawing.Draw_Arrays (Triangles, 0, 30);

         ---------------------------------------------------------------
         --                           Floor                           --
         ---------------------------------------------------------------

         GL.Toggles.Enable (GL.Toggles.Stencil_Test);

         -- Set any stencil to 1
         Set_Stencil_Function (GL.Rasterization.Front_And_Back, Always, 1, 16#FF#);
         Set_Stencil_Operation (GL.Rasterization.Front_And_Back, Keep, Keep, Replace);
         Set_Stencil_Mask (16#FF#);  -- Allow writing to stencil buffer

         --  Disable writing to the depth buffer in order to prevent the
         --  floor from hiding the reflection cube
         Set_Depth_Mask (False);
         FB_1.Clear ((Stencil => True, others => False));

         GL.Drawing.Draw_Arrays (Triangles, 30, 6);

         ---------------------------------------------------------------
         --                      Reflection cube                      --
         ---------------------------------------------------------------

         -- Pass test if stencil value is 1
         Set_Stencil_Function (GL.Rasterization.Front_And_Back, Equal, 1, 16#FF#);
         Set_Stencil_Mask (16#00#);  -- Don't write anything to stencil buffer
         Set_Depth_Mask (True);

         --  Start drawing reflection cube
         Uni_Model.Set_Matrix (Transforms.T ((0.0, 0.0, -1.0, 0.0)));

         Uni_Color.Set_Vector (Transforms.Vector4'(0.3, 0.3, 0.3, 0.0));
         GL.Drawing.Draw_Arrays (Triangles, 0, 30);
         --  End drawing reflection cube

         Uni_Color.Set_Vector (Transforms.Vector4'(1.0, 1.0, 1.0, 0.0));

         GL.Toggles.Disable (GL.Toggles.Stencil_Test);

         ---------------------------------------------------------------
         --                      Post-processing                      --
         ---------------------------------------------------------------

         FB_D.Use_Framebuffer;
         FB_D.Clear;

         VAO_Screen.GL_Vertex_Array.Bind;
         GL.Toggles.Disable (GL.Toggles.Depth_Test);

         Program_Screen.Use_Program;
         Color_Texture.Bind_Texture_Unit (0);

         GL.Drawing.Draw_Elements (Triangles, Indices_Screen'Length, UInt_Type, 0);

         --  Swap front and back buffers and process events
         GL_Test.Display_Backend.Swap_Buffers_And_Poll_Events;
      end loop;
   end;

   GL_Test.Display_Backend.Shutdown;
end Orka_Test.Test_12_Stencil;
