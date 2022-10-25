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

with Ada.Numerics;
with Ada.Text_IO;

with GL.Buffers;
with GL.Objects.Samplers;
with GL.Objects.Textures;
with GL.Pixels;
with GL.Rasterization;
with GL.Types;
with GL.Toggles;

with Orka.Cameras.Rotate_Around_Cameras;
with Orka.Contexts.AWT;
with Orka.Rendering.Buffers;
with Orka.Rendering.Drawing;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Programs.Uniforms;
with Orka.Rendering.Textures;
with Orka.Resources.Locations.Directories;
with Orka.Transforms.Singles.Matrices;
with Orka.Transforms.Doubles.Vector_Conversions;
with Orka.Windows;

with AWT.Inputs;

--  In this example we render a floor, a cube on top of it, and the
--  reflection of the cube on the floor using a stencil buffer.
--
--  These objects are rendered to a multisampled texture in a framebuffer
--  and then blitted to another framebuffer to resolve the samples. The
--  resulting fullscreen texture is then postprocessed and written to the
--  default framebuffer.

procedure Orka_12_Stencil is
   Width   : constant := 500;
   Height  : constant := 500;
   Samples : constant := 8;

   Context : constant Orka.Contexts.Context'Class := Orka.Contexts.AWT.Create_Context
     (Version => (4, 2), Flags  => (Debug => True, others => False));

   Window : constant Orka.Windows.Window'Class
     := Orka.Contexts.AWT.Create_Window (Context, Width, Height, Resizable => False);

   use GL.Buffers;
   use all type GL.Types.Connection_Mode;
   use all type GL.Types.Compare_Function;

   use Orka.Rendering.Buffers;
   use Orka.Rendering.Programs;
   use Orka.Rendering.Framebuffers;
   use Orka;

   Indices_Screen : constant Orka.Unsigned_32_Array
     := (1, 0, 2, 2, 0, 3);

   --  Create buffers containing attributes and indices
   Buffer_2 : constant Buffer := Create_Buffer ((others => False), Indices_Screen);
   --  vec2 position
   --  vec2 texcoord

   function Get_Scene_Data return Float_32_Array is
      V1 : constant Float_32_Array := (-0.5,  0.5,  0.5);
      V2 : constant Float_32_Array := (+0.5,  0.5,  0.5);
      V3 : constant Float_32_Array := (-0.5, -0.5,  0.5);
      V4 : constant Float_32_Array := (+0.5, -0.5,  0.5);

      V5 : constant Float_32_Array := (-0.5,  0.5, -0.5);
      V6 : constant Float_32_Array := (+0.5,  0.5, -0.5);
      V7 : constant Float_32_Array := (-0.5, -0.5, -0.5);
      V8 : constant Float_32_Array := (+0.5, -0.5, -0.5);

      C1 : constant Float_32_Array := (0.0, 0.0, 1.0);
      C2 : constant Float_32_Array := (1.0, 0.0, 1.0);
      C3 : constant Float_32_Array := (1.0, 0.0, 0.0);
      C4 : constant Float_32_Array := (0.0, 1.0, 0.0);

      C5 : constant Float_32_Array := (0.0, 1.0, 0.0);
      C6 : constant Float_32_Array := (1.0, 1.0, 0.0);
      C7 : constant Float_32_Array := (1.0, 1.0, 0.0);
      C8 : constant Float_32_Array := (0.0, 1.0, 1.0);

      Vertices : constant Float_32_Array
            --  Back
        := (V2 (0), V2 (1), V2 (2),   C2 (0), C2 (1), C2 (2),   1.0, 0.0,
            V6 (0), V6 (1), V6 (2),   C6 (0), C6 (1), C6 (2),   1.0, 1.0,
            V1 (0), V1 (1), V1 (2),   C1 (0), C1 (1), C1 (2),   0.0, 0.0,
            V1 (0), V1 (1), V1 (2),   C1 (0), C1 (1), C1 (2),   0.0, 0.0,
            V6 (0), V6 (1), V6 (2),   C6 (0), C6 (1), C6 (2),   1.0, 1.0,
            V5 (0), V5 (1), V5 (2),   C5 (0), C5 (1), C5 (2),   0.0, 1.0,

            --  Top
            V4 (0), V4 (1), V4 (2),   C4 (0), C4 (1), C4 (2),   1.0, 0.0,
            V2 (0), V2 (1), V2 (2),   C2 (0), C2 (1), C2 (2),   1.0, 1.0,
            V3 (0), V3 (1), V3 (2),   C3 (0), C3 (1), C3 (2),   0.0, 0.0,
            V3 (0), V3 (1), V3 (2),   C3 (0), C3 (1), C3 (2),   0.0, 0.0,
            V2 (0), V2 (1), V2 (2),   C2 (0), C2 (1), C2 (2),   1.0, 1.0,
            V1 (0), V1 (1), V1 (2),   C1 (0), C1 (1), C1 (2),   0.0, 1.0,

            --  Front
            V8 (0), V8 (1), V8 (2),   C8 (0), C8 (1), C8 (2),   1.0, 0.0,
            V4 (0), V4 (1), V4 (2),   C4 (0), C4 (1), C4 (2),   1.0, 1.0,
            V7 (0), V7 (1), V7 (2),   C7 (0), C7 (1), C7 (2),   0.0, 0.0,
            V7 (0), V7 (1), V7 (2),   C7 (0), C7 (1), C7 (2),   0.0, 0.0,
            V4 (0), V4 (1), V4 (2),   C4 (0), C4 (1), C4 (2),   1.0, 1.0,
            V3 (0), V3 (1), V3 (2),   C3 (0), C3 (1), C3 (2),   0.0, 1.0,

            --  Right
            V6 (0), V6 (1), V6 (2),   C6 (0), C6 (1), C6 (2),   1.0, 0.0,
            V2 (0), V2 (1), V2 (2),   C2 (0), C2 (1), C2 (2),   1.0, 1.0,
            V8 (0), V8 (1), V8 (2),   C8 (0), C8 (1), C8 (2),   0.0, 0.0,
            V8 (0), V8 (1), V8 (2),   C8 (0), C8 (1), C8 (2),   0.0, 0.0,
            V2 (0), V2 (1), V2 (2),   C2 (0), C2 (1), C2 (2),   1.0, 1.0,
            V4 (0), V4 (1), V4 (2),   C4 (0), C4 (1), C4 (2),   0.0, 1.0,

            --  Left
            V7 (0), V7 (1), V7 (2),   C7 (0), C7 (1), C7 (2),   1.0, 0.0,
            V3 (0), V3 (1), V3 (2),   C3 (0), C3 (1), C3 (2),   1.0, 1.0,
            V5 (0), V5 (1), V5 (2),   C5 (0), C5 (1), C5 (2),   0.0, 0.0,
            V5 (0), V5 (1), V5 (2),   C5 (0), C5 (1), C5 (2),   0.0, 0.0,
            V3 (0), V3 (1), V3 (2),   C3 (0), C3 (1), C3 (2),   1.0, 1.0,
            V1 (0), V1 (1), V1 (2),   C1 (0), C1 (1), C1 (2),   0.0, 1.0,

            --  Floor
            -1.0, -1.0, -0.5,    0.2, 0.2, 0.2,   0.0, 0.0,
             1.0, -1.0, -0.5,    0.2, 0.2, 0.2,   0.0, 0.0,
             1.0,  1.0, -0.5,    0.2, 0.2, 0.2,   0.0, 0.0,
             1.0,  1.0, -0.5,    0.2, 0.2, 0.2,   0.0, 0.0,
            -1.0,  1.0, -0.5,    0.2, 0.2, 0.2,   0.0, 0.0,
            -1.0, -1.0, -0.5,    0.2, 0.2, 0.2,   0.0, 0.0);
   begin
      return Vertices;
   end Get_Scene_Data;

   --  Create buffer containing attributes
   Buffer_0 : constant Buffer := Create_Buffer ((others => False), Get_Scene_Data);
   --  vec3 position
   --  vec3 color
   --  vec2 texcoord

   package Textures renames GL.Objects.Textures;

   procedure Load_Texture (Texture : in out Textures.Texture) is
      Pixels : aliased constant Float_32_Array
        := (0.1, 0.1, 0.1,   1.0, 1.0, 1.0,   0.1, 0.1, 0.1,   1.0, 1.0, 1.0,
            1.0, 1.0, 1.0,   0.1, 0.1, 0.1,   1.0, 1.0, 1.0,   0.1, 0.1, 0.1,
            0.1, 0.1, 0.1,   1.0, 1.0, 1.0,   0.1, 0.1, 0.1,   1.0, 1.0, 1.0,
            1.0, 1.0, 1.0,   0.1, 0.1, 0.1,   1.0, 1.0, 1.0,   0.1, 0.1, 0.1);
   begin
      Texture.Allocate_Storage (1, 1, GL.Pixels.RGB32F, 4, 4, 1);
      Texture.Load_From_Data (0, 0, 0, 0, 4, 4, 1,
        GL.Pixels.RGB, GL.Pixels.Float, Pixels'Address);
   end Load_Texture;

   Scene_Texture       : Textures.Texture (Textures.LE.Texture_2D);
   No_MS_Color_Texture : Textures.Texture (Textures.LE.Texture_2D);

   Color_Texture : Textures.Texture (Textures.LE.Texture_2D_Multisample);
   Depth_Texture : Textures.Texture (Textures.LE.Texture_2D_Multisample);

   Sampler_1 : GL.Objects.Samplers.Sampler;

   use Orka.Resources;

   Location_Shaders : constant Locations.Location_Ptr
     := Locations.Directories.Create_Location ("data/shaders");

   Program_Scene : Program := Create_Program (Modules.Create_Module
     (Location_Shaders, VS => "buffers_scene.vert", FS => "buffers_scene.frag"));

   Program_Screen : Program := Create_Program (Modules.Create_Module
     (Location_Shaders, VS => "buffers_screen.vert", FS => "buffers_screen.frag"));

   Uni_Model  : constant Uniforms.Uniform := Program_Scene.Uniform ("model");
   Uni_View   : constant Uniforms.Uniform := Program_Scene.Uniform ("view");
   Uni_Proj   : constant Uniforms.Uniform := Program_Scene.Uniform ("proj");
   Uni_Color  : constant Uniforms.Uniform := Program_Scene.Uniform ("overrideColor");

   Uni_Effect : constant Uniforms.Uniform := Program_Screen.Uniform ("effect");

   FB_1 : Framebuffer := Create_Framebuffer (Width, Height, Samples);
   FB_2 : Framebuffer := Create_Framebuffer (Width, Height);
   FB_D : Framebuffer := Create_Default_Framebuffer (Width, Height);

   use Orka.Cameras;
   Lens : constant Camera_Lens := Create_Lens (Width, Height, 45.0);
   Current_Camera : Rotate_Around_Cameras.Rotate_Around_Camera :=
     Rotate_Around_Cameras.Create_Camera (Lens);

   use all type GL.Objects.Samplers.Minifying_Function;
   use all type GL.Objects.Samplers.Wrapping_Mode;

   package Transforms renames Orka.Transforms.Singles.Matrices;

   World_TM : constant Transforms.Matrix4 := Transforms.Rx (-0.5 * Ada.Numerics.Pi);

   Default_Distance : constant := -3.0;
   Deg_45           : constant := 0.25 * Ada.Numerics.Pi;

   type Effect_Type is mod 4;

   Effect : Effect_Type := Effect_Type'First;
begin
   Current_Camera.Set_Orientation ((-Deg_45, 0.5 * Deg_45, Default_Distance, 0.0));
   Current_Camera.Update (0.0);

   FB_1.Set_Default_Values ((Color => (1.0, 1.0, 1.0, 1.0), Depth => 0.0, others => <>));
   FB_D.Set_Default_Values ((Color => (0.0, 0.0, 0.0, 1.0), Depth => 0.0, others => <>));

   GL.Toggles.Enable (GL.Toggles.Cull_Face);

   Sampler_1.Set_X_Wrapping (Clamp_To_Edge);
   Sampler_1.Set_Y_Wrapping (Clamp_To_Edge);

   Sampler_1.Set_Minifying_Filter (Nearest);
   Sampler_1.Set_Magnifying_Filter (Nearest);

   Sampler_1.Bind (0);

   --  Load checkerboard texture
   Load_Texture (Scene_Texture);

   Color_Texture.Allocate_Storage (1, Samples, GL.Pixels.RGBA8, Width, Height, 1);
   Depth_Texture.Allocate_Storage (1, Samples, GL.Pixels.Depth32F_Stencil8, Width, Height, 1);

   FB_1.Attach (Color_Texture);
   FB_1.Attach (Depth_Texture);

   No_MS_Color_Texture.Allocate_Storage (1, 1, GL.Pixels.RGBA8, Width, Height, 1);

   FB_2.Attach (No_MS_Color_Texture);

   --  Projection matrix
   Uni_Proj.Set_Matrix (Current_Camera.Projection_Matrix);

   Uni_Effect.Set_Int (Integer_32 (Effect));

   Ada.Text_IO.Put_Line ("Usage: Right click and drag mouse to move camera around cube.");
   Ada.Text_IO.Put_Line ("Usage: Use scroll wheel to zoom in and out.");
   Ada.Text_IO.Put_Line ("Usage: Press space key to cycle between post-processing effects.");

   begin
      while not Window.Should_Close loop
         AWT.Process_Events (0.001);

         declare
            Pointer  : constant AWT.Inputs.Pointer_State  := Window.State;
            Keyboard : constant AWT.Inputs.Keyboard_State := Window.State;

            use all type AWT.Inputs.Button_State;
            use all type AWT.Inputs.Keyboard_Button;
            use all type AWT.Inputs.Pointer_Button;
            use all type AWT.Inputs.Pointer_Mode;
            use all type AWT.Inputs.Dimension;

            Rotate_Camera : constant Boolean :=
              Pointer.Focused and Pointer.Buttons (Right) = Pressed;
         begin
            if Keyboard.Pressed (Key_Escape) then
               Window.Close;
            end if;

            if Keyboard.Pressed (Key_Space) then
               Effect := Effect + 1;
               Uni_Effect.Set_Int (Integer_32 (Effect));
            end if;

            declare
               New_Mode : constant AWT.Inputs.Pointer_Mode :=
                 (if Rotate_Camera then Locked else Visible);
            begin
               if Pointer.Mode /= New_Mode then
                  Window.Set_Pointer_Mode (New_Mode);
               end if;
            end;

            Current_Camera.Change_Orientation
              (((if Rotate_Camera then Orka.Float_64 (Pointer.Relative (X)) else 0.0),
                (if Rotate_Camera then Orka.Float_64 (Pointer.Relative (Y)) else 0.0),
                Orka.Float_64 (Pointer.Scroll (Y)),
                0.0));
         end;

         Current_Camera.Update (0.01666);
         declare
            VP : constant Transforms.Vector4 :=
              Orka.Transforms.Doubles.Vector_Conversions.Convert (Current_Camera.View_Position);

            use Transforms.Vectors;
            use Transforms;

            TM : constant Matrix4 := T (Vector_Type (Transforms.Zero_Point - Point (VP)));
         begin
            Uni_View.Set_Matrix (Current_Camera.View_Matrix * TM);
         end;

         ---------------------------------------------------------------
         --                           Cube                            --
         ---------------------------------------------------------------

         FB_1.Use_Framebuffer;
         FB_1.Clear;

         --  Model matrix
         Uni_Model.Set_Matrix (World_TM);

         GL.Toggles.Enable (GL.Toggles.Depth_Test);

         Program_Scene.Use_Program;
         Orka.Rendering.Textures.Bind (Scene_Texture, Orka.Rendering.Textures.Texture, 0);
         Buffer_0.Bind (Shader_Storage, 0);

         Orka.Rendering.Drawing.Draw (Triangles, 0, 30);

         ---------------------------------------------------------------
         --                           Floor                           --
         ---------------------------------------------------------------

         GL.Toggles.Enable (GL.Toggles.Stencil_Test);

         --  Set any stencil to 1
         Set_Stencil_Function (GL.Rasterization.Front_And_Back, Always, 1, 16#FF#);
         Set_Stencil_Operation (GL.Rasterization.Front_And_Back, Keep, Keep, Replace);
         Set_Stencil_Mask (16#FF#);  --  Allow writing to stencil buffer

         --  Disable writing to the depth buffer in order to prevent the
         --  floor from hiding the reflection cube
         Set_Depth_Mask (False);
         FB_1.Clear ((Stencil => True, others => False));

         Orka.Rendering.Drawing.Draw (Triangles, 30, 6);

         ---------------------------------------------------------------
         --                      Reflection cube                      --
         ---------------------------------------------------------------

         --  Pass test if stencil value is 1
         Set_Stencil_Function (GL.Rasterization.Front_And_Back, Equal, 1, 16#FF#);
         Set_Stencil_Mask (16#00#);  --  Don't write anything to stencil buffer
         Set_Depth_Mask (True);

         --  Start drawing reflection cube
         declare
            use Transforms;

            Offset : constant Matrix4 :=
              (0.0, 0.0, -1.0, 0.0) + Transforms.S ((1.0, 1.0, -1.0, 1.0));
         begin
            Uni_Model.Set_Matrix (World_TM * Offset);
         end;

         Uni_Color.Set_Vector (Transforms.Vector4'(0.3, 0.3, 0.3, 0.0));

         --  Disable face culling because we scaled Z by -1
         GL.Toggles.Disable (GL.Toggles.Cull_Face);
         Orka.Rendering.Drawing.Draw (Triangles, 0, 30);
         GL.Toggles.Enable (GL.Toggles.Cull_Face);
         --  End drawing reflection cube

         Uni_Color.Set_Vector (Transforms.Vector4'(1.0, 1.0, 1.0, 0.0));

         GL.Toggles.Disable (GL.Toggles.Stencil_Test);

         ---------------------------------------------------------------
         --                     Backside of floor                     --
         ---------------------------------------------------------------

         Uni_Model.Set_Matrix (World_TM);

         GL.Rasterization.Set_Cull_Face (GL.Rasterization.Front);
         Orka.Rendering.Drawing.Draw (Triangles, 30, 6);
         GL.Rasterization.Set_Cull_Face (GL.Rasterization.Back);

         ---------------------------------------------------------------
         --                      Post-processing                      --
         ---------------------------------------------------------------

         FB_1.Resolve_To (FB_2);

         FB_D.Use_Framebuffer;
         FB_D.Clear;

         GL.Toggles.Disable (GL.Toggles.Depth_Test);

         Program_Screen.Use_Program;
         Orka.Rendering.Textures.Bind (No_MS_Color_Texture, Orka.Rendering.Textures.Texture, 0);

         Orka.Rendering.Drawing.Draw_Indexed (Triangles, Buffer_2, 0, Indices_Screen'Length);

         --  Swap front and back buffers and process events
         Window.Swap_Buffers;
      end loop;
   end;
end Orka_12_Stencil;
