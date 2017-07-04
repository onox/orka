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

with Ada.Command_Line;
with Ada.Real_Time;
with Ada.Text_IO;

with GL.Buffers;
with GL.Toggles;
with GL.Types;

with GL.Debug.Logs;
with Orka.Debug;

with GL.Low_Level.Enums;
with GL.Objects.Textures;
with GL.Pixels;

--  Needed for MSAA
with Orka.Framebuffers;

with Orka.Behaviors;
with Orka.Cameras;
with Orka.Loops;
with Orka.Programs.Modules;
with Orka.Programs.Uniforms;
with Orka.Resources.Models.glTF;
with Orka.Scenes.Singles.Trees;
with Orka.Textures;
with Orka.Transforms.Singles.Vectors;
with Orka.Vertex_Formats.Formats;
with Orka.Windows.GLFW;

procedure Orka_Test.Test_6_GLTF is
   Width   : constant := 1280;
   Height  : constant := 720;
   Samples : constant := 1;

   Initialized : constant Orka.Windows.GLFW.Active_GLFW'Class
     := Orka.Windows.GLFW.Initialize (Major => 3, Minor => 2, Debug => True);
   pragma Unreferenced (Initialized);

   W : Orka.Windows.Window'Class := Orka.Windows.GLFW.Create_Window
     (Width, Height, Resizable => False);

   use GL.Objects.Textures;

   procedure Load_Texture (Texture : Texture_3D) is
      Pixels : constant GL.Types.Single_Array
            --  White
        := (0.5, 0.5, 0.5,   1.0, 1.0, 1.0,   0.5, 0.5, 0.5,   1.0, 1.0, 1.0,
            1.0, 1.0, 1.0,   0.5, 0.5, 0.5,   1.0, 1.0, 1.0,   0.5, 0.5, 0.5,
            0.5, 0.5, 0.5,   1.0, 1.0, 1.0,   0.5, 0.5, 0.5,   1.0, 1.0, 1.0,
            1.0, 1.0, 1.0,   0.5, 0.5, 0.5,   1.0, 1.0, 1.0,   0.5, 0.5, 0.5,

            --  Red
            0.5, 0.5, 0.5,   1.0, 0.0, 0.0,   0.5, 0.5, 0.5,   1.0, 0.0, 0.0,
            1.0, 0.0, 0.0,   0.5, 0.5, 0.5,   1.0, 0.0, 0.0,   0.5, 0.5, 0.5,
            0.5, 0.5, 0.5,   1.0, 0.0, 0.0,   0.5, 0.5, 0.5,   1.0, 0.0, 0.0,
            1.0, 0.0, 0.0,   0.5, 0.5, 0.5,   1.0, 0.0, 0.0,   0.5, 0.5, 0.5,

            --  Green
            0.5, 0.5, 0.5,   0.0, 1.0, 0.0,   0.5, 0.5, 0.5,   0.0, 1.0, 0.0,
            0.0, 1.0, 0.0,   0.5, 0.5, 0.5,   0.0, 1.0, 0.0,   0.5, 0.5, 0.5,
            0.5, 0.5, 0.5,   0.0, 1.0, 0.0,   0.5, 0.5, 0.5,   0.0, 1.0, 0.0,
            0.0, 1.0, 0.0,   0.5, 0.5, 0.5,   0.0, 1.0, 0.0,   0.5, 0.5, 0.5,

            --  Blue
            0.5, 0.5, 0.5,   0.0, 0.0, 1.0,   0.5, 0.5, 0.5,   0.0, 0.0, 1.0,
            0.0, 0.0, 1.0,   0.5, 0.5, 0.5,   0.0, 0.0, 1.0,   0.5, 0.5, 0.5,
            0.5, 0.5, 0.5,   0.0, 0.0, 1.0,   0.5, 0.5, 0.5,   0.0, 0.0, 1.0,
            0.0, 0.0, 1.0,   0.5, 0.5, 0.5,   0.0, 0.0, 1.0,   0.5, 0.5, 0.5,

            --  Cyan
            0.5, 0.5, 0.5,   0.0, 1.0, 1.0,   0.5, 0.5, 0.5,   0.0, 1.0, 1.0,
            0.0, 1.0, 1.0,   0.5, 0.5, 0.5,   0.0, 1.0, 1.0,   0.5, 0.5, 0.5,
            0.5, 0.5, 0.5,   0.0, 1.0, 1.0,   0.5, 0.5, 0.5,   0.0, 1.0, 1.0,
            0.0, 1.0, 1.0,   0.5, 0.5, 0.5,   0.0, 1.0, 1.0,   0.5, 0.5, 0.5,

            --  Yellow
            0.5, 0.5, 0.5,   1.0, 1.0, 0.0,   0.5, 0.5, 0.5,   1.0, 1.0, 0.0,
            1.0, 1.0, 0.0,   0.5, 0.5, 0.5,   1.0, 1.0, 0.0,   0.5, 0.5, 0.5,
            0.5, 0.5, 0.5,   1.0, 1.0, 0.0,   0.5, 0.5, 0.5,   1.0, 1.0, 0.0,
            1.0, 1.0, 0.0,   0.5, 0.5, 0.5,   1.0, 1.0, 0.0,   0.5, 0.5, 0.5,

            --  Magenta
            0.5, 0.5, 0.5,   1.0, 0.0, 1.0,   0.5, 0.5, 0.5,   1.0, 0.0, 1.0,
            1.0, 0.0, 1.0,   0.5, 0.5, 0.5,   1.0, 0.0, 1.0,   0.5, 0.5, 0.5,
            0.5, 0.5, 0.5,   1.0, 0.0, 1.0,   0.5, 0.5, 0.5,   1.0, 0.0, 1.0,
            1.0, 0.0, 1.0,   0.5, 0.5, 0.5,   1.0, 0.0, 1.0,   0.5, 0.5, 0.5);
   begin
      Texture.Set_X_Wrapping (Clamp_To_Edge);
      Texture.Set_Y_Wrapping (Clamp_To_Edge);

      Texture.Set_Minifying_Filter (Nearest);
      Texture.Set_Magnifying_Filter (Nearest);

      --  Load texture data
      Texture.Allocate_Storage (1, GL.Pixels.RGBA8, 4, 4, 7);
      Texture.Load_From_Data (0, 0, 0, 0, 4, 4, 7, GL.Pixels.RGB, GL.Pixels.Float, Pixels'Address);
   end Load_Texture;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("Usage: <path to .gltf file>");
      return;
   end if;

   Ada.Text_IO.Put_Line ("Flushing" & GL.Types.Size'Image (GL.Debug.Logs.Logged_Messages) & " messages in the debug log:");
   Orka.Debug.Flush_Log;

   Orka.Debug.Enable_Print_Callback;
   Ada.Text_IO.Put_Line ("Set callback for debug messages");

   declare
      package Models renames Orka.Resources.Models;

      VF : aliased Orka.Vertex_Formats.Vertex_Format
        := Orka.Vertex_Formats.Formats.Separate_Position_Normal_UV_Half_MDI;

      use Orka.Programs;
      use Orka.Framebuffers;

      Program_1 : Program := Create_Program (Modules.Create_Module
        (VS => "../examples/orka/shaders/test-6-module-1.vert",
         FS => "../examples/orka/shaders/test-6-module-1.frag"));

      Uni_View  : constant Uniforms.Uniform := Program_1.Uniform ("view");
      Uni_Proj  : constant Uniforms.Uniform := Program_1.Uniform ("proj");

      Uniform_WT  : aliased  Uniforms.Uniform_Sampler := Program_1.Uniform_Sampler ("matrixBuffer");
      Uni_Texture : constant Uniforms.Uniform_Sampler := Program_1.Uniform_Sampler ("diffuseTexture");
      Uni_Dither  : constant Uniforms.Uniform_Sampler := Program_1.Uniform_Sampler ("ditherTexture");

      FB_1 : constant Framebuffer_Ptr := new Framebuffer'(Create_Framebuffer (Width, Height, Samples));
      FB_D : constant Framebuffer_Ptr := new Framebuffer'(Create_Default_Framebuffer (Width, Height));

      Texture_1 : Texture_3D (GL.Low_Level.Enums.Texture_2D_Array);
      Texture_2 : constant Texture_2D := Orka.Textures.Bayer_Dithering_Pattern;

      use Orka.Cameras;
      Lens : constant Lens_Ptr := new Camera_Lens'Class'(Create_Lens (Width, Height, 45.0));
      Current_Camera : constant Camera_Ptr := new Camera'Class'(Create_Camera (Rotate_Around, W.Pointer_Input, Lens, FB_1));

      M : constant Models.Model := Models.glTF.Load_Model
        (VF'Access, Uniform_WT'Access, Ada.Command_Line.Argument (1));
   begin
      --  Load checkerboard texture
      Load_Texture (Texture_1);
      Uni_Texture.Set_Texture (Texture_1, 1);

      --  Set dithering texture
      Uni_Dither.Set_Texture (Texture_2, 2);

      Uni_Proj.Set_Matrix (Current_Camera.Projection_Matrix);

      GL.Toggles.Enable (GL.Toggles.Cull_Face);
      GL.Toggles.Enable (GL.Toggles.Depth_Test);

      declare
         procedure Render (Scene : not null Orka.Behaviors.Behavior_Array_Access; Camera : Orka.Cameras.Camera_Ptr) is
         begin
            GL.Buffers.Clear (GL.Buffers.Buffer_Bits'(Color => True, Depth => True, others => False));

            Camera.FB.Use_Framebuffer;
            Program_1.Use_Program;

            Uni_View.Set_Matrix (Camera.View_Matrix);
            --  TODO Update projection matrix if lens changed

            --  Render objects in scene here
            for Behavior of Scene.all loop
               Behavior.Render;
            end loop;

            --  Resolve the multiple samples in the FBO
            Camera.FB.Resolve_To (FB_D.all);
         end Render;

         package Loops is new Orka.Loops
           (Transforms  => Orka.Transforms.Singles.Vectors,
            Time_Step   => Ada.Real_Time.Microseconds (2083),
            Frame_Limit => Ada.Real_Time.Microseconds (16666),
            Window      => W'Access,
            Camera      => Current_Camera,
            Render      => Render'Access);
      begin
         Loops.Scene.Add (M.Create_Instance);
         Loops.Run_Loop;
      end;
   end;
end Orka_Test.Test_6_GLTF;
