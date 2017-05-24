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

--  Needed for TBO
with GL.Low_Level.Enums;
with GL.Objects.Textures;
with GL.Pixels;
with Orka.Buffers;
with Orka.Types;

--  Needed for MSAA
with Orka.Framebuffers;

with Orka.Behaviors;
with Orka.Cameras;
with Orka.Loops;
with Orka.Programs.Modules;
with Orka.Programs.Uniforms;
with Orka.Resources.Models.glTF;
with Orka.Scenes.Singles.Trees;
with Orka.Transforms.Singles.Matrices;
with Orka.Transforms.Singles.Vectors;
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

   package Models is new Orka.Resources.Models (Orka.Scenes.Singles.Trees);
   package glTF is new Models.glTF;

   use GL.Objects.Textures;

   procedure Load_Texture (Texture : Texture_3D) is
      Pixels : constant GL.Types.Single_Array
            --  White
        := (0.0, 0.0, 0.0,   1.0, 1.0, 1.0,   0.0, 0.0, 0.0,   1.0, 1.0, 1.0,
            1.0, 1.0, 1.0,   0.0, 0.0, 0.0,   1.0, 1.0, 1.0,   0.0, 0.0, 0.0,
            0.0, 0.0, 0.0,   1.0, 1.0, 1.0,   0.0, 0.0, 0.0,   1.0, 1.0, 1.0,
            1.0, 1.0, 1.0,   0.0, 0.0, 0.0,   1.0, 1.0, 1.0,   0.0, 0.0, 0.0,

            --  Red
            0.0, 0.0, 0.0,   1.0, 0.0, 0.0,   0.0, 0.0, 0.0,   1.0, 0.0, 0.0,
            1.0, 0.0, 0.0,   0.0, 0.0, 0.0,   1.0, 0.0, 0.0,   0.0, 0.0, 0.0,
            0.0, 0.0, 0.0,   1.0, 0.0, 0.0,   0.0, 0.0, 0.0,   1.0, 0.0, 0.0,
            1.0, 0.0, 0.0,   0.0, 0.0, 0.0,   1.0, 0.0, 0.0,   0.0, 0.0, 0.0,

            --  Green
            0.0, 0.0, 0.0,   0.0, 1.0, 0.0,   0.0, 0.0, 0.0,   0.0, 1.0, 0.0,
            0.0, 1.0, 0.0,   0.0, 0.0, 0.0,   0.0, 1.0, 0.0,   0.0, 0.0, 0.0,
            0.0, 0.0, 0.0,   0.0, 1.0, 0.0,   0.0, 0.0, 0.0,   0.0, 1.0, 0.0,
            0.0, 1.0, 0.0,   0.0, 0.0, 0.0,   0.0, 1.0, 0.0,   0.0, 0.0, 0.0,

            --  Blue
            0.0, 0.0, 0.0,   0.0, 0.0, 1.0,   0.0, 0.0, 0.0,   0.0, 0.0, 1.0,
            0.0, 0.0, 1.0,   0.0, 0.0, 0.0,   0.0, 0.0, 1.0,   0.0, 0.0, 0.0,
            0.0, 0.0, 0.0,   0.0, 0.0, 1.0,   0.0, 0.0, 0.0,   0.0, 0.0, 1.0,
            0.0, 0.0, 1.0,   0.0, 0.0, 0.0,   0.0, 0.0, 1.0,   0.0, 0.0, 0.0,

            --  Cyan
            0.0, 0.0, 0.0,   0.0, 1.0, 1.0,   0.0, 0.0, 0.0,   0.0, 1.0, 1.0,
            0.0, 1.0, 1.0,   0.0, 0.0, 0.0,   0.0, 1.0, 1.0,   0.0, 0.0, 0.0,
            0.0, 0.0, 0.0,   0.0, 1.0, 1.0,   0.0, 0.0, 0.0,   0.0, 1.0, 1.0,
            0.0, 1.0, 1.0,   0.0, 0.0, 0.0,   0.0, 1.0, 1.0,   0.0, 0.0, 0.0,

            --  Yellow
            0.0, 0.0, 0.0,   1.0, 1.0, 0.0,   0.0, 0.0, 0.0,   1.0, 1.0, 0.0,
            1.0, 1.0, 0.0,   0.0, 0.0, 0.0,   1.0, 1.0, 0.0,   0.0, 0.0, 0.0,
            0.0, 0.0, 0.0,   1.0, 1.0, 0.0,   0.0, 0.0, 0.0,   1.0, 1.0, 0.0,
            1.0, 1.0, 0.0,   0.0, 0.0, 0.0,   1.0, 1.0, 0.0,   0.0, 0.0, 0.0,

            --  Magenta
            0.0, 0.0, 0.0,   1.0, 0.0, 1.0,   0.0, 0.0, 0.0,   1.0, 0.0, 1.0,
            1.0, 0.0, 1.0,   0.0, 0.0, 0.0,   1.0, 0.0, 1.0,   0.0, 0.0, 0.0,
            0.0, 0.0, 0.0,   1.0, 0.0, 1.0,   0.0, 0.0, 0.0,   1.0, 0.0, 1.0,
            1.0, 0.0, 1.0,   0.0, 0.0, 0.0,   1.0, 0.0, 1.0,   0.0, 0.0, 0.0);
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
      use type Ada.Real_Time.Time;

      A : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
      M : Models.Model := glTF.Load_Model (Ada.Command_Line.Argument (1));
      B : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

      use Orka.Programs;
      use Orka.Framebuffers;

      Program_1 : Program := Create_Program (Modules.Create_Module
        (VS => "../examples/orka/shaders/test-6-module-1.vert",
         FS => "../examples/orka/shaders/test-6-module-1.frag"));

      Uni_View  : constant Uniforms.Uniform := Program_1.Uniform ("view");
      Uni_Proj  : constant Uniforms.Uniform := Program_1.Uniform ("proj");

      Uni_WT      : constant Uniforms.Uniform_Sampler := Program_1.Uniform_Sampler ("matrixBuffer");
      Uni_Texture : constant Uniforms.Uniform_Sampler := Program_1.Uniform_Sampler ("diffuseTexture");

      FB_1 : constant Framebuffer_Ptr := new Framebuffer'(Create_Framebuffer (Width, Height, Samples));
      FB_D : constant Framebuffer_Ptr := new Framebuffer'(Create_Default_Framebuffer (Width, Height));

      use Orka.Transforms.Singles.Matrices;
      use type GL.Types.Single;

      Texture_1 : Texture_3D (GL.Low_Level.Enums.Texture_2D_Array);

      use Orka.Cameras;
      Lens : constant Lens_Ptr := new Camera_Lens'Class'(Create_Lens (Width, Height, 45.0));
      Current_Camera : constant Camera_Ptr := new Camera'Class'(Create_Camera (Rotate_Around, W.Pointer_Input, Lens, FB_1));

      use Orka.Buffers;

      World_Transforms : Orka.Types.Singles.Matrix4_Array (1 .. GL.Types.Int (M.Shapes.Length))
        := (others => Orka.Types.Singles.Identity4);

      --  Set-up TBO for world transform matrices
      Buffer_1 : constant Buffer := Orka.Buffers.Create_Buffer ((Dynamic_Storage => True, others => False), Orka.Types.Single_Matrix_Type, World_Transforms'Length);
      TBO_1 : Buffer_Texture (GL.Low_Level.Enums.Texture_Buffer);
   begin
      Ada.Text_IO.Put_Line ("Duration glTF: " & Duration'Image (1e3 * Ada.Real_Time.To_Duration (B - A)));
      Ada.Text_IO.Put_Line ("Shapes: " & Integer'Image (Integer (M.Shapes.Length)));

      --  Compute the world transforms by multiplying the local transform
      --  of each node with the world transform of its parent.
      M.Update_World_Transforms;
      for I in 1 .. M.Shapes.Length loop
         declare
            C : constant Orka.Scenes.Singles.Trees.Cursor := M.Scene_Tree.To_Cursor (M.Shapes.Element (Positive (I)));
         begin
            World_Transforms (GL.Types.Int (I)) := M.Scene_Tree.World_Transform (C);
         end;
      end loop;

      --  In a loop we should use a persistent mapped buffer instead of Set_Data
      Buffer_1.Set_Data (World_Transforms);
      TBO_1.Attach_Buffer (GL.Pixels.RGBA32F, Buffer_1.GL_Buffer);
      Uni_WT.Set_Texture (TBO_1, 0);

      --  Load checkerboard texture
      Load_Texture (Texture_1);
      Uni_Texture.Set_Texture (Texture_1, 1);

      Uni_Proj.Set_Matrix (Current_Camera.Projection_Matrix);

      GL.Toggles.Enable (GL.Toggles.Cull_Face);
      GL.Toggles.Enable (GL.Toggles.Depth_Test);

      declare
         procedure Render (Scene : not null Orka.Behaviors.Behavior_Array_Access; Camera : Orka.Cameras.Camera_Ptr) is
         begin
            GL.Buffers.Clear (GL.Buffers.Buffer_Bits'(Color => True, Depth => True, others => False));

            Uni_View.Set_Matrix (Camera.View_Matrix);
            --  TODO Update projection matrix if lens changed

            --  TODO Render objects in scene here
            Camera.FB.Draw_Indirect (Program_1, M.Mesh, M.Command_Buffer);

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
         Loops.Run_Loop;
      end;
   end;
end Orka_Test.Test_6_GLTF;
