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

with GL.Buffers;
with GL.Low_Level.Enums;
with GL.Objects.Samplers;
with GL.Objects.Textures;
with GL.Pixels;
with GL.Types;

with Orka.Cameras.Rotate_Around_Cameras;
with Orka.Contexts.AWT;
with Orka.Frame_Graphs;
with Orka.OS;
with Orka.Rendering.Buffers;
with Orka.Rendering.Drawing;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Programs.Uniforms;
with Orka.Rendering.States;
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
     (Version => (4, 2), Flags => (Debug => True, others => False));

   Window : constant Orka.Windows.Window'Class
     := Orka.Contexts.AWT.Create_Window (Context, Width, Height, Resizable => False);

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

   Scene_Texture : Textures.Texture (Textures.LE.Texture_2D);

   Sampler_1 : GL.Objects.Samplers.Sampler;

   use Orka.Resources;

   Location_Shaders : constant Locations.Location_Ptr
     := Locations.Directories.Create_Location ("data/shaders");

   Program_Scene : constant Program := Create_Program (Modules.Create_Module
     (Location_Shaders, VS => "buffers_scene.vert", FS => "buffers_scene.frag"));

   Program_Screen : constant Program := Create_Program (Modules.Create_Module
     (Location_Shaders, VS => "buffers_screen.vert", FS => "buffers_screen.frag"));

   Uni_Model  : constant Uniforms.Uniform := Program_Scene.Uniform ("model");
   Uni_View   : constant Uniforms.Uniform := Program_Scene.Uniform ("view");
   Uni_Proj   : constant Uniforms.Uniform := Program_Scene.Uniform ("proj");
   Uni_Color  : constant Uniforms.Uniform := Program_Scene.Uniform ("overrideColor");

   Uni_Effect : constant Uniforms.Uniform := Program_Screen.Uniform ("effect");

   ----------------------------------------------------------------------------

   use Orka.Frame_Graphs;
   use all type Orka.Rendering.States.Cull_Face_Selector;
   use all type Orka.Rendering.States.Face;
   use all type GL.Buffers.Stencil_Action;

   Graph_Builder : Orka.Frame_Graphs.Frame_Graph
     (Maximum_Passes    => 10,
      Maximum_Handles   => 30,
      Maximum_Resources => 30);

   Default_State : constant Orka.Rendering.States.State := (others => <>);

   Floor_State : constant Orka.Rendering.States.State :=
      --  Set any stencil to 1
     (Stenciling => (Front | Back =>
                       (Reference  => 1,
                        Test_Func  => GL.Types.Always,
                        Test_Mask  => 16#FF#,
                        Write_Mask => 16#FF#,  --  Allow writing to stencil buffer
                        Operations => (Keep, Keep, Replace))),
      others => <>);

   Reflection_State : constant Orka.Rendering.States.State :=
      --  Pass test if stencil value is 1
     (Stenciling => (Front | Back =>
                       (Reference  => 1,
                        Test_Func  => GL.Types.Equal,
                        Test_Mask  => 16#FF#,
                        Write_Mask => 16#00#,  --  Don't write anything to stencil buffer
                        Operations => (Keep, Keep, Replace))),
      Cull_Face => None,  --  Disable face culling because we scaled Z by -1
      others => <>);

   Backside_Floor_State : constant Orka.Rendering.States.State :=
     (Cull_Face => Front, others => <>);

   Fullscreen_State : constant Orka.Rendering.States.State :=
     (Depth_Func => GL.Types.Always, others => <>);

   package Transforms renames Orka.Transforms.Singles.Matrices;

   World_TM : constant Transforms.Matrix4 := Transforms.Rx (-0.5 * Ada.Numerics.Pi);

   procedure Run_P1 (P : in out Program) is
   begin
      Uni_Model.Set_Matrix (World_TM);

      Orka.Rendering.Textures.Bind (Scene_Texture, Orka.Rendering.Textures.Texture, 0);
      Buffer_0.Bind (Shader_Storage, 0);

--            GL.Toggles.Disable (GL.Toggles.Stencil_Test);
      Orka.Rendering.Drawing.Draw (Triangles, 0, 30);
--            GL.Toggles.Enable (GL.Toggles.Stencil_Test);
   end Run_P1;

   procedure Run_P2 (P : in out Program) is
   begin
      Orka.Rendering.Drawing.Draw (Triangles, 30, 6);
   end Run_P2;

   procedure Run_P3 (P : in out Program) is
      use Transforms;

      Offset : constant Matrix4 :=
        (0.0, 0.0, -1.0, 0.0) + Transforms.S ((1.0, 1.0, -1.0, 1.0));
   begin
      --  Start drawing reflection cube
      Uni_Model.Set_Matrix (World_TM * Offset);
      Uni_Color.Set_Vector (Transforms.Vector4'(0.3, 0.3, 0.3, 0.0));

      Orka.Rendering.Drawing.Draw (Triangles, 0, 30);
   end Run_P3;

   procedure Run_P4 (P : in out Program) is
   begin
      Uni_Model.Set_Matrix (World_TM);
      Uni_Color.Set_Vector (Transforms.Vector4'(1.0, 1.0, 1.0, 0.0));

      Orka.Rendering.Drawing.Draw (Triangles, 30, 6);
   end Run_P4;

   procedure Run_P5 (P : in out Program) is
   begin
      Orka.Rendering.Drawing.Draw_Indexed (Triangles, Buffer_2, 0, Indices_Screen'Length);
   end Run_P5;

   Pass_1 : Render_Pass'Class :=
     Graph_Builder.Add_Pass ("P1_Cube", Default_State, Program_Scene, Run_P1'Unrestricted_Access);
   Pass_2 : Render_Pass'Class :=
     Graph_Builder.Add_Pass ("P2_Floor", Floor_State, Program_Scene, Run_P2'Unrestricted_Access);
   Pass_3 : Render_Pass'Class :=
     Graph_Builder.Add_Pass ("P3_Reflection", Reflection_State, Program_Scene,
       Run_P3'Unrestricted_Access);
   Pass_4 : Render_Pass'Class :=
     Graph_Builder.Add_Pass ("P4_Floor", Backside_Floor_State, Program_Scene,
       Run_P4'Unrestricted_Access);
   Pass_5 : Render_Pass'Class :=
     Graph_Builder.Add_Pass ("P5_Post", Fullscreen_State, Program_Screen,
       Run_P5'Unrestricted_Access);

   FB_D : constant Framebuffer := Create_Default_Framebuffer (Width, Height);

   package LE renames GL.Low_Level.Enums;

   Resource_1 : constant Orka.Frame_Graphs.Resource :=
     (Name    => +"R1",
      Kind    => LE.Texture_2D_Multisample,
      Format  => GL.Pixels.RGBA8,
      Size    => (FB_D.Width, FB_D.Height, 1),
      Samples => Samples,
      others  => <>);
   Resource_1B : Orka.Frame_Graphs.Resource;
   Resource_1C : Orka.Frame_Graphs.Resource;
   Resource_1D : Orka.Frame_Graphs.Resource;

   Resource_2 : constant Orka.Frame_Graphs.Resource :=
     (Name    => +"R2",
      Kind    => LE.Texture_2D_Multisample,
--      Format  => GL.Pixels.Depth_Component32F,
      Format  => GL.Pixels.Depth32F_Stencil8,
      Size    => (FB_D.Width, FB_D.Height, 1),
      Samples => Samples,
      others  => <>);
   Resource_2A : Orka.Frame_Graphs.Resource;  --  TODO Temp
   Resource_2B : Orka.Frame_Graphs.Resource;

   Resource_3 : constant Orka.Frame_Graphs.Resource :=
     (Name    => +"R3",
      Kind    => LE.Texture_2D_Multisample,
      Format  => GL.Pixels.Stencil_Index8,
      Size    => (FB_D.Width, FB_D.Height, 1),
      Samples => Samples,
      others  => <>);

   Resource_9 : constant Orka.Frame_Graphs.Resource :=
     (Name    => +"R9",
      Kind    => LE.Texture_2D,
      Format  => GL.Pixels.RGBA8,
      Size    => (FB_D.Width, FB_D.Height, 1),
      Samples => 0,
      others  => <>);

   ----------------------------------------------------------------------------

   use Orka.Cameras;
   Lens : constant Camera_Lens := Create_Lens (Width, Height, 45.0);
   Current_Camera : Rotate_Around_Cameras.Rotate_Around_Camera :=
     Rotate_Around_Cameras.Create_Camera (Lens);

   use all type GL.Objects.Samplers.Minifying_Function;
   use all type GL.Objects.Samplers.Magnifying_Function;
   use all type GL.Objects.Samplers.Wrapping_Mode;

   Default_Distance : constant := -3.0;
   Deg_45           : constant := 0.25 * Ada.Numerics.Pi;

   type Effect_Type is mod 4;

   Effect : Effect_Type := Effect_Type'First;
begin
   Current_Camera.Set_Orientation ((-Deg_45, 0.5 * Deg_45, Default_Distance, 0.0));
   Current_Camera.Update (0.0);

   Pass_1.Add_Output (Resource_1, Framebuffer_Attachment, 0);
   Pass_1.Add_Output (Resource_2, Framebuffer_Attachment, 0);

   Resource_1B :=
     Pass_2.Add_Input_Output (Resource_1, Framebuffer_Attachment, 0);

   --  Disable writing to the depth buffer in order to prevent the
   --  floor from hiding the reflection cube
   Pass_2.Add_Input (Resource_2, Framebuffer_Attachment, 0);
--   Pass_2.Add_Output (Resource_3, Framebuffer_Attachment, 0);

   --  TODO Resource_2A is temporary because Gallium doesn't support separate depth+stencil
   Resource_2A :=
     Pass_2.Add_Input_Output (Resource_2, Framebuffer_Attachment, 0);

   Resource_1C :=
     Pass_3.Add_Input_Output (Resource_1B, Framebuffer_Attachment, 0);
   Resource_2B :=
     Pass_3.Add_Input_Output (Resource_2A, Framebuffer_Attachment, 0);
   --  TODO Normally Resource_2 instead of Resource_2A

--   Pass_3.Add_Input (Resource_3, Framebuffer_Attachment, 0);

   Resource_1D :=
     Pass_4.Add_Input_Output (Resource_1C, Framebuffer_Attachment, 0);

   Pass_4.Add_Input (Resource_2B, Framebuffer_Attachment, 0);

   --  TODO Need to resolve Resource_1D (2D MS) to Resource_1E (2D)
   Pass_5.Add_Input (Resource_1D, Texture_Read, 0);
   Pass_5.Add_Output (Resource_9, Framebuffer_Attachment, 0);

   ----------------------------------------------------------------------------

   Sampler_1.Set_X_Wrapping (Clamp_To_Edge);
   Sampler_1.Set_Y_Wrapping (Clamp_To_Edge);

   Sampler_1.Set_Minifying_Filter (Nearest);
   Sampler_1.Set_Magnifying_Filter (Nearest);

   Sampler_1.Bind (0);

   --  Load checkerboard texture
   Load_Texture (Scene_Texture);

   --  Projection matrix
   Uni_Proj.Set_Matrix (Current_Camera.Projection_Matrix);

   Uni_Effect.Set_Int (Integer_32 (Effect));

   Orka.OS.Put_Line ("Usage: Right click and drag mouse to move camera around cube.");
   Orka.OS.Put_Line ("Usage: Use scroll wheel to zoom in and out.");
   Orka.OS.Put_Line ("Usage: Press space key to cycle between post-processing effects.");

   declare
      Graph : Orka.Frame_Graphs.Renderable_Graph'Class :=
        Graph_Builder.Cull (Present => Resource_1D);
   begin
      Graph.Initialize (Location_Shaders, FB_D);

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

         Graph.Render (Context);
         Window.Swap_Buffers;
      end loop;
   end;
end Orka_12_Stencil;
