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
with Ada.Exceptions;

with GL.Low_Level.Enums;
with GL.Objects.Textures;
with GL.Pixels;
with GL.Toggles;
with GL.Types;

with Orka.Behaviors;
with Orka.Contexts;
with Orka.Cameras;
with Orka.Culling;
with Orka.Debug;
with Orka.Futures;
with Orka.Jobs;
with Orka.Loops;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Programs.Uniforms;
with Orka.Rendering.Textures;
with Orka.Rendering.Vertex_Formats.Formats;
with Orka.Resources.Loaders;
with Orka.Resources.Locations.Directories;
with Orka.Resources.Managers;
with Orka.Resources.Models.glTF;
with Orka.Scenes.Singles.Trees;
with Orka.Types;
with Orka.Windows.GLFW;

with Orka_Package_glTF;

procedure Orka_GLTF is
   Width   : constant := 1280;
   Height  : constant := 720;
   Samples : constant := 2;

   use type GL.Types.Single;
   Light_Position : constant Orka.Types.Singles.Vector4 := (0.0, 0.0, 0.0, 1.0);

   ----------------------------------------------------------------------

   Initialized : constant Orka.Windows.GLFW.Active_GLFW'Class
     := Orka.Windows.GLFW.Initialize (Major => 4, Minor => 2, Debug => True);
   pragma Unreferenced (Initialized);

   W : aliased Orka.Windows.Window'Class := Orka.Windows.GLFW.Create_Window
     (Width, Height, Resizable => False);
   W_Ptr : constant Orka.Windows.Window_Ptr := Orka.Windows.Window_Ptr'(W'Unchecked_Access);

   Context : Orka.Contexts.Context;

   ----------------------------------------------------------------------

   use GL.Objects.Textures;

   procedure Load_Texture (Texture : in out GL.Objects.Textures.Texture) is
      Pixels : aliased constant GL.Types.Single_Array
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
      Texture.Allocate_Storage (1, 1, GL.Pixels.RGBA8, 4, 4, 7);
      Texture.Load_From_Data (0, 0, 0, 0, 4, 4, 7,
        GL.Pixels.RGB, GL.Pixels.Float, Pixels'Address);
   end Load_Texture;

   package Boss   renames Orka_Package_glTF.Boss;
   package Loader renames Orka_Package_glTF.Loader;

   use Ada.Exceptions;
begin
   if Ada.Command_Line.Argument_Count /= 2 then
      Ada.Text_IO.Put_Line ("Usage: <path to resources folder> <relative path to .gltf file");
      Boss.Shutdown;
      Loader.Shutdown;
      return;
   end if;

   declare
      Messages : constant Natural := Orka.Debug.Logged_Messages;
   begin
      Ada.Text_IO.Put_Line ("Flushing" & Messages'Image & " messages in the debug log:");
   end;
   Orka.Debug.Flush_Log;

   Orka.Debug.Enable_Print_Callback;
   Ada.Text_IO.Put_Line ("Set callback for debug messages");

   --  Enable some features
   Context.Enable (Orka.Contexts.Reversed_Z);
   if Samples > 0 then
      Context.Enable (Orka.Contexts.Multisample);
   end if;

   GL.Toggles.Enable (GL.Toggles.Cull_Face);
   GL.Toggles.Enable (GL.Toggles.Depth_Test);

   declare
      Location_Path : constant String := Ada.Command_Line.Argument (1);
      Model_Path    : constant String := Ada.Command_Line.Argument (2);

      use Orka.Resources;

      Location_Resources : constant Locations.Location_Ptr
        := Locations.Directories.Create_Location ("../resources");

      package Formats renames Orka.Rendering.Vertex_Formats;

      VF_1 : constant Formats.Vertex_Format_Ptr
        := new Formats.Vertex_Format'(Formats.Formats.Separate_Position_Normal_UV_Half_MDI);

      use Orka.Rendering.Programs;
      use Orka.Rendering.Framebuffers;

      P_1 : Program := Create_Program (Modules.Create_Module
        (Location_Resources,
         VS => "shaders/tools/gltf.vert",
         FS => "shaders/tools/gltf.frag"));

      Uni_View  : constant Uniforms.Uniform := P_1.Uniform ("view");
      Uni_Proj  : constant Uniforms.Uniform := P_1.Uniform ("proj");

      Uni_Light : constant Uniforms.Uniform := P_1.Uniform ("lightPosition");

      ----------------------------------------------------------------------

      Uni_Texture : constant Uniforms.Uniform_Sampler := P_1.Uniform_Sampler ("diffuseTexture");
      Uni_Dither  : constant Uniforms.Uniform_Sampler := P_1.Uniform_Sampler ("ditherTexture");

      Texture_1 : Texture (GL.Low_Level.Enums.Texture_2D_Array);
      Texture_2 : constant Texture := Orka.Rendering.Textures.Bayer_Dithering_Pattern;

      ----------------------------------------------------------------------

      FB_1 : constant Framebuffer_Ptr
        := new Framebuffer'(Create_Framebuffer (Width, Height, Samples, Context));
      FB_D : constant Framebuffer_Ptr
        := new Framebuffer'(Get_Default_Framebuffer (W));

      use Orka.Cameras;
      Lens : constant Lens_Ptr
        := new Camera_Lens'Class'(Create_Lens (Width, Height, 45.0, Context));
      Current_Camera : constant Camera_Ptr
        := new Camera'Class'(Create_Camera (Rotate_Around, W.Pointer_Input, Lens, FB_1));

      use Orka.Culling;
      Culler_1 : constant Culler_Ptr
        := new Culler'Class'(Culler'Class (Create_Culler (Location_Resources)));

      ----------------------------------------------------------------------

      task Resource_Test;

      Manager : constant Managers.Manager_Ptr := Managers.Create_Manager;

      Group : aliased Orka.Resources.Models.Group_Access := null;
      use type Orka.Resources.Models.Group_Access;

      task body Resource_Test is
         use Ada.Real_Time;

         Loader_glTF : constant Loaders.Loader_Ptr := Models.glTF.Create_Loader
           (VF_1, Manager);

         Location_Models : constant Locations.Location_Ptr
           := Locations.Directories.Create_Location (Location_Path);
      begin
         Loader.Add_Location (Location_Models, Loader_glTF);
         Ada.Text_IO.Put_Line ("Registered location for glTF models");

         declare
            T1 : constant Time := Clock;
            T2 : Time;

            Future_Ref : Orka.Futures.Pointers.Reference := Loader.Load (Model_Path);

            use type Orka.Futures.Status;
            Resource_Status : Orka.Futures.Status;

            Resource_Future : constant Orka.Futures.Future_Access := Future_Ref.Value;
         begin
            select
               Resource_Future.Wait_Until_Done (Resource_Status);
               T2 := Clock;

               pragma Assert (Resource_Status = Orka.Futures.Done);
               pragma Assert (Manager.Contains (Model_Path));

               declare
                  Loading_Time : constant Duration := 1e3 * To_Duration (T2 - T1);
               begin
                  Ada.Text_IO.Put_Line ("Loaded in " & Loading_Time'Image & " ms");
               end;

               declare
                  Model_1 : constant Models.Model_Ptr
                    := Models.Model_Ptr (Manager.Resource (Model_Path));

                  Handle : Orka.Futures.Pointers.Mutable_Pointer;

                  Create_Instance_Job : constant Orka.Jobs.Job_Ptr
                    := new Orka_Package_glTF.Create_Group_Job'
                        (Orka.Jobs.Abstract_Job with Model => Model_1, Culler => Culler_1,
                        Group => Group'Unchecked_Access);
               begin
                  Ada.Text_IO.Put_Line ("Adding resource to scene...");
                  Boss.Queue.Enqueue (Create_Instance_Job, Handle);
               end;
            or
               delay until T1 + Milliseconds (2000);
               Ada.Text_IO.Put_Line ("Not completed loading: " & Future_Ref.Current_Status'Image);
            end select;
         end;
      exception
         when Error : others =>
            Ada.Text_IO.Put_Line ("Error loading resource: " & Exception_Information (Error));
      end Resource_Test;
   begin
      --  Load checkerboard texture array
      Load_Texture (Texture_1);
      Uni_Texture.Set_Texture (Texture_1, 1);

      --  Set dithering texture
      Uni_Dither.Set_Texture (Texture_2, 2);

      Uni_Proj.Set_Matrix (Current_Camera.Projection_Matrix);

      Uni_Light.Set_Vector (Light_Position);

      declare
         Group_Added : Boolean := False;

         procedure Add_Behavior (Object : Orka.Behaviors.Behavior_Ptr);

         procedure Render
           (Scene  : not null Orka.Behaviors.Behavior_Array_Access;
            Camera : Orka.Cameras.Camera_Ptr) is
         begin
            Camera.FB.Use_Framebuffer;

            --  Clear color to black and depth to 0.0 (because of reversed Z)
            Camera.FB.GL_Framebuffer.Clear_Color_Buffer (0, (0.0, 0.0, 0.0, 0.0));
            Camera.FB.GL_Framebuffer.Clear_Depth_Buffer (0.0);

            Uni_View.Set_Matrix (Camera.View_Matrix);

            declare
               use Orka.Cameras.Transforms;
            begin
               --  TODO Don't re-compute projection matrix every frame
               Culler_1.Bind (Current_Camera.Projection_Matrix * Camera.View_Matrix);
            end;

            if Group /= null then
               if not Group_Added then
                  Group_Added := True;
                  declare
                     Instance : Orka.Resources.Models.Model_Instance_Ptr :=
                       new Orka_Package_glTF.No_Behavior'(Orka.Resources.Models.Model_Instance
                         with Position => (0.0, 0.0, 0.0, 1.0));
                  begin
                     Group.Add_Instance (Instance);
                     Add_Behavior (Orka.Behaviors.Behavior_Ptr (Instance));
                  end;
               end if;

               Group.Cull;
               P_1.Use_Program;

               --  Render objects in scene here
               for Behavior of Scene.all loop
                  Behavior.Render;
               end loop;
               Group.Render;

               --  Resolve the multiple samples in the FBO
               Camera.FB.Resolve_To (FB_D.all);

               Group.After_Render;
               for Behavior of Scene.all loop
                  Behavior.After_Render;
               end loop;
            end if;
         end Render;

         package Loops is new Orka.Loops
           (Time_Step   => Ada.Real_Time.Microseconds (2_083),
            Frame_Limit => Ada.Real_Time.Microseconds (8_334),
            Window      => W_Ptr,
            Camera      => Current_Camera,
            Render      => Render'Unrestricted_Access,
            Job_Manager => Boss);

         procedure Add_Behavior (Object : Orka.Behaviors.Behavior_Ptr) is
         begin
            Loops.Scene.Add (Object);
         end Add_Behavior;
      begin
         Loops.Scene.Add (Orka.Behaviors.Null_Behavior);
         Loops.Handler.Enable_Limit (False);
         Ada.Text_IO.Put_Line ("Running render loop...");
         Loops.Run_Loop;
      end;
   exception
      when Error : others =>
         Ada.Text_IO.Put_Line ("Error: " & Exception_Information (Error));
   end;
   Ada.Text_IO.Put_Line ("Shutting down...");
   Boss.Shutdown;
   Loader.Shutdown;
   Ada.Text_IO.Put_Line ("Shutdown job system and loader");
exception
   when Error : others =>
      Ada.Text_IO.Put_Line ("Error: " & Exception_Information (Error));
end Orka_GLTF;
