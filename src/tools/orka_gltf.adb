--  SPDX-License-Identifier: Apache-2.0
--
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
with Ada.Directories;
with Ada.Real_Time;
with Ada.Text_IO;
with Ada.Exceptions;

with GL.Low_Level.Enums;
with GL.Objects.Samplers;
with GL.Objects.Textures;
with GL.Pixels;
with GL.Toggles;
with GL.Types;

with Orka.Behaviors;
with Orka.Contexts;
with Orka.Cameras.Rotate_Around_Cameras;
with Orka.Culling;
with Orka.Debug;
with Orka.Futures;
with Orka.Jobs;
with Orka.Loggers.Terminal;
with Orka.Logging;
with Orka.Loops;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Programs.Uniforms;
with Orka.Rendering.Textures;
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
      Texture.Allocate_Storage (1, 1, GL.Pixels.RGBA8, 4, 4, 7);
      Texture.Load_From_Data (0, 0, 0, 0, 4, 4, 7,
        GL.Pixels.RGB, GL.Pixels.Float, Pixels'Address);
   end Load_Texture;

   package Job_System renames Orka_Package_glTF.Job_System;
   package Loader     renames Orka_Package_glTF.Loader;

   use Ada.Exceptions;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("Usage: <path to .gltf file");
      Job_System.Shutdown;
      Loader.Shutdown;
      return;
   end if;

   Orka.Logging.Set_Logger (Orka.Loggers.Terminal.Create_Logger (Level => Orka.Loggers.Debug));

   declare
      Library : constant Orka.Contexts.Library'Class
        := Orka.Windows.GLFW.Initialize (Major => 4, Minor => 2, Debug => True);

      Window : aliased Orka.Windows.Window'Class := Library.Create_Window
        (Width, Height, Resizable => True);
      W_Ptr : constant Orka.Windows.Window_Ptr := Orka.Windows.Window_Ptr'(Window'Unchecked_Access);

      Context : Orka.Contexts.Context'Class := Window.Context;
   begin
      Orka.Debug.Set_Log_Messages (Enable => True, Raise_API_Error => True);

      --  Enable some features
      Context.Enable (Orka.Contexts.Reversed_Z);
      if Samples > 0 then
         Context.Enable (Orka.Contexts.Multisample);
      end if;

      GL.Toggles.Enable (GL.Toggles.Cull_Face);
      GL.Toggles.Enable (GL.Toggles.Depth_Test);

      declare
         Full_Path     : constant String := Ada.Command_Line.Argument (1);

         Location_Path : constant String := Ada.Directories.Containing_Directory (Full_Path);
         Model_Path    : constant String := Ada.Directories.Simple_Name (Full_Path);

         use Orka.Resources;

         Location_Shaders : constant Locations.Location_Ptr
           := Locations.Directories.Create_Location ("../data/shaders");

         package LE renames GL.Low_Level.Enums;

         use Orka.Rendering.Programs;
         use Orka.Rendering.Framebuffers;

         P_1 : Program := Create_Program (Modules.Create_Module
           (Location_Shaders,
            VS => "tools/gltf.vert",
            FS => "tools/gltf.frag"));

         Uni_View  : constant Uniforms.Uniform := P_1.Uniform ("view");
         Uni_Proj  : constant Uniforms.Uniform := P_1.Uniform ("proj");

         Uni_Light : constant Uniforms.Uniform := P_1.Uniform ("lightPosition");

         ----------------------------------------------------------------------

         Uni_Texture : constant Uniforms.Uniform_Sampler := P_1.Uniform_Sampler ("diffuseTexture");
         Uni_Dither  : constant Uniforms.Uniform_Sampler := P_1.Uniform_Sampler ("ditherTexture");

         use GL.Objects.Textures;
         use GL.Objects.Samplers;

         Texture_1 : Texture (LE.Texture_2D_Array);
         Texture_2 : constant Texture := Orka.Rendering.Textures.Bayer_Dithering_Pattern;

         Texture_3 : Texture (LE.Texture_2D_Multisample);
         Texture_4 : Texture (LE.Texture_2D_Multisample);

         Sampler_1 : Sampler;
         Sampler_2 : constant Sampler := Orka.Rendering.Textures.Bayer_Dithering_Pattern;

         ----------------------------------------------------------------------

         FB_1 : Framebuffer := Create_Framebuffer (Width, Height, Samples, Context);
         FB_D : constant Framebuffer := Get_Default_Framebuffer (Window);

         use Orka.Cameras;
         Lens : constant Lens_Ptr
           := new Camera_Lens'Class'(Create_Lens (Width, Height, 45.0, Context));
         Current_Camera : constant Camera_Ptr
           := new Camera'Class'(Camera'Class
                (Rotate_Around_Cameras.Create_Camera (Window.Pointer_Input, Lens, FB_1)));

         use Orka.Culling;
         Culler_1 : constant Culler_Ptr
           := new Culler'Class'(Culler'Class (Create_Culler (Location_Shaders)));

         ----------------------------------------------------------------------

         task Resource_Test;

         Manager : constant Managers.Manager_Ptr := Managers.Create_Manager;

         Group : aliased Orka.Resources.Models.Group_Access := null;
         use type Orka.Resources.Models.Group_Access;

         task body Resource_Test is
            use Ada.Real_Time;

            Loader_glTF : constant Loaders.Loader_Ptr := Models.glTF.Create_Loader (Manager);

            Location_Models : constant Locations.Location_Ptr
              := Locations.Directories.Create_Location (Location_Path);
         begin
            Loader.Add_Location (Location_Models, Loader_glTF);

            declare
               T1 : constant Time := Clock;

               Future_Ref : Orka.Futures.Pointers.Reference := Loader.Load (Model_Path).Get;

               use type Orka.Futures.Status;
               Resource_Status : Orka.Futures.Status;

               Resource_Future : constant Orka.Futures.Future_Access := Future_Ref.Value;
            begin
               select
                  Resource_Future.Wait_Until_Done (Resource_Status);

                  pragma Assert (Resource_Status = Orka.Futures.Done);
                  pragma Assert (Manager.Contains (Model_Path));

                  declare
                     Model_1 : constant Models.Model_Ptr
                       := Models.Model_Ptr (Manager.Resource (Model_Path));

                     Handle : Orka.Futures.Pointers.Mutable_Pointer;

                     Create_Instance_Job : constant Orka.Jobs.Job_Ptr
                       := new Orka_Package_glTF.Create_Group_Job'
                           (Orka.Jobs.Abstract_Job with Model => Model_1, Culler => Culler_1,
                           Group => Group'Unchecked_Access);
                  begin
                     Job_System.Queue.Enqueue (Create_Instance_Job, Handle);
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
         --  Clear color to black and depth to 0.0 (if using reversed Z)
         FB_1.Set_Default_Values
           ((Color => (0.0, 0.0, 0.0, 1.0),
             Depth => (if Context.Enabled (Orka.Contexts.Reversed_Z) then 0.0 else 1.0),
             others => <>));

         Texture_3.Allocate_Storage (1, Samples, GL.Pixels.RGBA8, Width, Height, 1);
         Texture_4.Allocate_Storage (1, Samples, GL.Pixels.Depth32F_Stencil8, Width, Height, 1);

         Sampler_1.Set_X_Wrapping (Clamp_To_Edge);
         Sampler_1.Set_Y_Wrapping (Clamp_To_Edge);

         Sampler_1.Set_Minifying_Filter (Nearest);
         Sampler_1.Set_Magnifying_Filter (Nearest);

         Sampler_1.Bind (0);
         Sampler_2.Bind (1);

         FB_1.Attach (Texture_3);
         FB_1.Attach (Texture_4);

         --  Load checkerboard texture array
         Load_Texture (Texture_1);

         Uni_Texture.Verify_Compatibility (Texture_1);
         Uni_Dither.Verify_Compatibility (Texture_2);

         Orka.Rendering.Textures.Bind (Texture_1, Orka.Rendering.Textures.Texture, 0);
         Orka.Rendering.Textures.Bind (Texture_2, Orka.Rendering.Textures.Texture, 1);

         Uni_Proj.Set_Matrix (Current_Camera.Projection_Matrix);

         Uni_Light.Set_Vector (Light_Position);

         Window.Set_Title ("glTF viewer - " & Model_Path);

         declare
            Group_Added : Boolean := False;
            use Ada.Real_Time;

            procedure Add_Behavior (Object : Orka.Behaviors.Behavior_Ptr);

            procedure Render_Scene
              (Scene  : not null Orka.Behaviors.Behavior_Array_Access;
               Camera : Orka.Cameras.Camera_Ptr) is
            begin
               Camera.FB.Use_Framebuffer;
               Camera.FB.Clear;

               Uni_View.Set_Matrix (Camera.View_Matrix);

               declare
                  use Orka.Cameras.Transforms;
               begin
                  --  TODO Don't re-compute projection matrix every frame
                  Culler_1.Bind (Camera.Projection_Matrix * Camera.View_Matrix);
               end;

               if Group /= null then
                  if not Group_Added then
                     Group_Added := True;
                     declare
                        Instance_1 : Orka.Resources.Models.Model_Instance_Ptr :=
                          new Orka_Package_glTF.No_Behavior'(Orka.Resources.Models.Model_Instance
                            with Position => (0.0, 0.0, 0.0, 1.0));
                     begin
                        Group.Add_Instance (Instance_1);
                        Add_Behavior (Orka.Behaviors.Behavior_Ptr (Instance_1));

                        --  For Rotate_Around_Camera and Look_At_Camera
                        if Camera.all in Observing_Camera'Class then
                           Observing_Camera'Class (Camera.all).Look_At
                             (Orka.Behaviors.Behavior_Ptr (Instance_1));
                        end if;
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
                  Camera.FB.Resolve_To (FB_D);

                  Group.After_Render;
                  for Behavior of Scene.all loop
                     Behavior.After_Render;
                  end loop;
               end if;
            end Render_Scene;

            package Loops is new Orka.Loops
              (Time_Step   => Ada.Real_Time.Microseconds (2_083),
               Frame_Limit => Ada.Real_Time.Microseconds (8_334),
               Window      => W_Ptr,
               Camera      => Current_Camera,
               Job_Manager => Job_System);

            procedure Add_Behavior (Object : Orka.Behaviors.Behavior_Ptr) is
            begin
               Loops.Scene.Add (Object);
            end Add_Behavior;
         begin
            Loops.Scene.Add (Orka.Behaviors.Null_Behavior);
            Loops.Handler.Enable_Limit (False);
            Loops.Run_Loop (Render_Scene'Access);
         end;
      exception
         when Error : others =>
            Ada.Text_IO.Put_Line ("Error: " & Exception_Information (Error));
      end;
   end;

   Job_System.Shutdown;
   Loader.Shutdown;

   Ada.Text_IO.Put_Line ("Shutdown job system and loader");
exception
   when Error : others =>
      Ada.Text_IO.Put_Line ("Error: " & Exception_Information (Error));
end Orka_GLTF;
