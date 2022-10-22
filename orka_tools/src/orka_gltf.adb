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
with Ada.Exceptions;
with Ada.Real_Time;

with GL.Low_Level.Enums;
with GL.Objects.Samplers;
with GL.Objects.Textures;
with GL.Pixels;
with GL.Types;

with Orka.Behaviors;
with Orka.Contexts.AWT;
with Orka.Cameras.Rotate_Around_Cameras;
with Orka.Culling;
with Orka.Debug;
with Orka.Frame_Graphs;
with Orka.Futures;
with Orka.Jobs;
with Orka.Loggers.Terminal;
with Orka.Logging.Default;
with Orka.Loops;
with Orka.OS;
with Orka.Rendering.Drawing;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Programs.Uniforms;
with Orka.Rendering.States;
with Orka.Rendering.Textures;
with Orka.Resources.Loaders;
with Orka.Resources.Locations.Directories;
with Orka.Resources.Managers;
with Orka.Resources.Models.glTF;
with Orka.Types;
with Orka.Windows;

with AWT.Inputs;
with AWT.Monitors;

with Orka_Package_glTF;

procedure Orka_GLTF is
   Width   : constant := 1280;
   Height  : constant := 720;
   Samples : constant := 2;

   Light_Position : constant Orka.Types.Singles.Vector4 := (0.0, 0.0, 0.0, 1.0);

   use all type Orka.Logging.Default_Module;
   use all type Orka.Logging.Severity;

   procedure Log is new Orka.Logging.Default.Generic_Log (Application);

   ----------------------------------------------------------------------

   procedure Load_Texture (Texture : in out GL.Objects.Textures.Texture) is
      Pixels : aliased constant Orka.Float_32_Array
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
      Orka.OS.Put_Line ("Usage: <path to .gltf file");
      Job_System.Shutdown;
      Loader.Shutdown;
      return;
   end if;

   Orka.Logging.Set_Logger (Orka.Loggers.Terminal.Create_Logger (Level => Orka.Loggers.Debug));

   AWT.Initialize;
   for Monitor of AWT.Monitors.Monitors loop
      Monitor.Log_Information;
   end loop;

   declare
      Context : constant Orka.Contexts.Context'Class := Orka.Contexts.AWT.Create_Context
        (Version => (4, 2), Flags => (Debug => True, others => False));

      Window : constant Orka.Windows.Window'Class
        := Orka.Contexts.AWT.Create_Window (Context, Width, Height);
   begin
      Orka.Debug.Set_Log_Messages (Enable => True, Raise_API_Error => True);

      declare
         Full_Path     : constant String := Ada.Command_Line.Argument (1);

         Location_Path : constant String := Ada.Directories.Containing_Directory (Full_Path);
         Model_Path    : constant String := Ada.Directories.Simple_Name (Full_Path);

         use Orka.Resources;

         Location_Shaders : constant Locations.Location_Ptr
           := Locations.Directories.Create_Location ("data/shaders");

         package LE renames GL.Low_Level.Enums;

         use Orka.Rendering.Programs;
         use Orka.Rendering.Framebuffers;

         P_1 : Program := Create_Program (Modules.Create_Module
           (Location_Shaders,
            VS => "tools/gltf.vert",
            FS => "tools/gltf.frag"));

         P_2 : Program := Create_Program (Modules.Create_Module
           (Location_Shaders,
            VS => "oversized-triangle.vert",
            FS => "tools/resolve.frag"));

         Uni_View  : constant Uniforms.Uniform := P_1.Uniform ("view");
         Uni_Proj  : constant Uniforms.Uniform := P_1.Uniform ("proj");

         Uni_Light : constant Uniforms.Uniform := P_1.Uniform ("lightPosition");

         ----------------------------------------------------------------------

         Uni_Texture : constant Uniforms.Uniform_Sampler := P_1.Uniform_Sampler ("diffuseTexture");

         use GL.Objects.Textures;
         use GL.Objects.Samplers;

         Texture_1 : Texture (LE.Texture_2D_Array);

         Sampler_1 : Sampler;

         ----------------------------------------------------------------------

         FB_D : constant Framebuffer := Create_Default_Framebuffer (Window.Width, Window.Height);

         Lens : constant Orka.Cameras.Camera_Lens :=
           Orka.Cameras.Create_Lens
             (Width, Height, Orka.Cameras.Transforms.FOV (36.0, 50.0));
         Current_Camera : aliased Orka.Cameras.Rotate_Around_Cameras.Rotate_Around_Camera :=
           Orka.Cameras.Rotate_Around_Cameras.Create_Camera (Lens);

         use Orka.Culling;
         Culler_1 : aliased Culler := Create_Culler (Location_Shaders);

         ----------------------------------------------------------------------

         use Orka.Frame_Graphs;

         Graph_Builder : Orka.Frame_Graphs.Builder
           (Maximum_Passes    => 3,
            Maximum_Handles   => 10,
            Maximum_Resources => 10);

         Default_State    : constant Orka.Rendering.States.State := (others => <>);
         Fullscreen_State : constant Orka.Rendering.States.State :=
           (Depth_Func => GL.Types.Always, others => <>);

         Pass_1 : Render_Pass'Class := Graph_Builder.Add_Pass ("P1", Default_State);
         Pass_2 : Render_Pass'Class := Graph_Builder.Add_Pass ("P2", Fullscreen_State);

         Resource_1 : constant Orka.Frame_Graphs.Resource :=
           (Name    => +"R1",
            Kind    => LE.Texture_2D_Multisample,
            Format  => GL.Pixels.R11F_G11F_B10F,
            Extent  => (1280, 720, 1),
            Samples => Samples,
            others  => <>);

         Resource_2 : constant Orka.Frame_Graphs.Resource :=
           (Name    => +"R2",
            Kind    => LE.Texture_2D_Multisample,
            Format  => GL.Pixels.Depth_Component32F,
            Extent  => (1280, 720, 1),
            Samples => Samples,
            others  => <>);

         Resource_3 : constant Orka.Frame_Graphs.Resource :=
           (Name    => +"R3",
            Kind    => LE.Texture_2D,
            Format  => GL.Pixels.R11F_G11F_B10F,
            Extent  => (Positive (FB_D.Width), Positive (FB_D.Height), 1),
            Samples => 0,
            others  => <>);

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
                           (Orka.Jobs.Abstract_Job with
                              Model  => Model_1,
                              Culler => Culler_1'Unchecked_Access,
                           Group => Group'Unchecked_Access);
                  begin
                     Job_System.Queue.Enqueue (Create_Instance_Job, Handle);
                  end;
               or
                  delay until T1 + Milliseconds (4000);
                  Log (Failure, "Not completed loading: " & Future_Ref.Current_Status'Image);
               end select;
            end;
         exception
            when Error : others =>
               Log (Orka.Loggers.Error, "Error loading resource: " & Exception_Information (Error));
         end Resource_Test;
      begin
         Pass_1.Add_Output (Resource_1, Framebuffer_Attachment, 0);
         Pass_1.Add_Output (Resource_2, Framebuffer_Attachment, 1);

         Pass_2.Add_Input (Resource_1, Texture_Read, 0);
         Pass_2.Add_Output (Resource_3, Framebuffer_Attachment, 0);

         Sampler_1.Set_X_Wrapping (Clamp_To_Edge);
         Sampler_1.Set_Y_Wrapping (Clamp_To_Edge);

         Sampler_1.Set_Minifying_Filter (Nearest);
         Sampler_1.Set_Magnifying_Filter (Nearest);

         Sampler_1.Bind (0);

         --  Load checkerboard texture array
         Load_Texture (Texture_1);
         Uni_Texture.Verify_Compatibility (Texture_1);
         Orka.Rendering.Textures.Bind (Texture_1, Orka.Rendering.Textures.Texture, 0);

         Uni_Proj.Set_Matrix (Current_Camera.Projection_Matrix);
         Uni_Light.Set_Vector (Light_Position);

         Window.Set_Title ("glTF viewer - " & Model_Path);

         declare
            Group_Added : Boolean := False;

            package Loops is new Orka.Loops
              (Time_Step   => Ada.Real_Time.Microseconds (2_083),
               Frame_Limit => Ada.Real_Time.Microseconds (16_667),
               Camera      => Current_Camera'Unchecked_Access,
               Job_Manager => Job_System);

            procedure Add_Behavior (Object : Orka.Behaviors.Behavior_Ptr);

            FG : Orka.Frame_Graphs.Graph'Class := Graph_Builder.Cull (Present => Resource_3);

            procedure Render_Scene
              (Scene  : not null Orka.Behaviors.Behavior_Array_Access;
               Camera : Orka.Cameras.Camera_Ptr)
            is
               procedure Run_Frame (Pass : Render_Pass_Data) is
                  Name : constant String := Orka.Frame_Graphs.Name (Pass);
               begin
                  if Name = "P1" then
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
                              if Camera.all in Orka.Cameras.Observing_Camera'Class then
                                 Orka.Cameras.Observing_Camera'Class (Camera.all).Look_At
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
                     end if;
                  elsif Name = "P2" then
                     P_2.Uniform ("screenResolution").Set_Vector (Orka.Types.Singles.Vector4'
                      (Orka.Float_32 (FB_D.Width), Orka.Float_32 (FB_D.Height), 0.0, 0.0));
                     P_2.Uniform ("exposure").Set_Single (1.0);
                     P_2.Use_Program;

                     Orka.Rendering.Drawing.Draw (GL.Types.Triangles, 0, 3);
                  end if;
               end Run_Frame;
            begin
               FG.Render (Context, Run_Frame'Access);

               if Group /= null then
                  Group.After_Render;
                  for Behavior of Scene.all loop
                     Behavior.After_Render;
                  end loop;
               end if;

               Window.Swap_Buffers;

               if Window.Should_Close then
                  Loops.Stop_Loop;
               end if;
            end Render_Scene;

            procedure Add_Behavior (Object : Orka.Behaviors.Behavior_Ptr) is
            begin
               Loops.Scene.Add (Object);
            end Add_Behavior;
         begin
            Loops.Scene.Add (Orka.Behaviors.Null_Behavior);
            Loops.Handler.Enable_Limit (False);

            FG.Initialize (Location_Shaders, FB_D);
            FG.Log_Graph;

            declare
               task Render_Task is
                  entry Start_Rendering;
               end Render_Task;

               task body Render_Task is
               begin
                  accept Start_Rendering;

                  Context.Make_Current (Window);
                  Loops.Run_Loop (Render_Scene'Access);
                  Context.Make_Not_Current;
               exception
                  when Error : others =>
                     Log (Orka.Loggers.Error, "Error: " & Exception_Information (Error));
                     Context.Make_Not_Current;
                     raise;
               end Render_Task;
            begin
               Context.Make_Not_Current;
               Render_Task.Start_Rendering;

               while not Window.Should_Close loop
                  AWT.Process_Events (0.016667);

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

                     declare
                        New_Mode : constant AWT.Inputs.Pointer_Mode :=
                          (if Rotate_Camera then Locked else Visible);
                     begin
                        if Pointer.Mode /= New_Mode then
                           Window.Set_Pointer_Mode (New_Mode);
                        end if;
                     end;

                     --  FIXME Update with PO?
                     Current_Camera.Change_Orientation
                       (((if Rotate_Camera then Orka.Float_64 (Pointer.Relative (X)) else 0.0),
                         (if Rotate_Camera then Orka.Float_64 (Pointer.Relative (Y)) else 0.0),
                         Orka.Float_64 (Pointer.Scroll (Y)),
                         0.0));
                  end;
               end loop;
            end;
         end;
      exception
         when Error : others =>
            Log (Orka.Loggers.Error, "Error: " & Exception_Information (Error));
      end;
   end;

   Job_System.Shutdown;
   Loader.Shutdown;
exception
   when Error : others =>
      Orka.OS.Put_Line ("Error: " & Exception_Information (Error));
      Job_System.Shutdown;
      Loader.Shutdown;
end Orka_GLTF;
