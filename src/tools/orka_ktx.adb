--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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
with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Text_IO;

with GL.Objects.Textures;
with GL.Types;

with Orka.Behaviors;
with Orka.Contexts;
with Orka.Cameras.Rotate_Around_Cameras;
with Orka.Loops;
with Orka.Debug;
with Orka.Futures;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Programs.Uniforms;
with Orka.Rendering.Vertex_Formats;
with Orka.Resources.Loaders;
with Orka.Resources.Locations.Directories;
with Orka.Resources.Managers;
with Orka.Resources.Textures.KTX;
with Orka.Types;
with Orka.Windows.GLFW;

with Orka_Package_glTF;

procedure Orka_KTX is
   Width   : constant := 1280;
   Height  : constant := 720;

   Initialized : constant Orka.Windows.GLFW.Active_GLFW'Class
     := Orka.Windows.GLFW.Initialize (Major => 4, Minor => 2, Debug => True);
   pragma Unreferenced (Initialized);

   W : constant Orka.Windows.Window'Class := Orka.Windows.GLFW.Create_Window
     (Width, Height, Resizable => False);
   W_Ptr : constant Orka.Windows.Window_Ptr := Orka.Windows.Window_Ptr'(W'Unchecked_Access);

   Context : Orka.Contexts.Context;

   package Job_System renames Orka_Package_glTF.Job_System;
   package Loader renames Orka_Package_glTF.Loader;

   use Ada.Exceptions;
begin
   if Ada.Command_Line.Argument_Count /= 2 then
      Ada.Text_IO.Put_Line ("Usage: <path to data folder> <relative path to .ktx file>");
      Job_System.Shutdown;
      Loader.Shutdown;
      return;
   end if;

   Orka.Debug.Set_Log_Messages (Enable => True);
   Ada.Text_IO.Put_Line ("Set callback for debug messages");

   --  Enable some features
   Context.Enable (Orka.Contexts.Reversed_Z);

   declare
      Location_Path : constant String := Ada.Command_Line.Argument (1);
      Texture_Path  : constant String := Ada.Command_Line.Argument (2);

      package Textures renames Orka.Resources.Textures;
      package Formats renames Orka.Rendering.Vertex_Formats;

      use Orka.Rendering.Programs;
      use Orka.Rendering.Framebuffers;
      use Orka.Resources;

      use GL.Types;
      use all type Orka.Types.Index_Type;

      Location_Shaders : constant Locations.Location_Ptr
        := Locations.Directories.Create_Location ("../data");

      P_1 : Program := Create_Program (Modules.Create_Module
        (Location_Shaders,
         VS => "oversized-triangle.vert",
         FS => "shaders/tools/ktx.frag"));

      Uni_Texture : constant Uniforms.Uniform_Sampler := P_1.Uniform_Sampler ("colorTexture");

      Screen_Size : constant Uniforms.TS.Vector4
        := (Single (Width), Single (Height), 0.0, 0.0);

      Uni_Screen : constant Uniforms.Uniform := P_1.Uniform ("screenSize");

      --  Create an empty vertex format. Vertex shader contains the data needed
      --  to generate a quad
      VF_1 : constant Formats.Vertex_Format
        := Formats.Create_Vertex_Format (GL.Types.Triangles, UInt_Type);

      ----------------------------------------------------------------------

      FB_D : constant Framebuffer_Ptr
        := new Framebuffer'(Get_Default_Framebuffer (W));

      --  The camera provides a view and projection matrices, but are unused
      --  in the shaders. The object is created because package Loops needs it
      use Orka.Cameras;
      Lens : constant Lens_Ptr
        := new Camera_Lens'Class'(Create_Lens (Width, Height, 45.0, Context));
      Current_Camera : constant Camera_Ptr
           := new Camera'Class'(Camera'Class
                (Rotate_Around_Cameras.Create_Camera (W.Pointer_Input, Lens, FB_D)));

      ----------------------------------------------------------------------

      task Load_Resource;

      use Ada.Real_Time;

      Manager : constant Managers.Manager_Ptr := Managers.Create_Manager;
      Loading_Failed : Boolean := False;

      task body Load_Resource is
         Loader_KTX : constant Loaders.Loader_Ptr := Textures.KTX.Create_Loader (Manager);
      begin
         declare
            Location_Textures : constant Locations.Location_Ptr
              := Locations.Directories.Create_Location (Location_Path);
         begin
            Loader.Add_Location (Location_Textures, Loader_KTX);
         end;

         declare
            Future_Ref : Orka.Futures.Pointers.Reference := Loader.Load (Texture_Path).Get;

            use type Orka.Futures.Status;
            Resource_Status : Orka.Futures.Status;
         begin
            Future_Ref.Wait_Until_Done (Resource_Status);

            pragma Assert (Resource_Status = Orka.Futures.Done);
            pragma Assert (Manager.Contains (Texture_Path));

            --  Here we should either create a GPU job to set the
            --  texture to the uniform or just simply set it in the
            --  render callback below
         end;
      exception
         when Error : others =>
            Ada.Text_IO.Put_Line ("Error loading resource: " & Exception_Message (Error));
            Loading_Failed := True;
      end Load_Resource;
   begin
      Uni_Screen.Set_Vector (Screen_Size);

      --  Clear color to black and depth to 0.0 (because of reversed Z)
      FB_D.Set_Default_Values ((Color => (0.0, 0.0, 0.0, 0.0), Depth => 0.0, others => <>));

      declare
         use GL.Objects.Textures;
         Loaded : Boolean := False;

         procedure Render
           (Scene  : not null Orka.Behaviors.Behavior_Array_Access;
            Camera : Orka.Cameras.Camera_Ptr) is
         begin
            Camera.FB.Clear;

            Camera.FB.Use_Framebuffer;
            P_1.Use_Program;

            if Loading_Failed then
               raise Program_Error with "Loading resource failed";
            end if;

            if not Loaded and then Manager.Contains (Texture_Path) then
               declare
                  T_1 : constant Textures.Texture_Ptr
                    := Textures.Texture_Ptr (Manager.Resource (Texture_Path));

                  T_2 : constant GL.Objects.Textures.Texture := T_1.Element;
                  --  TODO Handle non-Texture_2D textures
               begin
                  T_2.Set_X_Wrapping (Clamp_To_Edge);
                  T_2.Set_X_Wrapping (Clamp_To_Edge);

                  T_2.Set_Minifying_Filter (Nearest);
                  T_2.Set_Magnifying_Filter (Nearest);

                  T_2.Set_Lowest_Mipmap_Level (0);

                  Uni_Texture.Set_Texture (T_2, 0);
               end;
               Ada.Text_IO.Put_Line ("Set texture " & Texture_Path);
               Loaded := True;
            end if;

            VF_1.Draw (0, 3);
         end Render;

         package Loops is new Orka.Loops
           (Time_Step   => Ada.Real_Time.Microseconds (2_083),
            Frame_Limit => Ada.Real_Time.Microseconds (16_667),
            Window      => W_Ptr,
            Camera      => Current_Camera,
            Render      => Render'Unrestricted_Access,
            Job_Manager => Job_System);
      begin
         Loops.Scene.Add (Orka.Behaviors.Null_Behavior);
         Ada.Text_IO.Put_Line ("Running render loop...");
         Loops.Run_Loop;
      end;
      Ada.Text_IO.Put_Line ("Shutting down...");
      Job_System.Shutdown;
      Loader.Shutdown;
      Ada.Text_IO.Put_Line ("Shutdown job system and loader");
   end;
exception
   when Error : others =>
      Ada.Text_IO.Put_Line ("Error: " & Exception_Information (Error));
end Orka_KTX;
