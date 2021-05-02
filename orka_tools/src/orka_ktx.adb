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
with Ada.Directories;
with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Text_IO;

with GL.Objects.Samplers;
with GL.Objects.Textures;
with GL.Toggles;
with GL.Types;
with GL.Viewports;

with Orka.Behaviors;
with Orka.Cameras.Rotate_Around_Cameras;
with Orka.Contexts.AWT;
with Orka.Debug;
with Orka.Loggers.Terminal;
with Orka.Logging;
with Orka.Loops;
with Orka.Rendering.Drawing;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Programs.Uniforms;
with Orka.Rendering.Textures;
with Orka.Resources.Locations.Directories;
with Orka.Resources.Textures.KTX;
with Orka.Types;
with Orka.Windows;

with AWT.Inputs;

with Orka_Package_KTX;

procedure Orka_KTX is
   Width   : constant := 1280;
   Height  : constant := 720;

   type Zoom_Mode is (Best_Fit, Actual_Size);

   type View_Mode is (External, Internal);

   package Job_System renames Orka_Package_KTX.Job_System;
   package Loader     renames Orka_Package_KTX.Loader;

   use all type Orka.Logging.Source;
   use all type Orka.Logging.Severity;

   package Messages is new Orka.Logging.Messages (Application);
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("Usage: <path to .ktx file>");
      Job_System.Shutdown;
      Loader.Shutdown;
      return;
   end if;

   Orka.Logging.Set_Logger (Orka.Loggers.Terminal.Create_Logger (Level => Orka.Loggers.Info));

   declare
      Context : constant Orka.Contexts.Context'Class := Orka.Contexts.AWT.Create_Context
        (Version => (4, 2), Flags  => (Debug => True, others => False));

      Window : constant Orka.Windows.Window'Class
        := Orka.Contexts.AWT.Create_Window (Context, Width, Height, Resizable => False);
   begin
      Orka.Debug.Set_Log_Messages (Enable => True);

      declare
         Full_Path     : constant String := Ada.Command_Line.Argument (1);

         Location_Path : constant String := Ada.Directories.Containing_Directory (Full_Path);
         Texture_Path  : constant String := Ada.Directories.Simple_Name (Full_Path);

         use Orka.Rendering.Programs;
         use Orka.Rendering.Framebuffers;
         use Orka.Resources;

         use GL.Types;

         ----------------------------------------------------------------------

         FB_D : Framebuffer := Get_Default_Framebuffer (Window);

         use Orka.Cameras;
         Lens : constant Lens_Ptr
           := new Camera_Lens'Class'(Create_Lens
                (Width, Height, Transforms.FOV (36.0, 50.0), Context));
         Current_Camera : constant Camera_Ptr
              := new Camera'Class'(Camera'Class
                   (Rotate_Around_Cameras.Create_Camera (Window.Pointer_Input, Lens, FB_D)));

         ----------------------------------------------------------------------

         use GL.Objects.Textures;
         use GL.Objects.Samplers;

         Sampler_1 : Sampler;

         Location_Textures : constant Locations.Location_Ptr :=
           Locations.Directories.Create_Location (Location_Path);

         T_1 : constant Texture := Orka.Resources.Textures.KTX.Read_Texture
           (Location_Textures, Texture_Path);

         Maximum_Level : constant Mipmap_Level := T_1.Mipmap_Levels - 1;

         ----------------------------------------------------------------------

         Location_Shaders : constant Locations.Location_Ptr
           := Locations.Directories.Create_Location ("data/shaders");

         function Get_Module (Kind : LE.Texture_Kind) return Modules.Module is
            use all type LE.Texture_Kind;
         begin
            case Kind is
               when Texture_1D =>
                  return Modules.Create_Module (Location_Shaders,
                    VS => "oversized-triangle.vert",
                    FS => "tools/ktx-1D.frag");
               when Texture_2D =>
                  return Modules.Create_Module (Location_Shaders,
                    VS => "oversized-triangle.vert",
                    FS => "tools/ktx-2D.frag");
               when Texture_3D =>
                  return Modules.Create_Module (Location_Shaders,
                    VS => "tools/volume.vert",
                    FS => "tools/ktx-3D.frag");
               when Texture_Cube_Map =>
                  return Modules.Create_Module (Location_Shaders,
                    VS => "tools/cube.vert",
                    FS => "tools/ktx-cube.frag");
               when Texture_2D_Array =>
                  return Modules.Create_Module (Location_Shaders,
                    VS => "oversized-triangle.vert",
                    FS => "tools/ktx-2D-array.frag");
               when others => raise Constraint_Error with "Unsupported texture kind";
            end case;
         end Get_Module;

         procedure Draw (Kind : LE.Texture_Kind) is
            use all type LE.Texture_Kind;
         begin
            case Kind is
               when Texture_1D | Texture_2D | Texture_2D_Array =>
                  Orka.Rendering.Drawing.Draw (GL.Types.Triangles, 0, 3);
               when Texture_3D | Texture_Cube_Map =>
                  Orka.Rendering.Drawing.Draw (GL.Types.Triangles, 0, 6 * 6);
               when others => raise Constraint_Error;
            end case;
         end Draw;

         procedure Update_Title
           (Kind   : LE.Texture_Kind;
            Mode   : Zoom_Mode;
            View   : View_Mode;
            Colors : Boolean;
            Level, Levels : Mipmap_Level)
         is
            use all type LE.Texture_Kind;
            use type Zoom_Mode;
            use type View_Mode;

            Text : SU.Unbounded_String := SU.To_Unbounded_String ("KTX viewer - " & Texture_Path);
         begin
            if Levels > 1 then
               SU.Append (Text, " (level " & Orka.Logging.Trim (Mipmap_Level'Image (Level + 1)) &
                 "/" & Orka.Logging.Trim (Levels'Image) & ")");
            end if;

            case Kind is
               when Texture_1D | Texture_2D | Texture_2D_Array =>
                  SU.Append (Text, " (zoom: " & Mode'Image & ")");
               when Texture_3D | Texture_Cube_Map =>
                  SU.Append (Text, " (view: " & View'Image & ")");
                  SU.Append (Text, " (colors: " & Colors'Image & ")");
               when others => null;
            end case;

            Window.Set_Title (SU.To_String (Text));
         end Update_Title;

         P_1 : Program := Create_Program (Get_Module (T_1.Kind));

         Uni_Texture : constant Uniforms.Uniform_Sampler := P_1.Uniform_Sampler ("colorTexture");
      begin
         --  Clear color to black and depth to 0.0 (if using reversed Z)
         FB_D.Set_Default_Values
           ((Color => (0.0, 0.0, 0.0, 1.0),
             Depth => (if Context.Enabled (Orka.Contexts.Reversed_Z) then 0.0 else 1.0),
             others => <>));

         FB_D.Use_Framebuffer;

         P_1.Use_Program;

         Uni_Texture.Verify_Compatibility (T_1);
         Orka.Rendering.Textures.Bind (T_1, Orka.Rendering.Textures.Texture, 0);

         Sampler_1.Set_X_Wrapping (Clamp_To_Edge);
         Sampler_1.Set_Y_Wrapping (Clamp_To_Edge);

         Sampler_1.Set_Minifying_Filter (Linear);
         Sampler_1.Set_Magnifying_Filter (Linear);

         Sampler_1.Bind (0);

         GL.Toggles.Enable (GL.Toggles.Depth_Test);
         GL.Toggles.Enable (GL.Toggles.Texture_Cube_Map_Seamless);

         declare
            Level : Mipmap_Level := 0 with Atomic;

            Render_Zoom   : Zoom_Mode := Best_Fit with Atomic;
            Render_View   : View_Mode := External with Atomic;
            Render_Colors : Boolean   := False  with Atomic;

            procedure Render
              (Scene  : not null Orka.Behaviors.Behavior_Array_Access;
               Camera : Orka.Cameras.Camera_Ptr)
            is
               use all type LE.Texture_Kind;
            begin
               Camera.FB.Clear;

               T_1.Set_Lowest_Mipmap_Level (Level);
               Update_Title
                 (T_1.Kind, Render_Zoom, Render_View, Render_Colors, Level, Maximum_Level + 1);

               case T_1.Kind is
                  when Texture_1D | Texture_2D | Texture_2D_Array =>
                     declare
                        Uni_Screen   : constant Uniforms.Uniform := P_1.Uniform ("screenSize");
                        Uni_Best_Fit : constant Uniforms.Uniform := P_1.Uniform ("useBestFit");
                     begin
                        Uni_Screen.Set_Vector (Orka.Types.Singles.Vector4'
                          (Single (Window.Width), Single (Window.Height), 0.0, 0.0)
                        );
                        Uni_Best_Fit.Set_Boolean (Render_Zoom = Best_Fit);
                     end;
                  when Texture_3D | Texture_Cube_Map =>
                     declare
                        Uni_View : constant Uniforms.Uniform := P_1.Uniform ("view");
                        Uni_Proj : constant Uniforms.Uniform := P_1.Uniform ("proj");

                        Uni_External : constant Uniforms.Uniform := P_1.Uniform ("showExternal");
                        Uni_Colors   : constant Uniforms.Uniform := P_1.Uniform ("showColors");
                     begin
                        Uni_View.Set_Matrix (Camera.View_Matrix);
                        Uni_Proj.Set_Matrix (Camera.Projection_Matrix);

                        Uni_External.Set_Boolean (Render_View = External);
                        Uni_Colors.Set_Boolean (Render_Colors);
                     end;
                  when others => null;
               end case;

               --  TODO When the window gets resized, it should automatically update
               --  the default framebuffer
               GL.Viewports.Set_Viewports
                 ((0 => (X      => 0.0,
                         Y      => 0.0,
                         Width  => Single (Window.Width),
                         Height => Single (Window.Height))
                 ));

               Draw (T_1.Kind);
            end Render;

            package Loops is new Orka.Loops
              (Time_Step   => Ada.Real_Time.Microseconds (2_083),
               Frame_Limit => Ada.Real_Time.Microseconds (16_667),
               Window      => Window'Unchecked_Access,
               Camera      => Current_Camera,
               Job_Manager => Job_System);
         begin
            Loops.Scene.Add (Orka.Behaviors.Null_Behavior);

            declare
               task Render_Task is
                  entry Start_Rendering;
               end Render_Task;

               task body Render_Task is
               begin
                  accept Start_Rendering;

                  Context.Make_Current (Window);
                  Loops.Run_Loop (Render'Access, Loops.Stop_Loop'Access);
                  Context.Make_Not_Current;
               exception
                  when Error : others =>
                     Ada.Text_IO.Put_Line ("Error: " &
                       Ada.Exceptions.Exception_Information (Error));
                     Context.Make_Not_Current;
                     raise;
               end Render_Task;
            begin
               Context.Make_Not_Current;
               Render_Task.Start_Rendering;

               while not Window.Should_Close and then AWT.Process_Events (0.016667) loop
                  declare
                     Keyboard : constant AWT.Inputs.Keyboard_State := Window.State;

                     use all type AWT.Inputs.Keyboard_Button;
                  begin
                     if Keyboard.Pressed (Key_Escape) then
                        Window.Close;
                     end if;

                     if Keyboard.Pressed (Key_Arrow_Down) then
                        Level := Mipmap_Level'Max (0, Level - 1);
                     end if;

                     if Keyboard.Pressed (Key_Arrow_Up) then
                        Level := Mipmap_Level'Min (Maximum_Level, Level + 1);
                     end if;

                     if Keyboard.Pressed (Key_Z) then
                        if Render_Zoom = Zoom_Mode'Last then
                           Render_Zoom := Zoom_Mode'First;
                        else
                           Render_Zoom := Zoom_Mode'Succ (Render_Zoom);
                        end if;
                     end if;

                     if Keyboard.Pressed (Key_V) then
                        if Render_View = View_Mode'Last then
                           Render_View := View_Mode'First;
                        else
                           Render_View := View_Mode'Succ (Render_View);
                        end if;
                     end if;

                     if Keyboard.Pressed (Key_C) then
                        Render_Colors := not Render_Colors;
                     end if;
                  end;
               end loop;
            end;
         end;
      end;
   end;

   Job_System.Shutdown;
   Loader.Shutdown;
exception
   when Error : others =>
      Ada.Text_IO.Put_Line ("Error: " & Ada.Exceptions.Exception_Information (Error));
      Job_System.Shutdown;
      Loader.Shutdown;
end Orka_KTX;
