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
with Orka.Contexts;
with Orka.Debug;
with Orka.Futures;
with Orka.Inputs.GLFW;
with Orka.Inputs.Joysticks.Gamepads;
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
with Orka.Windows.GLFW;

with Orka_Package_glTF;

procedure Orka_KTX is
   Width   : constant := 1280;
   Height  : constant := 720;

   type Display_Mode is (Mode_A, Mode_B);

   package Job_System renames Orka_Package_glTF.Job_System;
   package Loader     renames Orka_Package_glTF.Loader;

   use all type Orka.Logging.Source;
   use all type Orka.Logging.Severity;

   package Messages is new Orka.Logging.Messages (Application);

   use Ada.Exceptions;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("Usage: <path to .ktx file>");
      Job_System.Shutdown;
      Loader.Shutdown;
      return;
   end if;

   Orka.Logging.Set_Logger (Orka.Loggers.Terminal.Create_Logger (Level => Orka.Loggers.Info));

   declare
      Library : constant Orka.Contexts.Library'Class
        := Orka.Windows.GLFW.Initialize (Major => 4, Minor => 2, Debug => True);

      Window : constant Orka.Windows.Window'Class := Library.Create_Window
        (Width, Height, Resizable => False);
      W_Ptr : constant Orka.Windows.Window_Ptr := Orka.Windows.Window_Ptr'(Window'Unchecked_Access);

      Context : Orka.Contexts.Context'Class := Window.Context;

      JS : Orka.Inputs.Joysticks.Joystick_Input_Access;

      JS_Manager : constant Orka.Inputs.Joysticks.Joystick_Manager_Ptr :=
        Orka.Inputs.GLFW.Create_Joystick_Manager;

      use type Orka.Inputs.Joysticks.Joystick_Input_Access;
   begin
      Orka.Debug.Set_Log_Messages (Enable => True);

      --  Updating the gamepad mappings database might be necessary
      --  in order to detect some joysticks as gamepads
      declare
         package Locations renames Orka.Resources.Locations;

         Location_Data : constant Locations.Location_Ptr
           := Locations.Directories.Create_Location ("../data");
      begin
         if Location_Data.Exists ("gamecontrollerdb.txt") then
            declare
               Mappings : constant String
                 := Orka.Resources.Convert (Orka.Resources.Byte_Array'(Location_Data.Read_Data
                      ("gamecontrollerdb.txt").Get));
            begin
               Orka.Inputs.GLFW.Update_Gamepad_Mappings (Mappings);
               Messages.Log (Info, "Updated gamepad mappings");
            end;
         end if;
      end;

      JS_Manager.Acquire (JS);

      if JS /= null then
         Messages.Log (Info, "Found " & (if JS.Is_Gamepad then "gamepad" else "joystick"));
         Messages.Log (Info, "  name: " & JS.Name);
         Messages.Log (Info, "  GUID: " & JS.GUID);
      end if;

      declare
         Full_Path     : constant String := Ada.Command_Line.Argument (1);

         Location_Path : constant String := Ada.Directories.Containing_Directory (Full_Path);
         Texture_Path  : constant String := Ada.Directories.Simple_Name (Full_Path);

         use Orka.Rendering.Programs;
         use Orka.Rendering.Framebuffers;
         use Orka.Resources;

         use GL.Types;
         use all type Orka.Types.Index_Type;

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
           := Locations.Directories.Create_Location ("../data/shaders");

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
            Mode   : Display_Mode;
            Colors : Boolean;
            Level, Levels : Mipmap_Level)
         is
            use all type LE.Texture_Kind;
            use type Display_Mode;

            Text : SU.Unbounded_String := SU.To_Unbounded_String ("KTX viewer - " & Texture_Path);
         begin
            if Levels > 1 then
               SU.Append (Text, " (level " & Orka.Logging.Trim (Mipmap_Level'Image (Level + 1)) &
                 "/" & Orka.Logging.Trim (Levels'Image) & ")");
            end if;

            case Kind is
               when Texture_1D | Texture_2D | Texture_2D_Array =>
                  SU.Append (Text, " (best fit: " & Boolean'Image (Mode = Mode_A) & ")");
               when Texture_3D | Texture_Cube_Map =>
                  SU.Append (Text, " (external view: " & Boolean'Image (Mode = Mode_B) & ")");
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
            package Gamepads renames Orka.Inputs.Joysticks.Gamepads;

            Level : Mipmap_Level := 0;

            Render_Mode   : Display_Mode := Mode_A;
            Render_Colors : Boolean      := False;

            procedure Render
              (Scene  : not null Orka.Behaviors.Behavior_Array_Access;
               Camera : Orka.Cameras.Camera_Ptr)
            is
               use all type LE.Texture_Kind;
            begin
               Camera.FB.Clear;

               if JS /= null and then JS.Is_Present and then JS.Is_Gamepad then
                  JS.Update_State (null);

                  declare
                     Last_State : constant Orka.Inputs.Joysticks.Joystick_State := JS.Last_State;
                     State      : constant Orka.Inputs.Joysticks.Joystick_State := JS.Current_State;

                     use all type Orka.Inputs.Joysticks.Button_State;
                     use all type Gamepads.Button;
                  begin
                     for Index in 1 .. State.Button_Count loop
                        if State.Buttons (Index) /= Last_State.Buttons (Index)
                          and State.Buttons (Index) = Pressed
                        then
                           case Gamepads.Button'(Gamepads.Value (Index)) is
                              when Left_Pad_Left =>
                                 Level := Mipmap_Level'Max (0, Level - 1);
                              when Left_Pad_Right =>
                                 Level := Mipmap_Level'Min (Maximum_Level, Level + 1);
                              when Right_Pad_Up =>
                                 if Render_Mode = Display_Mode'Last then
                                    Render_Mode := Display_Mode'First;
                                 else
                                    Render_Mode := Display_Mode'Succ (Render_Mode);
                                 end if;
                              when Right_Pad_Down =>
                                 Render_Colors := not Render_Colors;
                              when others => null;
                           end case;
                        end if;
                     end loop;
                  end;
               end if;

               T_1.Set_Lowest_Mipmap_Level (Level);
               Update_Title (T_1.Kind, Render_Mode, Render_Colors, Level, Maximum_Level + 1);

               case T_1.Kind is
                  when Texture_1D | Texture_2D | Texture_2D_Array =>
                     declare
                        Uni_Screen   : constant Uniforms.Uniform := P_1.Uniform ("screenSize");
                        Uni_Best_Fit : constant Uniforms.Uniform := P_1.Uniform ("useBestFit");
                     begin
                        Uni_Screen.Set_Vector (Orka.Types.Singles.Vector4'
                          (Single (Window.Width), Single (Window.Height), 0.0, 0.0)
                        );
                        Uni_Best_Fit.Set_Boolean (Render_Mode = Mode_A);
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

                        Uni_External.Set_Boolean (Render_Mode = Mode_B);
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
               Window      => W_Ptr,
               Camera      => Current_Camera,
               Job_Manager => Job_System);
         begin
            Loops.Scene.Add (Orka.Behaviors.Null_Behavior);
            Loops.Run_Loop (Render'Access);
         end;
      end;
   end;

   Job_System.Shutdown;
   Loader.Shutdown;
exception
   when Error : others =>
      Ada.Text_IO.Put_Line ("Error: " & Exception_Information (Error));
end Orka_KTX;
