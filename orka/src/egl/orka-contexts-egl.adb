--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2020 onox <denkpadje@gmail.com>
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

with GL.Buffers;
with GL.Context;
with GL.Rasterization;
with GL.Toggles;
with GL.Types;
with GL.Viewports;

with Orka.Debug;
with Orka.Loggers;
with Orka.Logging.Default;

package body Orka.Contexts.EGL is

   use all type Orka.Logging.Default_Module;
   use all type Orka.Logging.Severity;
   use Orka.Logging;

   procedure Log is new Orka.Logging.Default.Generic_Log (Window_System);

   procedure Print_Error
     (Error : Standard.EGL.Errors.Error_Code;
      Level : Standard.EGL.Debug.Severity;
      Command, Message : String)
   is
      package Debug renames Standard.EGL.Debug;

      Severity : constant Orka.Loggers.Severity :=
        (case Level is
           when Debug.Critical => Loggers.Error,
           when Debug.Error    => Loggers.Error,
           when Debug.Warning  => Loggers.Warning,
           when Debug.Info     => Loggers.Debug);
   begin
      Log (Severity, Error'Image & " in " & Command & ": " & Trim (Message));
   end Print_Error;

   procedure Print_Debug
     (Display : Standard.EGL.Objects.Displays.Display;
      Version : Context_Version;
      Flags   : Context_Flags) is
   begin
      Log (Info,  "Created EGL context for " & Image (Version));
      Log (Info,  "  platform: " & Display.Platform'Image);
      declare
         Name : constant String := Display.Device.Name;
      begin
         Log (Info, "  device:   " & (if Name /= "" then Name else "unknown"));
      end;
      Log (Loggers.Debug, "  vendor:   " & Display.Vendor);
      Log (Loggers.Debug, "  version:  " & Display.Version);
      Log (Loggers.Info,  "  context:");
      Log (Loggers.Info,  "    flags:    " & Image (Flags));
      Log (Loggers.Info,  "    version:  " & GL.Context.Version_String);
      Log (Loggers.Info,  "    renderer: " & GL.Context.Renderer);
   end Print_Debug;

   procedure Post_Initialize (Object : in out EGL_Context'Class) is
      Flags : constant GL.Context.Context_Flags := GL.Context.Flags;
   begin
      Object.Version :=
        (Major => GL.Context.Major_Version,
         Minor => GL.Context.Minor_Version);

      pragma Assert (Flags.Forward_Compatible);

      Object.Flags.Debug    := Flags.Debug;
      Object.Flags.Robust   := Flags.Robust_Access;
      Object.Flags.No_Error := Flags.No_Error;
      --  Other information about context can be read back as well with
      --  GL.Context.Reset_Notification and GL.Context.Release_Behavior

      Orka.Debug.Set_Log_Messages (Enable => Flags.Debug);

      GL.Rasterization.Set_Provoking_Vertex (GL.Rasterization.First_Vertex);
      GL.Viewports.Set_Clipping (GL.Viewports.Lower_Left, GL.Viewports.Zero_To_One);

      --  Enable reversed Z for better depth precision at great distances
      --  See https://developer.nvidia.com/content/depth-precision-visualized
      --  for a visualization
      GL.Buffers.Set_Depth_Function (GL.Types.Greater);
      --  Note: When clearing the depth buffer, the value 0.0 instead of 1.0 must be used

      --  Enable MSAA, face culling, and seamless cubemaps
      GL.Toggles.Enable (GL.Toggles.Multisample);
      GL.Toggles.Enable (GL.Toggles.Cull_Face);
      GL.Toggles.Enable (GL.Toggles.Texture_Cube_Map_Seamless);

      Object.GL_Context := (Is_Present => True, Value => <>);
      Object.GL_Context.Value.Pipeline.Bind;
   end Post_Initialize;

   function Create_Context
     (Device   : Standard.EGL.Objects.Devices.Device;
      Version  : Context_Version;
      Flags    : Context_Flags := (others => False)) return Device_EGL_Context
   is
      package EGL_Contexts renames Standard.EGL.Objects.Contexts;
      package EGL_Displays renames Standard.EGL.Objects.Displays;
   begin
      Standard.EGL.Debug.Set_Message_Callback (Print_Error'Access);

      declare
         Display : constant EGL_Displays.Display := EGL_Displays.Create_Display (Device);
      begin
         return Result : Device_EGL_Context do
            Result.Context := EGL_Contexts.Create_Context
              (Display,
               (Major => Version.Major,
                Minor => Version.Minor),
               (Debug    => Flags.Debug,
                Robust   => Flags.Robust,
                No_Error => Flags.No_Error));

            Result.Context.Make_Current;

            Post_Initialize (Result);
            Print_Debug (Display, Result.Version, Flags);
         end return;
      end;
   end Create_Context;

   overriding
   function Create_Context
     (Version : Context_Version;
      Flags   : Context_Flags := (others => False)) return Device_EGL_Context
   is
      package EGL_Devices renames Standard.EGL.Objects.Devices;

      Devices : constant EGL_Devices.Device_List := EGL_Devices.Devices;
   begin
      Log (Loggers.Debug, "EGL devices:");
      for Device of Devices loop
         declare
            Name : constant String := Device.Name;
         begin
            Log (Loggers.Debug, "  - " & (if Name /= "" then Name else "unknown"));
         end;
      end loop;

      return Create_Context (Devices (Devices'First), Version, Flags);
   end Create_Context;

   overriding
   procedure Finalize (Object : in out EGL_Context) is
   begin
      if Object.Flags.Debug then
         Log (Loggers.Debug, "Shutting down EGL");
      end if;
   end Finalize;

   overriding
   procedure Update_State (Object : in out EGL_Context; State : Orka.Rendering.States.State) is
   begin
      Orka.Rendering.States.Apply_Changes (Object.Previous_State, State);
      Object.Previous_State := State;
   end Update_State;

   overriding
   procedure Bind_Shaders (Object : EGL_Context; Shaders : Orka.Rendering.Shaders.Objects.Shader_Objects) is
      use all type Orka.Rendering.Shaders.Shader_Kind;
   begin
      Object.GL_Context.Value.Pipeline.Remove_All_Program_Stages;

      for Stage in Shaders'Range when Shaders (Stage).Is_Present loop
         Object.GL_Context.Value.Pipeline.Use_Program_Stages
           ((case Stage is
               when Vertex_Shader          => (Vertex_Shader          => True, others => False),
               when Fragment_Shader        => (Fragment_Shader        => True, others => False),
               when Geometry_Shader        => (Geometry_Shader        => True, others => False),
               when Tess_Control_Shader    => (Tess_Control_Shader    => True, others => False),
               when Tess_Evaluation_Shader => (Tess_Evaluation_Shader => True, others => False),
               when Compute_Shader         => (Compute_Shader         => True, others => False)),
            Shaders (Stage).Value.GL_Program);
      end loop;

      if Object.Flags.Debug and then not Object.Flags.No_Error and then not Object.GL_Context.Value.Pipeline.Validate then
         Log (Loggers.Error, "Validation of shaders failed");
      end if;
   end Bind_Shaders;

   overriding
   procedure Make_Current (Object : Device_EGL_Context) is
   begin
      Object.Context.Make_Current;
   end Make_Current;

   overriding
   procedure Make_Not_Current (Object : Device_EGL_Context) is
   begin
      Object.Context.Make_Not_Current;
   end Make_Not_Current;

end Orka.Contexts.EGL;
