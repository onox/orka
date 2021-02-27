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

with GL.Context;
with GL.Viewports;

with Orka.Loggers;
with Orka.Logging;

with EGL.Debug;
with EGL.Errors;

package body Orka.Contexts.EGL is

   use all type Orka.Logging.Source;
   use all type Orka.Logging.Severity;
   use Orka.Logging;

   package Messages is new Orka.Logging.Messages (Window_System);

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
      Messages.Log (Severity, Error'Image & " in " & Command & ": " & Trim (Message));
   end Print_Error;

   procedure Print_Debug
     (Display : Standard.EGL.Objects.Displays.Display;
      Flags   : Context_Flags) is
   begin
      Messages.Log (Debug, "Created EGL context");
      Messages.Log (Debug, "  platform: " & Display.Platform'Image);
      declare
         Name : constant String := Display.Device.Name;
      begin
         Messages.Log (Debug, "  device:   " & (if Name /= "" then Name else "unknown"));
      end;
      Messages.Log (Debug, "  vendor:   " & Display.Vendor);
      Messages.Log (Debug, "  version:  " & Display.Version);
      Messages.Log (Debug, "  context:");
      Messages.Log (Debug, "    flags:    " & Image (Flags));
      Messages.Log (Debug, "    version:  " & GL.Context.Version_String);
      Messages.Log (Debug, "    renderer: " & GL.Context.Renderer);
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

      GL.Viewports.Set_Clipping (GL.Viewports.Lower_Left, GL.Viewports.Zero_To_One);
      Object.Vertex_Array.Create;
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
            Result.Context := EGL_Contexts.Create_Context (Display, Version, Flags);

            Post_Initialize (Result);
            Print_Debug (Display, Flags);
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
      Messages.Log (Debug, "EGL devices:");
      for Device of Devices loop
         declare
            Name : constant String := Device.Name;
         begin
            Messages.Log (Debug, "  - " & (if Name /= "" then Name else "unknown"));
         end;
      end loop;

      return Create_Context (Devices (Devices'First), Version, Flags);
   end Create_Context;

   overriding
   function Create_Context
     (Version : Context_Version;
      Flags   : Context_Flags := (others => False)) return Wayland_EGL_Context is
   begin
      return Result : constant Wayland_EGL_Context :=
        (Ada.Finalization.Limited_Controlled with others => <>)
      do
         --  EGL context for Wayland platform requires a pointer to a native
         --  Wayland display
         raise Program_Error;
      end return;
   end Create_Context;

   function Create_Context
     (Window  : Standard.EGL.Native_Display_Ptr;
      Version : Context_Version;
      Flags   : Context_Flags := (others => False)) return Wayland_EGL_Context
   is
      package EGL_Contexts renames Standard.EGL.Objects.Contexts;
      package EGL_Displays renames Standard.EGL.Objects.Displays;
   begin
      Standard.EGL.Debug.Set_Message_Callback (Print_Error'Access);

      declare
         Display : constant EGL_Displays.Display := EGL_Displays.Create_Display (Window);
      begin
         return Result : Wayland_EGL_Context do
            Result.Context := EGL_Contexts.Create_Context (Display, Version, Flags);

            Result.Context.Make_Current;

            Post_Initialize (Result);
            Print_Debug (Display, Flags);
         end return;
      end;
   end Create_Context;

   overriding
   procedure Finalize (Object : in out EGL_Context) is
   begin
      if Object.Flags.Debug then
         Messages.Log (Debug, "Shutting down EGL");
      end if;

      Object.Vertex_Array.Delete;
   end Finalize;

   overriding
   procedure Enable (Object : in out EGL_Context; Subject : Feature) is
   begin
      Contexts.Enable (Object.Features, Subject);
   end Enable;

   overriding
   function Enabled (Object : EGL_Context; Subject : Feature) return Boolean
     is (Contexts.Enabled (Object.Features, Subject));

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

   overriding
   procedure Make_Current (Object : Wayland_EGL_Context) is
   begin
      Object.Context.Make_Current;
   end Make_Current;

   overriding
   procedure Make_Not_Current (Object : Wayland_EGL_Context) is
   begin
      Object.Context.Make_Not_Current;
   end Make_Not_Current;

end Orka.Contexts.EGL;
