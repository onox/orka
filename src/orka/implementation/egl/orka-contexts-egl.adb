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

   function Create_Context
     (Device  : Standard.EGL.Objects.Devices.Device;
      Version : Context_Version;
      Flags   : Context_Flags := (others => False)) return EGL_Context
   is
      package EGL_Contexts renames Standard.EGL.Objects.Contexts;
      package EGL_Displays renames Standard.EGL.Objects.Displays;
   begin
      Standard.EGL.Debug.Set_Message_Callback (Print_Error'Access);

      return Result : EGL_Context := (Ada.Finalization.Limited_Controlled with
        Platform => Standard.EGL.Objects.Displays.Device,
        Version  => Version,
        Flags    => Flags,
        others   => <>)
      do
         declare
            Display : constant EGL_Displays.Display :=
              EGL_Displays.Create_Display (EGL_Displays.Device, Device);
         begin
            Result.Context := EGL_Contexts.Create_Context (Display, Version, Flags);

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
         end;

         GL.Viewports.Set_Clipping (GL.Viewports.Lower_Left, GL.Viewports.Zero_To_One);
         Result.Vertex_Array.Create;
      end return;
   end Create_Context;

   overriding
   function Create_Context
     (Version : Context_Version;
      Flags   : Context_Flags := (others => False)) return EGL_Context
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

end Orka.Contexts.EGL;
