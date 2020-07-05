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

with Ada.Text_IO;

with EGL.Objects.Displays;
with EGL.Objects.Devices;
with EGL.Errors;

with Orka.Contexts.EGL;
with Orka.Debug;
with Orka.Loggers.Terminal;
with Orka.Logging;

procedure Orka_EGL_Info is
   use Ada.Text_IO;

   Devices : constant EGL.Objects.Devices.Device_List := EGL.Objects.Devices.Devices;
begin
   Orka.Logging.Set_Logger (Orka.Loggers.Terminal.Create_Logger (Level => Orka.Loggers.Debug));

   Put_Line ("Client extensions:");
   for Extension of EGL.Objects.Displays.Client_Extensions loop
      Put_Line ("  " & EGL.SU.To_String (Extension));
   end loop;
   Put_Line ("");

   Put_Line ("Platforms:");
   for Platform in EGL.Objects.Displays.Platform_Kind'Range loop
      Put_Line ("");
      begin
         declare
            Display : constant EGL.Objects.Displays.Display :=
               EGL.Objects.Displays.Create_Display (Platform, Devices (Devices'First));
         begin
            Put_Line (Display.Platform'Image & ":");
            Put_Line ("  vendor:     " & Display.Vendor);
            Put_Line ("  version:    " & Display.Version);
            Put_Line ("  extensions:");
            for Extension of Display.Extensions loop
               Put_Line ("    " & EGL.SU.To_String (Extension));
            end loop;
         end;
      exception
         when EGL.Errors.Not_Initialized_Error =>
            Put_Line (Platform'Image & ": not supported");
      end;
   end loop;
   Put_Line ("");

   Put_Line ("Devices:");
   for Device of Devices loop
      Put_Line ("");
      declare
         Name : constant String := Device.Name;
      begin
         Put_Line (if Name /= "" then Device.Name else "unknown");
      end;
      Put_Line ("  extensions:");
      for Extension of Device.Extensions loop
         Put_Line ("    " & EGL.SU.To_String (Extension));
      end loop;
   end loop;
end Orka_EGL_Info;
