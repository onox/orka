--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2013 Felix Krause <contact@flyx.org>
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GL.Context;

with Orka.Contexts;
with Orka.Debug;
with Orka.Loggers.Terminal;
with Orka.Logging;
with Orka.Windows.GLFW;

procedure Orka_Info is
   procedure Display_Context_Information is
      package SU renames Ada.Strings.Unbounded;
      use Ada.Text_IO;
   begin
      Put_Line ("Major version: " & Natural'Image (GL.Context.Major_Version));
      Put_Line ("Minor version: " & Natural'Image (GL.Context.Minor_Version));

      Put_Line ("  Vendor: " & GL.Context.Vendor);
      Put_Line ("Renderer: " & GL.Context.Renderer);

      Put_Line ("OpenGL version: " & GL.Context.Version_String);
      Put_Line ("  GLSL version: " & GL.Context.Primary_Shading_Language_Version);

      Put_Line ("Extensions: ");

      for Extension of GL.Context.Extensions loop
         Put_Line ("  " & SU.To_String (Extension));
      end loop;

      Put_Line ("Supported shading language versions:");

      begin
         for Version of GL.Context.Supported_Shading_Language_Versions loop
            Put_Line ("  " & SU.To_String (Version));
         end loop;
      exception
         when others =>
            Put_Line ("OpenGL version too old for querying supported versions");
      end;
   end Display_Context_Information;
begin
   Orka.Logging.Set_Logger (Orka.Loggers.Terminal.Create_Logger (Level => Orka.Loggers.Info));

   declare
      Context : constant Orka.Contexts.Context'Class := Orka.Windows.GLFW.Create_Context
        (Version => (3, 2), Flags  => (Debug => True, others => False));

      Window : constant Orka.Windows.Window'Class
        := Orka.Windows.GLFW.Create_Window (Context, Width => 1, Height => 1, Visible => False);
      pragma Assert (Window.Width = 1 and Window.Height = 1);
   begin
      Orka.Debug.Set_Log_Messages (Enable => True);
      Display_Context_Information;
   end;
end Orka_Info;
