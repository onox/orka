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

with GL.Context;

with Orka.Contexts.AWT;
with Orka.Debug;
with Orka.Loggers.Terminal;
with Orka.Logging;
with Orka.OS;

procedure Orka_Info is
   procedure Display_Context_Information (Context : Orka.Contexts.Context'Class) is
      package SU renames Ada.Strings.Unbounded;
      use Orka.OS;
   begin
      Put_Line ("Version:  " & Orka.Contexts.Image (Context.Version));
      Put_Line ("  OpenGL: " & GL.Context.Version_String);
      Put_Line ("  GLSL:   " & GL.Context.Primary_Shading_Language_Version);

      Put_Line ("Vendor:   " & GL.Context.Vendor);
      Put_Line ("Renderer: " & GL.Context.Renderer);

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
      Context : constant Orka.Contexts.Context'Class := Orka.Contexts.AWT.Create_Context
        (Version => (3, 2), Flags  => (Debug => True, others => False));
   begin
      Orka.Debug.Set_Log_Messages (Enable => True);
      Display_Context_Information (Context);
   end;
end Orka_Info;
