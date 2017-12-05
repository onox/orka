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
with GL.Types;

with GL_Test.Display_Backend;

procedure GL_Test.Context is
   Initialized : constant Boolean := Display_Backend.Init
     (Major => 3, Minor => 2, Width => 500, Height => 500, Visible => False);
   pragma Unreferenced (Initialized);

   use Ada.Text_IO;

   procedure Display_Context_Information is
      use Ada.Strings.Unbounded;
   begin
      Put_Line ("Major version: " & GL.Types.Int'Image (GL.Context.Major_Version));
      Put_Line ("Minor version: " & GL.Types.Int'Image (GL.Context.Minor_Version));

      Put_Line ("Vendor: "   & GL.Context.Vendor);
      Put_Line ("Renderer: " & GL.Context.Renderer);

      Put_Line ("OpenGL version: " & GL.Context.Version_String);
      Put_Line ("  GLSL version: " &
                GL.Context.Primary_Shading_Language_Version);

      Put_Line ("Extensions: ");

      for US_Name of GL.Context.Extensions loop
         declare
            Name : constant String := To_String (US_Name);
         begin
            Put_Line ("  " & Name);
            pragma Assert (GL.Context.Has_Extension (Name));
         end;
      end loop;

      begin
         declare
            List : constant GL.Context.String_List :=
              GL.Context.Supported_Shading_Language_Versions;
         begin
            Put_Line ("Supported shading language versions:");
            for US_Version of List loop
               declare
                  Version : constant String := To_String (US_Version);
               begin
                  Put_Line ("  " & Version);
                  pragma Assert (GL.Context.Supports_Shading_Language_Version (Version));
               end;
            end loop;
         end;
      exception
         when others =>
            Put_Line ("OpenGL version too old for querying supported versions");
      end;
   end Display_Context_Information;

begin
   Put_Line ("Getting information with requested OpenGL version 3.2:");
   Display_Context_Information;

   Display_Backend.Shutdown;
end GL_Test.Context;
