--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GL.Context;
with GL.Types;

with GL_Test.Display_Backend;

procedure GL_Test.Context is

   use Ada.Text_IO;

   procedure Display_Context_Information is
      use Ada.Strings.Unbounded;
   begin
      Put_Line ("Major version: " & GL.Types.Int'Image (GL.Context.Major_Version));
      Put_Line ("Minor version: " & GL.Types.Int'Image (GL.Context.Minor_Version));

      Put_Line ("Version string: " & GL.Context.Version_String);
      Put_Line ("Vendor: "   & GL.Context.Vendor);
      Put_Line ("Renderer: " & GL.Context.Renderer);
      Put_Line ("Extensions: ");

      for US_Name of GL.Context.Extensions loop
         declare
            Name : constant String := To_String (US_Name);
         begin
            Put_Line ("  " & Name);
            pragma Assert (GL.Context.Has_Extension (Name));
         end;
      end loop;

      Put_Line ("Primary shading language version: " &
                GL.Context.Primary_Shading_Language_Version);

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

   Display_Backend.Init (Major => 3, Minor => 2);
   Display_Backend.Open_Window (Width => 500, Height => 500, Visible => False);
   Display_Context_Information;
   Display_Backend.Shutdown;
end GL_Test.Context;
