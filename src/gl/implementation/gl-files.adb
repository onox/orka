--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <contact@flyx.org>
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

with Ada.Streams.Stream_IO;

with Interfaces.C.Strings;

with GL.API;
with GL.Low_Level;
with GL.Types;

package body GL.Files is
   use GL.Types;

   procedure Read_Whole_File (File_Name : String;
                              File_Size : out Int;
                              Contents  : out C.Strings.chars_ptr) is
      File   : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_IO.Stream_Access;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, File_Name);
      File_Size := Int (Ada.Streams.Stream_IO.Size (File));

      declare
         subtype File_String is C.char_array (1 .. C.size_t (File_Size));

         C_File_Size : constant C.size_t := C.size_t (File_Size);
         use type Interfaces.C.size_t;

         --  Increase size by 1 for Nul character at the end
         Raw_Contents : C.char_array (1 .. C_File_Size + 1);
      begin
         --  Read the file and write to Raw_Contents (excluding the Nul
         --  character at the end of the array)
         Stream := Ada.Streams.Stream_IO.Stream (File);
         File_String'Read (Stream, Raw_Contents (1 .. C_File_Size));

         Raw_Contents (C_File_Size + 1) := C.nul;
         Contents := C.Strings.New_Char_Array (Raw_Contents);

         Ada.Streams.Stream_IO.Close (File);
      exception
         when others =>
            Ada.Streams.Stream_IO.Close (File);
            raise;
      end;
   end Read_Whole_File;

   procedure Load_Shader_Source_From_File (Object : Objects.Shaders.Shader;
                                           File_Name : String) is
      Sources : Low_Level.CharPtr_Array (1 .. 1);
      Sizes   : Low_Level.Int_Array (1 .. 1);
   begin
      Read_Whole_File (File_Name, Sizes (1), Sources (1));
      API.Shader_Source (Object.Raw_Id, 1, Sources, Sizes);
      C.Strings.Free (Sources (1));
      Raise_Exception_On_OpenGL_Error;
   end Load_Shader_Source_From_File;

end GL.Files;
