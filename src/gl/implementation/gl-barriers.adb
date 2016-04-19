--------------------------------------------------------------------------------
-- Copyright (c) 2016 onox <denkpadje@gmail.com>
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

with Ada.Unchecked_Conversion;

with GL.API;

package body GL.Barriers is

   procedure Texture_Barrier is
   begin
      API.Texture_Barrier;
      Raise_Exception_On_OpenGL_Error;
   end Texture_Barrier;

   procedure Memory_Barrier (Bits : Memory_Barrier_Bits) is
      use type Low_Level.Bitfield;

      function Convert is new Ada.Unchecked_Conversion
        (Source => Memory_Barrier_Bits, Target => Low_Level.Bitfield);
      Raw_Bits : constant Low_Level.Bitfield :=
        Convert (Bits) and 2#1111111111101111#;
   begin
      API.Memory_Barrier (Raw_Bits);
      Raise_Exception_On_OpenGL_Error;
   end Memory_Barrier;

   procedure Memory_Barrier_By_Region (Bits : Memory_Barrier_Bits) is
      use type Low_Level.Bitfield;

      function Convert is new Ada.Unchecked_Conversion
        (Source => Memory_Barrier_Bits, Target => Low_Level.Bitfield);
      Raw_Bits : constant Low_Level.Bitfield :=
        Convert (Bits) and 2#0011010000101100#;
   begin
      API.Memory_Barrier_By_Region (Raw_Bits);
      Raise_Exception_On_OpenGL_Error;
   end Memory_Barrier_By_Region;

end GL.Barriers;
