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

with Glfw.API;

with Interfaces.C.Strings;

package body Glfw is

   procedure Init is
   begin
      if API.Init = 0 then
         raise Initialization_Exception;
      end if;
   end Init;

   procedure Shutdown is
   begin
      API.Glfw_Terminate;
   end Shutdown;

   procedure Version (Major, Minor, Rev : out Natural) is
      Raw_Major, Raw_Minor, Raw_Rev : C.int;
   begin
      API.Get_Version (Raw_Major, Raw_Minor, Raw_Rev);
      Major := Natural (Raw_Major);
      Minor := Natural (Raw_Minor);
      Rev   := Natural (Raw_Rev);
   end Version;

   function Version_String return String is
   begin
      return Interfaces.C.Strings.Value (API.Get_Version_String);
   end Version_String;

   function Time return Seconds is
   begin
      return API.Get_Time;
   end Time;

   procedure Set_Time (Value : Seconds) is
   begin
      API.Set_Time (Value);
   end Set_Time;

   function Extension_Supported (Name : String) return Boolean is
   begin
      return Boolean (API.Extension_Supported (Interfaces.C.To_C (Name)));
   end Extension_Supported;

end Glfw;
