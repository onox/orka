--------------------------------------------------------------------------------
-- Copyright (c) 2015 onox <denkpadje@gmail.com>
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

with GL.API;
with GL.Enums;

with Interfaces.C.Strings;

package body GL.Objects.Pipelines is

   procedure Use_Program_Stages (Object : Pipeline; Shader : Shaders.Shader_Type;
                                 Program : Programs.Program) is
   begin
      API.Use_Program_Stages (Object.Reference.GL_Id, Shader, Program.Raw_Id);
      Raise_Exception_On_OpenGL_Error;
   end Use_Program_Stages;

   procedure Set_Active_Program (Object : Pipeline; Program : Programs.Program) is
   begin
      API.Active_Shader_Program (Object.Reference.GL_Id, Program.Raw_Id);
      Raise_Exception_On_OpenGL_Error;
   end Set_Active_Program;

   procedure Bind (Object : Pipeline) is
   begin
      API.Bind_Program_Pipeline (Object.Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
   end Bind;

   function Validate (Object : Pipeline) return Boolean is
      Status_Value : Int := 0;
   begin
      API.Validate_Program_Pipeline (Object.Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
      API.Get_Program_Pipeline_Param (Object.Reference.GL_Id, Enums.Validate_Status,
                                      Status_Value);
      Raise_Exception_On_OpenGL_Error;
      return Status_Value /= 0;
   end Validate;

   function Info_Log (Object : Pipeline) return String is
      Log_Length : Int := 0;
   begin
      API.Get_Program_Pipeline_Param (Object.Reference.GL_Id, Enums.Info_Log_Length,
                                      Log_Length);
      Raise_Exception_On_OpenGL_Error;
      --  Returned length includes null termination character
      Log_Length := Log_Length - 1;
      declare
         Info_Log : String (1 .. Integer (Log_Length));
         --  Do not care that string does not get initialized
         pragma Warnings (Off, Info_Log);
         C_Info_Log : C.Strings.chars_ptr := C.Strings.New_String (Info_Log);
         Actual_Length : Size := 0;
      begin
         API.Get_Program_Pipeline_Info_Log (Object.Reference.GL_Id,
                                            Log_Length + 1,
                                            Actual_Length, C_Info_Log);
         Raise_Exception_On_OpenGL_Error;
         Info_Log := C.Strings.Value (C_Info_Log, C.size_t (Actual_Length));
         C.Strings.Free (C_Info_Log);
         return Info_Log;
      end;
   end Info_Log;

   overriding
   procedure Initialize_Id (Object : in out Pipeline) is
      New_Id : UInt := 0;
   begin
      API.Create_Program_Pipelines (1, New_Id);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := New_Id;
      Object.Reference.Initialized := True;
   end Initialize_Id;

   overriding
   procedure Delete_Id (Object : in out Pipeline) is
   begin
      API.Delete_Program_Pipelines (1, (1 => Object.Reference.GL_Id));
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := 0;
      Object.Reference.Initialized := False;
   end Delete_Id;

end GL.Objects.Pipelines;
