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

with GL.API;
with GL.Enums.Getter;

package body GL.Objects.Queries is

   overriding
   procedure Initialize_Id (Object : in out Query) is
      New_Id : UInt := 0;
   begin
      API.Gen_Queries (1, New_Id);
      Raise_Exception_On_OpenGL_Error;

      Object.Reference.GL_Id := New_Id;
      Object.Reference.Initialized := True;
   end Initialize_Id;

   overriding
   procedure Delete_Id (Object : in out Query) is
   begin
      API.Delete_Queries (1, (1 => Object.Reference.GL_Id));
      Raise_Exception_On_OpenGL_Error;

      Object.Reference.GL_Id := 0;
      Object.Reference.Initialized := False;
   end Delete_Id;

   function Begin_Query (Object : in out Query;
                         Target : in     Async_Query_Type;
                         Index  : in     Natural) return Active_Query'Class is
   begin
      return Active_Query'(Ada.Finalization.Limited_Controlled
        with Query_Object => Object, Target => Target, Index => Index);
   end Begin_Query;

   overriding
   procedure Initialize (Object : in out Active_Query) is
   begin
      API.Begin_Query_Indexed (Object.Target, UInt (Object.Index), Object.Query_Object.Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
   end Initialize;

   overriding
   procedure Finalize (Object : in out Active_Query) is
   begin
      API.End_Query_Indexed (Object.Target, UInt (Object.Index));
      Raise_Exception_On_OpenGL_Error;
   end Finalize;

   overriding
   procedure Initialize (Object : in out Conditional_Render) is
   begin
      API.Begin_Conditional_Render (Object.Query_Object.Reference.GL_Id, Object.Mode);
      Raise_Exception_On_OpenGL_Error;
   end Initialize;

   overriding
   procedure Finalize (Object : in out Conditional_Render) is
   begin
      API.End_Conditional_Render;
      Raise_Exception_On_OpenGL_Error;
   end Finalize;

   function Begin_Primitive_Query (Object : in out Query;
                                   Target : in     Primitive_Query_Type;
                                   Index  : in     Natural := 0)
     return Active_Query'Class is
   begin
      return Begin_Query (Object, Target, Index);
   end Begin_Primitive_Query;

   function Begin_Occlusion_Query (Object : in out Query;
                                   Target : in     Occlusion_Query_Type)
     return Active_Query'Class is
   begin
      return Begin_Query (Object, Target, 0);
   end Begin_Occlusion_Query;

   function Begin_Timer_Query (Object : in out Query;
                               Target : in     Time_Query_Type)
     return Active_Query'Class is
   begin
      return Begin_Query (Object, Target, 0);
   end Begin_Timer_Query;

   function Begin_Conditional_Render (Object : in out Query;
                                      Mode   : in     Query_Mode)
     return Conditional_Render'Class is
   begin
      return Conditional_Render'(Ada.Finalization.Limited_Controlled
        with Query_Object => Object, Mode => Mode);
   end Begin_Conditional_Render;

   function Result_Available (Object : in out Query) return Boolean is
      Available : UInt := 0;
   begin
      API.Get_Query_Object_UInt (Object.Reference.GL_Id, Result_Available, Available);
      Raise_Exception_On_OpenGL_Error;
      return Available = 1;
   end Result_Available;

   function Result_If_Available (Object : in out Query; Default_Value : Boolean)
     return Boolean
   is
      Result : UInt := (if Default_Value then 1 else 0);
   begin
      API.Get_Query_Object_UInt (Object.Reference.GL_Id, Result_No_Wait, Result);
      Raise_Exception_On_OpenGL_Error;
      return Result = 1;
   end Result_If_Available;

   function Result_If_Available (Object : in out Query; Default_Value : Natural)
     return Natural
   is
      Result : UInt := UInt (Default_Value);
   begin
      API.Get_Query_Object_UInt (Object.Reference.GL_Id, Result_No_Wait, Result);
      Raise_Exception_On_OpenGL_Error;
      return Natural (Result);
   end Result_If_Available;

   function Result (Object : in out Query) return Boolean is
      Result_Value : UInt := 0;
   begin
      API.Get_Query_Object_UInt (Object.Reference.GL_Id, Result, Result_Value);
      Raise_Exception_On_OpenGL_Error;
      return Result_Value = 1;
   end Result;

   --  TODO Handle Integer (Int) and Long (UInt64?)?
   function Result (Object : in out Query) return Natural is
      Result_Value : UInt := 0;
   begin
      API.Get_Query_Object_UInt (Object.Reference.GL_Id, Result, Result_Value);
      Raise_Exception_On_OpenGL_Error;
      return Natural (Result_Value);
   end Result;

   function Result_Bits (Target : in Query_Type) return Natural is
      Bits : Int := 0;
   begin
      API.Get_Query_Indexed_Param (Target, 0, Counter_Bits, Bits);
      Raise_Exception_On_OpenGL_Error;
      return Natural (Bits);
   end Result_Bits;

   procedure Record_Current_Time (Object : in out Query) is
   begin
      API.Query_Counter (Object.Reference.GL_Id, Timestamp);
      Raise_Exception_On_OpenGL_Error;
   end Record_Current_Time;

   function Get_Current_Time return Long is
      Result : aliased Types.Long;
   begin
      API.Get_Long (Enums.Getter.Timestamp, Result'Access);
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Get_Current_Time;

end GL.Objects.Queries;
