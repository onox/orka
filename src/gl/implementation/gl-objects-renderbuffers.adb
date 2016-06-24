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

with Ada.Containers.Indefinite_Holders;
with Ada.Unchecked_Conversion;

with GL.API;
with GL.Enums.Getter;

package body GL.Objects.Renderbuffers is

   package Renderbuffer_Holder is new Ada.Containers.Indefinite_Holders
     (Element_Type => Renderbuffer'Class);

   type Renderbuffer_Target_Array is array (Low_Level.Enums.Renderbuffer_Kind)
     of Renderbuffer_Holder.Holder;
   Current_Renderbuffers : Renderbuffer_Target_Array;

   procedure Allocate (Object : Renderbuffer;
                       Format : Pixels.Internal_Format;
                       Width, Height : Size;
                       Samples : Size := 0) is
   begin
      if Samples = 0 then
         API.Named_Renderbuffer_Storage
           (Object.Reference.GL_Id, Format, Width, Height);
      else
         API.Named_Renderbuffer_Storage_Multisample
           (Object.Reference.GL_Id, Samples, Format, Width, Height);
      end if;
      Raise_Exception_On_OpenGL_Error;
   end Allocate;

   function Width (Object : Renderbuffer) return Size is
      Value : Int := 0;
   begin
      API.Get_Named_Renderbuffer_Parameter_Int
        (Object.Reference.GL_Id, Enums.Getter.Width, Value);
      return Value;
   end Width;

   function Height (Object : Renderbuffer) return Size is
      Value : Int := 0;
   begin
      API.Get_Named_Renderbuffer_Parameter_Int
        (Object.Reference.GL_Id, Enums.Getter.Height, Value);
      return Value;
   end Height;

   function Internal_Format (Object : Renderbuffer)
                             return Pixels.Internal_Format is
      Value : Pixels.Internal_Format := Pixels.Internal_Format'First;
   begin
      API.Get_Named_Renderbuffer_Parameter_Internal_Format
        (Object.Reference.GL_Id, Enums.Getter.Internal_Format, Value);
      return Value;
   end Internal_Format;

   function Red_Size (Object : Renderbuffer) return Size is
      Value : Int := 0;
   begin
      API.Get_Named_Renderbuffer_Parameter_Int
        (Object.Reference.GL_Id, Enums.Getter.Green_Size, Value);
      return Value;
   end Red_Size;

   function Green_Size (Object : Renderbuffer) return Size is
      Value : Int := 0;
   begin
      API.Get_Named_Renderbuffer_Parameter_Int
        (Object.Reference.GL_Id, Enums.Getter.Green_Size, Value);
      return Value;
   end Green_Size;

   function Blue_Size (Object : Renderbuffer) return Size is
      Value : Int := 0;
   begin
      API.Get_Named_Renderbuffer_Parameter_Int
        (Object.Reference.GL_Id, Enums.Getter.Blue_Size, Value);
      return Value;
   end Blue_Size;

   function Alpha_Size (Object : Renderbuffer) return Size is
      Value : Int := 0;
   begin
      API.Get_Named_Renderbuffer_Parameter_Int
        (Object.Reference.GL_Id, Enums.Getter.Alpha_Size, Value);
      return Value;
   end Alpha_Size;

   function Depth_Size (Object : Renderbuffer) return Size is
      Value : Int := 0;
   begin
      API.Get_Named_Renderbuffer_Parameter_Int
        (Object.Reference.GL_Id, Enums.Getter.Depth_Size, Value);
      return Value;
   end Depth_Size;

   function Stencil_Size (Object : Renderbuffer) return Size is
      Value : Int := 0;
   begin
      API.Get_Named_Renderbuffer_Parameter_Int
        (Object.Reference.GL_Id, Enums.Getter.Stencil_Size, Value);
      return Value;
   end Stencil_Size;

   function Raw_Kind (Object : Renderbuffer_Target)
                      return Low_Level.Enums.Renderbuffer_Kind is
   begin
      return Object.Kind;
   end Raw_Kind;

   procedure Bind (Target : Renderbuffer_Target; Object : Renderbuffer'Class) is
      Holder : Renderbuffer_Holder.Holder := Current_Renderbuffers (Target.Kind);
   begin
      if Holder.Is_Empty or else Object /= Holder.Element then
         API.Bind_Renderbuffer (Target.Kind, Object.Reference.GL_Id);
         Raise_Exception_On_OpenGL_Error;
         Holder.Replace_Element (Object);
      end if;
   end Bind;

   function Current (Target : Renderbuffer_Target) return Renderbuffer'Class is
      Holder : constant Renderbuffer_Holder.Holder := Current_Renderbuffers (Target.Kind);
   begin
      if Holder.Is_Empty then
         raise No_Object_Bound_Exception with GL.Low_Level.Enums.Renderbuffer_Kind'Image (Target.Kind);
      else
         return Holder.Element;
      end if;
   end Current;

   overriding
   procedure Initialize_Id (Object : in out Renderbuffer) is
      New_Id : UInt := 0;
   begin
      API.Create_Renderbuffers (1, New_Id);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := New_Id;
      Object.Reference.Initialized := True;
   end Initialize_Id;

   overriding
   procedure Delete_Id (Object : in out Renderbuffer) is
      Arr : constant Low_Level.UInt_Array := (1 => Object.Reference.GL_Id);
   begin
      API.Delete_Renderbuffers (1, Arr);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := 0;
      Object.Reference.Initialized := False;
   end Delete_Id;

end GL.Objects.Renderbuffers;
