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

with System;

with Ada.Containers.Indefinite_Holders;
with Ada.Unchecked_Conversion;

with GL.API;
with GL.Low_Level;

package body GL.Objects.Vertex_Arrays is

   package Vertex_Array_Holder is new Ada.Containers.Indefinite_Holders
      (Element_Type => Vertex_Array_Object);

   Current_Vertex_Array : Vertex_Array_Holder.Holder;

   procedure Bind (Object : Vertex_Array_Object) is
   begin
      if Object = Null_Array_Object then
         API.Bind_Vertex_Array (0);
         Raise_Exception_On_OpenGL_Error;
         Current_Vertex_Array.Replace_Element (Null_Array_Object);
      elsif Current_Vertex_Array.Is_Empty or else Object /= Current_Vertex_Array.Element then
         API.Bind_Vertex_Array (Object.Reference.GL_Id);
         Raise_Exception_On_OpenGL_Error;
         Current_Vertex_Array.Replace_Element (Object);
      end if;
   end Bind;

   procedure Enable_Attribute (Object : Vertex_Array_Object; Index : Attributes.Attribute) is
   begin
      API.Enable_Vertex_Array_Attrib (Object.Reference.GL_Id, Index);
      Raise_Exception_On_OpenGL_Error;
   end Enable_Attribute;

   procedure Disable_Attribute (Object : Vertex_Array_Object; Index : Attributes.Attribute) is
   begin
      API.Disable_Vertex_Array_Attrib (Object.Reference.GL_Id, Index);
      Raise_Exception_On_OpenGL_Error;
   end Disable_Attribute;

   procedure Set_Attribute_Format (Object : Vertex_Array_Object;
                                   Index  : Attributes.Attribute;
                                   Count  : Component_Count;
                                   Kind   : Numeric_Type;
                                   Offset : UInt) is
   begin
      case Kind is
         when Single_Type =>
            API.Vertex_Array_Attrib_Format
              (Object.Reference.GL_Id, Index, Count, Kind, Low_Level.False,
               Offset * Single'Size / System.Storage_Unit);
         when Double_Type =>
            API.Vertex_Array_AttribL_Format
              (Object.Reference.GL_Id, Index, Count, Kind,
               Offset * Double'Size / System.Storage_Unit);
         when UInt_Type =>
            API.Vertex_Array_AttribI_Format
              (Object.Reference.GL_Id, Index, Count, Kind,
               Offset * UInt'Size / System.Storage_Unit);
         when UByte_Type =>
            API.Vertex_Array_AttribI_Format
              (Object.Reference.GL_Id, Index, Count, Kind,
               Offset * UByte'Size / System.Storage_Unit);
         when UShort_Type =>
            API.Vertex_Array_AttribI_Format
              (Object.Reference.GL_Id, Index, Count, Kind,
               Offset * UShort'Size / System.Storage_Unit);
         when Int_Type =>
            API.Vertex_Array_AttribI_Format
              (Object.Reference.GL_Id, Index, Count, Kind,
               Offset * Int'Size / System.Storage_Unit);
         when Byte_Type =>
            API.Vertex_Array_AttribI_Format
              (Object.Reference.GL_Id, Index, Count, Kind,
               Offset * Byte'Size / System.Storage_Unit);
         when Short_Type =>
            API.Vertex_Array_AttribI_Format
              (Object.Reference.GL_Id, Index, Count, Kind,
               Offset * Short'Size / System.Storage_Unit);
      end case;
      Raise_Exception_On_OpenGL_Error;
   end Set_Attribute_Format;

   procedure Set_Attribute_Binding (Object : Vertex_Array_Object;
                                    Index  : Attributes.Attribute;
                                    Binding_Index : Binding) is
   begin
      API.Vertex_Array_Attrib_Binding (Object.Reference.GL_Id, Index, Binding_Index);
      Raise_Exception_On_OpenGL_Error;
   end Set_Attribute_Binding;

   procedure Bind_Vertex_Buffer (Object : Vertex_Array_Object;
                                 Binding_Index : Binding;
                                 Buffer : Objects.Buffers.Buffer;
                                 Kind   : Numeric_Type;
                                 Offset, Stride : Size) is
      Bytes : Size;
   begin
      case Kind is
         when Single_Type =>
            Bytes := Single'Size / System.Storage_Unit;
         when Double_Type =>
            Bytes := Double'Size / System.Storage_Unit;
         when UInt_Type =>
            Bytes := UInt'Size / System.Storage_Unit;
         when UByte_Type =>
            Bytes := UByte'Size / System.Storage_Unit;
         when UShort_Type =>
            Bytes := UShort'Size / System.Storage_Unit;
         when Int_Type =>
            Bytes := Int'Size / System.Storage_Unit;
         when Byte_Type =>
            Bytes := Byte'Size / System.Storage_Unit;
         when Short_Type =>
            Bytes := Short'Size / System.Storage_Unit;
      end case;
      API.Vertex_Array_Vertex_Buffer (Object.Reference.GL_Id, Binding_Index,
        Buffer.Raw_Id,
        Offset * Bytes,
        Stride * Bytes);
      Raise_Exception_On_OpenGL_Error;
   end Bind_Vertex_Buffer;

   procedure Set_Attribute_Binding_Divisor (Object : Vertex_Array_Object;
                                            Binding_Index : Binding;
                                            Divisor : UInt) is
   begin
      API.Vertex_Array_Binding_Divisor (Object.Reference.GL_Id, Binding_Index, Divisor);
      Raise_Exception_On_OpenGL_Error;
   end Set_Attribute_Binding_Divisor;

   procedure Draw_Arrays (Mode : Connection_Mode; Offset, Count : Size) is
   begin
      API.Draw_Arrays (Mode, Offset, Count);
      Raise_Exception_On_OpenGL_Error;
   end Draw_Arrays;

   procedure Draw_Arrays (Mode : Connection_Mode; Offset, Count, Instances : Size) is
   begin
      API.Draw_Arrays_Instanced (Mode, Offset, Count, Instances);
      Raise_Exception_On_OpenGL_Error;
   end Draw_Arrays;

   procedure Draw_Multiple_Arrays (Mode : Connection_Mode;
                                   Offsets, Counts : Size_Array) is
   begin
      API.Multi_Draw_Arrays (Mode, Offsets, Counts, Counts'Length);
      Raise_Exception_On_OpenGL_Error;
   end Draw_Multiple_Arrays;

   overriding
   procedure Initialize_Id (Object : in out Vertex_Array_Object) is
      New_Id : UInt := 0;
   begin
      API.Gen_Vertex_Arrays (1, New_Id);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := New_Id;
      Object.Reference.Initialized := True;
   end Initialize_Id;

   function Current_Array_Object return Vertex_Array_Object is
   begin
      if Current_Vertex_Array.Is_Empty then
         return Null_Array_Object;
      else
         return Current_Vertex_Array.Element;
      end if;
   end Current_Array_Object;

   overriding
   procedure Delete_Id (Object : in out Vertex_Array_Object) is
      Arr : constant Low_Level.UInt_Array := (1 => Object.Reference.GL_Id);
   begin
      API.Delete_Vertex_Arrays (1, Arr);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := 0;
      Object.Reference.Initialized := False;
   end Delete_Id;
end GL.Objects.Vertex_Arrays;
