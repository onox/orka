--  Copyright (c) 2012 Felix Krause <contact@flyx.org>
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

with Ada.Containers.Indefinite_Holders;
with Ada.Unchecked_Conversion;

with System;

with GL.API;
with GL.Enums;

package body GL.Objects.Buffers is
   use type Low_Level.Enums.Buffer_Kind;

   package Buffer_Holder is new Ada.Containers.Indefinite_Holders
     (Element_Type => Buffer'Class);

   type Buffer_Target_Array is array (Low_Level.Enums.Buffer_Kind) of Buffer_Holder.Holder;
   Current_Buffers : Buffer_Target_Array;

   procedure Bind (Target : Buffer_Target; Object : Buffer'Class) is
      Holder : Buffer_Holder.Holder := Current_Buffers (Target.Kind);
   begin
      if Holder.Is_Empty or else Object /= Holder.Element then
         API.Bind_Buffer (Target.Kind, Object.Reference.GL_Id);
         Raise_Exception_On_OpenGL_Error;
         Holder.Replace_Element (Object);
      end if;
   end Bind;

   procedure Bind_Base (Target : Buffer_Target; Object : Buffer'Class; Index : Natural) is
      Holder : Buffer_Holder.Holder := Current_Buffers (Target.Kind);
   begin
      if Holder.Is_Empty or else Object /= Holder.Element then
         API.Bind_Buffer_Base (Target.Kind, UInt (Index), Object.Reference.GL_Id);
         Raise_Exception_On_OpenGL_Error;
         Holder.Replace_Element (Object);
      end if;
   end Bind_Base;

   function Current_Object (Target : Buffer_Target) return Buffer'Class is
      Holder : constant Buffer_Holder.Holder := Current_Buffers (Target.Kind);
   begin
      if Holder.Is_Empty then
         raise No_Object_Bound_Exception with GL.Low_Level.Enums.Buffer_Kind'Image (Target.Kind);
      else
         return Holder.Element;
      end if;
   end Current_Object;

   procedure Allocate (Object : Buffer; Number_Of_Elements : Long;
                       Kind : Numeric_Type; Storage_Flags : Storage_Bits) is
      use type Low_Level.Bitfield;

      function Convert is new Ada.Unchecked_Conversion
        (Source => Storage_Bits, Target => Low_Level.Bitfield);
      Raw_Bits : constant Low_Level.Bitfield :=
        Convert (Storage_Flags) and 2#0000001111000011#;

      Bytes : Long;
   begin
      case Kind is
         when Half_Type =>
            Bytes := Number_Of_Elements * Half'Size / System.Storage_Unit;
         when Single_Type =>
            Bytes := Number_Of_Elements * Single'Size / System.Storage_Unit;
         when Double_Type =>
            Bytes := Number_Of_Elements * Double'Size / System.Storage_Unit;
         when UInt_Type =>
            Bytes := Number_Of_Elements * UInt'Size / System.Storage_Unit;
         when UByte_Type =>
            Bytes := Number_Of_Elements * UByte'Size / System.Storage_Unit;
         when UShort_Type =>
            Bytes := Number_Of_Elements * UShort'Size / System.Storage_Unit;
         when Int_Type =>
            Bytes := Number_Of_Elements * Int'Size / System.Storage_Unit;
         when Byte_Type =>
            Bytes := Number_Of_Elements * Byte'Size / System.Storage_Unit;
         when Short_Type =>
            Bytes := Number_Of_Elements * Short'Size / System.Storage_Unit;
      end case;
      API.Named_Buffer_Storage (Object.Reference.GL_Id, Low_Level.SizeIPtr (Bytes),
                                System.Null_Address, Raw_Bits);
      Raise_Exception_On_OpenGL_Error;
   end Allocate;

   function Access_Type (Object : Buffer) return Access_Kind is
      Ret : Access_Kind := Access_Kind'First;
   begin
      API.Get_Named_Buffer_Parameter_Access_Kind (Object.Reference.GL_Id, Enums.Buffer_Access,
                                            Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Access_Type;

   function Immutable (Object : Buffer) return Boolean is
      Ret : Low_Level.Bool := Low_Level.Bool (False);
   begin
      API.Get_Named_Buffer_Parameter_Bool (Object.Reference.GL_Id, Enums.Buffer_Immutable_Storage, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Boolean (Ret);
   end Immutable;

   function Mapped (Object : Buffer) return Boolean is
      Ret : Low_Level.Bool := Low_Level.Bool (False);
   begin
      API.Get_Named_Buffer_Parameter_Bool (Object.Reference.GL_Id, Enums.Buffer_Mapped, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Boolean (Ret);
   end Mapped;

   function Size (Object : Buffer) return Types.Long_Size is
      Ret : Types.Long_Size := 0;
   begin
      API.Get_Named_Buffer_Parameter_Size (Object.Reference.GL_Id, Enums.Buffer_Size, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Size;

   function Storage_Flags (Object : Buffer) return Storage_Bits is
      use type Low_Level.Bitfield;

      function Convert is new Ada.Unchecked_Conversion
        (Source => Low_Level.Bitfield, Target => Storage_Bits);

      Raw_Bits : Low_Level.Bitfield := 2#0000000000000000#;
   begin
      API.Get_Named_Buffer_Parameter_Bitfield (Object.Reference.GL_Id,
                                               Enums.Buffer_Storage_Flags,
                                               Raw_Bits);
      Raise_Exception_On_OpenGL_Error;
      return Convert (Raw_Bits and 2#0000001111000011#);
   end Storage_Flags;

   overriding procedure Initialize_Id (Object : in out Buffer) is
      New_Id : UInt := 0;
   begin
      API.Create_Buffers (1, New_Id);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := New_Id;
      Object.Reference.Initialized := True;
   end Initialize_Id;

   overriding procedure Delete_Id (Object : in out Buffer) is
      Arr : constant Low_Level.UInt_Array := (1 => Object.Reference.GL_Id);
   begin
      API.Delete_Buffers (1, Arr);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := 0;
      Object.Reference.Initialized := False;
   end Delete_Id;

   procedure Unmap (Object : in out Buffer) is
   begin
      API.Unmap_Named_Buffer (Object.Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
   end Unmap;

   procedure Invalidate_Data (Object : in out Buffer) is
   begin
      API.Invalidate_Buffer_Data (Object.Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
   end Invalidate_Data;

   package body Buffer_Pointers is

      procedure Bind_Range (Target : Buffer_Target; Object : Buffer'Class; Index : Natural;
                            Offset, Length : Types.Size) is
         Holder : Buffer_Holder.Holder := Current_Buffers (Target.Kind);

         Offset_In_Bytes : constant Int := Offset * Pointers.Element'Size / System.Storage_Unit;
         Number_Of_Bytes : constant Int := Length * Pointers.Element'Size / System.Storage_Unit;
      begin
         if Holder.Is_Empty or else Object /= Holder.Element then
            API.Bind_Buffer_Range (Target.Kind, UInt (Index), Object.Reference.GL_Id,
                                   Low_Level.IntPtr (Offset_In_Bytes),
                                   Low_Level.SizeIPtr (Number_Of_Bytes));
            Raise_Exception_On_OpenGL_Error;
            Holder.Replace_Element (Object);
         end if;
      end Bind_Range;

      procedure Load_To_Immutable_Buffer (Object : Buffer;
                                          Data   : Pointers.Element_Array;
                                          Storage_Flags : Storage_Bits) is
         use type Low_Level.Bitfield;

         function Convert is new Ada.Unchecked_Conversion
           (Source => Storage_Bits, Target => Low_Level.Bitfield);
         Raw_Bits : constant Low_Level.Bitfield :=
           Convert (Storage_Flags) and 2#0000001111000011#;

         Number_Of_Bytes : constant Long := Data'Length * Pointers.Element'Size / System.Storage_Unit;
      begin
         API.Named_Buffer_Storage (Object.Reference.GL_Id, Low_Level.SizeIPtr (Number_Of_Bytes),
                                   Data (Data'First)'Address, Raw_Bits);
         Raise_Exception_On_OpenGL_Error;
      end Load_To_Immutable_Buffer;

      procedure Map (Object : in out Buffer; Access_Type : Access_Kind;
                     Pointer : out Pointers.Pointer) is
         function Map_Named_Buffer is new API.Loader.Function_With_2_Params
           ("glMapNamedBuffer", UInt, Access_Kind,
            Pointers.Pointer);
      begin
         Pointer := Map_Named_Buffer (Object.Reference.GL_Id, Access_Type);
         Raise_Exception_On_OpenGL_Error;
      end Map;

      procedure Map_Range (Object : in out Buffer; Access_Flags : Access_Bits;
                     Offset, Length : Types.Size;
                     Pointer : out Pointers.Pointer) is
         use type Low_Level.Bitfield;

         function Convert is new Ada.Unchecked_Conversion
           (Source => Access_Bits, Target => Low_Level.Bitfield);
         Raw_Bits : constant Low_Level.Bitfield :=
           Convert (Access_Flags) and 2#0000000011111111#;

         function Map_Named_Buffer_Range is new API.Loader.Function_With_4_Params
           ("glMapNamedBufferRange", UInt, Low_Level.IntPtr, Low_Level.SizeIPtr, Low_Level.Bitfield,
            Pointers.Pointer);

         Offset_In_Bytes : constant Int := Offset * Pointers.Element'Size / System.Storage_Unit;
         Number_Of_Bytes : constant Int := Length * Pointers.Element'Size / System.Storage_Unit;
      begin
         Pointer := Map_Named_Buffer_Range (Object.Reference.GL_Id,
           Low_Level.IntPtr (Offset_In_Bytes), Low_Level.SizeIPtr (Number_Of_Bytes),
           Raw_Bits);
         Raise_Exception_On_OpenGL_Error;
      end Map_Range;

      procedure Flush_Buffer_Range (Object : in out Buffer;
                                    Offset, Length : Types.Size) is
         Offset_In_Bytes : constant Int := Offset * Pointers.Element'Size / System.Storage_Unit;
         Number_Of_Bytes : constant Int := Length * Pointers.Element'Size / System.Storage_Unit;
      begin
         API.Flush_Mapped_Named_Buffer_Range (Object.Reference.GL_Id,
                                              Low_Level.IntPtr (Offset_In_Bytes),
                                              Low_Level.SizeIPtr (Number_Of_Bytes));
         Raise_Exception_On_OpenGL_Error;
      end Flush_Buffer_Range;

      procedure Copy_Sub_Data (Object, Target_Object : in out Buffer;
                               Read_Offset, Write_Offset, Length : Types.Size) is
         Read_Offset_In_Bytes  : constant Int := Read_Offset  * Pointers.Element'Size / System.Storage_Unit;
         Write_Offset_In_Bytes : constant Int := Write_Offset * Pointers.Element'Size / System.Storage_Unit;
         Number_Of_Bytes : constant Int := Length * Pointers.Element'Size / System.Storage_Unit;
      begin
         API.Copy_Named_Buffer_Sub_Data (Object.Reference.GL_Id, Target_Object.Reference.GL_Id,
                                         Low_Level.IntPtr (Read_Offset_In_Bytes),
                                         Low_Level.IntPtr (Write_Offset_In_Bytes),
                                         Low_Level.SizeIPtr (Number_Of_Bytes));
         Raise_Exception_On_OpenGL_Error;
      end Copy_Sub_Data;

      function Pointer (Object : Buffer) return Pointers.Pointer is
         procedure Named_Buffer_Pointer is new API.Loader.Getter_With_3_Params
           ("glGetNamedBufferPointerv", UInt,
            Enums.Buffer_Pointer_Param, Pointers.Pointer);
         Ret : Pointers.Pointer := null;
      begin
         Named_Buffer_Pointer (Object.Reference.GL_Id, Enums.Buffer_Map_Pointer, Ret);
         Raise_Exception_On_OpenGL_Error;
         return Ret;
      end Pointer;

      procedure Set_Sub_Data (Object : Buffer;
                              Offset : Types.Size;
                              Data   : in out Pointers.Element_Array) is
         Offset_In_Bytes : constant Int := Offset * Pointers.Element'Size / System.Storage_Unit;
         Number_Of_Bytes : constant Long := Data'Length * Pointers.Element'Size / System.Storage_Unit;
      begin
         API.Named_Buffer_Sub_Data (Object.Reference.GL_Id,
                                    Low_Level.IntPtr (Offset_In_Bytes),
                                    Low_Level.SizeIPtr (Number_Of_Bytes), Data (Data'First)'Address);
         Raise_Exception_On_OpenGL_Error;
      end Set_Sub_Data;

      procedure Get_Sub_Data (Object : Buffer;
                              Offset : Types.Size;
                              Data   : out Pointers.Element_Array) is
         procedure Get_Named_Buffer_Sub_Data is new API.Loader.Getter_With_4_Params
           ("glGetNamedBufferSubData", UInt, Low_Level.IntPtr,
            Low_Level.SizeIPtr, Pointers.Element_Array);

         Offset_In_Bytes : constant Int := Offset * Pointers.Element'Size / System.Storage_Unit;
         Number_Of_Bytes : constant Long := Data'Length * Pointers.Element'Size / System.Storage_Unit;
      begin
         Get_Named_Buffer_Sub_Data (Object.Reference.GL_Id,
                                    Low_Level.IntPtr (Offset_In_Bytes),
                                    Low_Level.SizeIPtr (Number_Of_Bytes), Data);
         Raise_Exception_On_OpenGL_Error;
      end Get_Sub_Data;

      procedure Invalidate_Sub_Data (Object : in out Buffer;
                                     Offset, Length : Types.Size) is
         Offset_In_Bytes : constant Int := Offset * Pointers.Element'Size / System.Storage_Unit;
         Number_Of_Bytes : constant Int := Length * Pointers.Element'Size / System.Storage_Unit;
      begin
         API.Invalidate_Buffer_Sub_Data (Object.Reference.GL_Id,
                                         Low_Level.IntPtr (Offset_In_Bytes),
                                         Low_Level.SizeIPtr (Number_Of_Bytes));
         Raise_Exception_On_OpenGL_Error;
      end Invalidate_Sub_Data;

   end Buffer_Pointers;

end GL.Objects.Buffers;
