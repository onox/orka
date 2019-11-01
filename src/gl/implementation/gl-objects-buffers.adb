--  SPDX-License-Identifier: Apache-2.0
--
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
with GL.Enums.Getter;
with GL.Pixels;

package body GL.Objects.Buffers is

   function Minimum_Alignment return Types.Size is
      Ret : Types.Size := 64;
   begin
      API.Get_Size (Enums.Getter.Min_Map_Buffer_Alignment, Ret);
      return Ret;
   end Minimum_Alignment;

   use type Enums.Buffer_Kind;

   type Format_Data is record
      Internal_Format : GL.Pixels.Internal_Format_Buffer_Texture;
      Format          : GL.Pixels.Format;
      Data_Type       : GL.Pixels.Data_Type;
   end record;

   function Get_Format_Data (Kind : Numeric_Type; Length : Natural) return Format_Data is
      use GL.Pixels;

      type Format_Array is array (1 .. 4) of Format;

      Float_Formats   : constant Format_Array := (Red, RG, RGB, RGBA);
      Integer_Formats : constant Format_Array := (Red_Integer, RG_Integer, RGB_Integer, RGBA_Integer);

      type Internal_Format_Array is array (1 .. 4) of Internal_Format_Buffer_Texture;

      Internal_Formats : Internal_Format_Array;
   begin
      return Result : Format_Data do
         --  For certain data types, OpenGL does not provide a sized
         --  internal format [for buffer textures] that has three components
         case Kind is
            when Byte_Type =>
               Result.Data_Type := Pixels.Byte;
               Internal_Formats := (R8I, RG8I, R8I, RGBA8I);
               --  Third position invalid
            when UByte_Type =>
               Result.Data_Type := Pixels.Unsigned_Byte;
               Internal_Formats := (R8UI, RG8UI, R8UI, RGBA8UI);
               --  Third position invalid
            when Short_Type =>
               Result.Data_Type := Pixels.Short;
               Internal_Formats := (R16I, RG16I, R16I, RGBA16I);
               --  Third position invalid
            when UShort_Type =>
               Result.Data_Type := Pixels.Unsigned_Short;
               Internal_Formats := (R16UI, RG16UI, R16UI, RGBA16UI);
               --  Third position invalid
            when Int_Type =>
               Result.Data_Type := Pixels.Int;
               Internal_Formats := (R32I, RG32I, RGB32I, RGBA32I);
            when UInt_Type =>
               Result.Data_Type := Pixels.Unsigned_Int;
               Internal_Formats := (R32UI, RG32UI, RGB32UI, RGBA32UI);
            when Half_Type =>
               Result.Data_Type := Pixels.Half_Float;
               Internal_Formats := (R16F, RG16F, R16F, RGBA16F);
               --  Third position invalid
            when Single_Type =>
               Result.Data_Type := Pixels.Float;
               Internal_Formats := (R32F, RG32F, RGB32F, RGBA32F);
            when Double_Type =>
               raise Constraint_Error;
         end case;

         Result.Internal_Format := Internal_Formats (Length);

         case Kind is
            when Byte_Type .. UInt_Type =>
               Result.Format := Integer_Formats (Length);
            when Half_Type | Single_Type =>
               Result.Format := Float_Formats (Length);
            when Double_Type =>
               raise Constraint_Error;
         end case;
      end return;
   end Get_Format_Data;

   package Buffer_Holder is new Ada.Containers.Indefinite_Holders
     (Element_Type => Buffer'Class);

   type Buffer_Target_Array is array (Enums.Buffer_Kind) of Buffer_Holder.Holder;
   Current_Buffers : Buffer_Target_Array;

   procedure Bind (Target : Buffer_Target; Object : Buffer'Class) is
      Holder : Buffer_Holder.Holder := Current_Buffers (Target.Kind);
   begin
      if Holder.Is_Empty or else Object /= Holder.Element then
         API.Bind_Buffer (Target.Kind, Object.Reference.GL_Id);
         Holder.Replace_Element (Object);
      end if;
   end Bind;

   procedure Bind_Base (Target : Buffer_Target; Object : Buffer'Class; Index : Natural) is
      Holder : Buffer_Holder.Holder := Current_Buffers (Target.Kind);
   begin
      if Holder.Is_Empty or else Object /= Holder.Element then
         API.Bind_Buffer_Base (Target.Kind, UInt (Index), Object.Reference.GL_Id);
         Holder.Replace_Element (Object);
      end if;
   end Bind_Base;

   function Current_Object (Target : Buffer_Target) return Buffer'Class is
      Holder : constant Buffer_Holder.Holder := Current_Buffers (Target.Kind);
   begin
      if Holder.Is_Empty then
         raise No_Object_Bound_Exception with Target.Kind'Image;
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
   end Allocate;

   function Access_Type (Object : Buffer) return Access_Kind is
      Ret : Access_Kind := Access_Kind'First;
   begin
      API.Get_Named_Buffer_Parameter_Access_Kind (Object.Reference.GL_Id, Enums.Buffer_Access,
                                            Ret);
      return Ret;
   end Access_Type;

   function Immutable (Object : Buffer) return Boolean is
      Ret : Low_Level.Bool := Low_Level.Bool (False);
   begin
      API.Get_Named_Buffer_Parameter_Bool (Object.Reference.GL_Id, Enums.Buffer_Immutable_Storage, Ret);
      return Boolean (Ret);
   end Immutable;

   function Mapped (Object : Buffer) return Boolean is
      Ret : Low_Level.Bool := Low_Level.Bool (False);
   begin
      API.Get_Named_Buffer_Parameter_Bool (Object.Reference.GL_Id, Enums.Buffer_Mapped, Ret);
      return Boolean (Ret);
   end Mapped;

   function Size (Object : Buffer) return Types.Long_Size is
      Ret : Types.Long_Size := 0;
   begin
      API.Get_Named_Buffer_Parameter_Size (Object.Reference.GL_Id, Enums.Buffer_Size, Ret);
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
      return Convert (Raw_Bits and 2#0000001111000011#);
   end Storage_Flags;

   overriding procedure Initialize_Id (Object : in out Buffer) is
      New_Id : UInt := 0;
   begin
      API.Create_Buffers (1, New_Id);
      Object.Reference.GL_Id := New_Id;
      Object.Reference.Initialized := True;
   end Initialize_Id;

   overriding procedure Delete_Id (Object : in out Buffer) is
      Arr : constant Low_Level.UInt_Array := (1 => Object.Reference.GL_Id);
   begin
      API.Delete_Buffers (1, Arr);
      Object.Reference.GL_Id := 0;
      Object.Reference.Initialized := False;
   end Delete_Id;

   procedure Unmap (Object : in out Buffer) is
   begin
      API.Unmap_Named_Buffer (Object.Reference.GL_Id);
   end Unmap;

   procedure Clear_With_Zeros (Object : Buffer) is
   begin
      --  The internal format, format, and data type need to match
      --  each other, but are otherwise ignored because of the null
      --  address
      API.Clear_Named_Buffer_Data
        (Object.Reference.GL_Id, GL.Pixels.R8I, GL.Pixels.Red_Integer,
         GL.Pixels.Byte, System.Null_Address);
   end Clear_With_Zeros;

   procedure Invalidate_Data (Object : in out Buffer) is
   begin
      API.Invalidate_Buffer_Data (Object.Reference.GL_Id);
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
      end Load_To_Immutable_Buffer;

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
      end Map_Range;

      function Get_Mapped_Data
        (Pointer : not null Pointers.Pointer;
         Offset, Length : Types.Size) return Pointers.Element_Array
      is
         package IC renames Interfaces.C;
         use type Pointers.Pointer;
      begin
         return Pointers.Value (Pointer + IC.ptrdiff_t (Offset), IC.ptrdiff_t (Length));
      end Get_Mapped_Data;

      procedure Set_Mapped_Data
        (Pointer : not null Pointers.Pointer;
         Offset  : Types.Size;
         Data    : Pointers.Element_Array)
      is
         package IC renames Interfaces.C;
         use type Pointers.Pointer;

         subtype Data_Array is Pointers.Element_Array (Data'Range);

         type Data_Access is access Data_Array;

         function Convert is new Ada.Unchecked_Conversion
           (Source => Pointers.Pointer, Target => Data_Access);
      begin
         Convert (Pointer + IC.ptrdiff_t (Offset)).all := Data;
      end Set_Mapped_Data;

      procedure Set_Mapped_Data
        (Pointer : not null Pointers.Pointer;
         Offset  : Types.Size;
         Value   : Pointers.Element)
      is
         package IC renames Interfaces.C;
         use type Pointers.Pointer;

         Offset_Pointer : constant Pointers.Pointer := Pointer + IC.ptrdiff_t (Offset);
      begin
         Offset_Pointer.all := Value;
      end Set_Mapped_Data;

      procedure Flush_Buffer_Range (Object : in out Buffer;
                                    Offset, Length : Types.Size) is
         Offset_In_Bytes : constant Int := Offset * Pointers.Element'Size / System.Storage_Unit;
         Number_Of_Bytes : constant Int := Length * Pointers.Element'Size / System.Storage_Unit;
      begin
         API.Flush_Mapped_Named_Buffer_Range (Object.Reference.GL_Id,
                                              Low_Level.IntPtr (Offset_In_Bytes),
                                              Low_Level.SizeIPtr (Number_Of_Bytes));
      end Flush_Buffer_Range;

      procedure Clear_Data
        (Object : Buffer;
         Kind : Numeric_Type;
         Data : in out Pointers.Element_Array)
      is
         Format_Info : constant Format_Data := Get_Format_Data (Kind, Data'Length);
      begin
         API.Clear_Named_Buffer_Data
           (Object.Reference.GL_Id, Format_Info.Internal_Format,
            Format_Info.Format, Format_Info.Data_Type, Data (Data'First)'Address);
      end Clear_Data;

      procedure Clear_Sub_Data
        (Object : Buffer;
         Offset, Length : Types.Size;
         Kind : Numeric_Type;
         Data : in out Pointers.Element_Array)
      is
         Offset_In_Bytes : constant Int := Offset * Pointers.Element'Size / System.Storage_Unit;
         Length_In_Bytes : constant Int := Length * Pointers.Element'Size / System.Storage_Unit;

         Format_Info : constant Format_Data := Get_Format_Data (Kind, Data'Length);
      begin
         API.Clear_Named_Buffer_Sub_Data
           (Object.Reference.GL_Id, Format_Info.Internal_Format,
            Low_Level.IntPtr (Offset_In_Bytes), Low_Level.SizeIPtr (Length_In_Bytes),
            Format_Info.Format, Format_Info.Data_Type, Data (Data'First)'Address);
      end Clear_Sub_Data;

      procedure Clear_With_Zeros (Object : Buffer; Offset, Length : Types.Size) is
         Offset_In_Bytes : constant Int := Offset * Pointers.Element'Size / System.Storage_Unit;
         Length_In_Bytes : constant Int := Length * Pointers.Element'Size / System.Storage_Unit;
      begin
         --  The internal format, format, and data type need to match
         --  each other, but are otherwise ignored because of the null
         --  address
         API.Clear_Named_Buffer_Sub_Data
           (Object.Reference.GL_Id, GL.Pixels.R8I,
            Low_Level.IntPtr (Offset_In_Bytes), Low_Level.SizeIPtr (Length_In_Bytes),
            GL.Pixels.Red_Integer, GL.Pixels.Byte, System.Null_Address);
      end Clear_With_Zeros;

      procedure Copy_Sub_Data (Object, Target_Object : Buffer;
                               Read_Offset, Write_Offset, Length : Types.Size) is
         Read_Offset_In_Bytes  : constant Int := Read_Offset  * Pointers.Element'Size / System.Storage_Unit;
         Write_Offset_In_Bytes : constant Int := Write_Offset * Pointers.Element'Size / System.Storage_Unit;
         Number_Of_Bytes : constant Int := Length * Pointers.Element'Size / System.Storage_Unit;
      begin
         API.Copy_Named_Buffer_Sub_Data (Object.Reference.GL_Id, Target_Object.Reference.GL_Id,
                                         Low_Level.IntPtr (Read_Offset_In_Bytes),
                                         Low_Level.IntPtr (Write_Offset_In_Bytes),
                                         Low_Level.SizeIPtr (Number_Of_Bytes));
      end Copy_Sub_Data;

      function To_Pointer (Object : Buffer) return Pointers.Pointer is
         procedure Named_Buffer_Pointer is new API.Loader.Getter_With_3_Params
           ("glGetNamedBufferPointerv", UInt,
            Enums.Buffer_Pointer_Param, Pointers.Pointer);
         Ret : Pointers.Pointer := null;
      begin
         Named_Buffer_Pointer (Object.Reference.GL_Id, Enums.Buffer_Map_Pointer, Ret);
         return Ret;
      end To_Pointer;

      procedure Set_Sub_Data (Object : Buffer;
                              Offset : Types.Size;
                              Data   : Pointers.Element_Array) is
         Offset_In_Bytes : constant Int := Offset * Pointers.Element'Size / System.Storage_Unit;
         Number_Of_Bytes : constant Long := Data'Length * Pointers.Element'Size / System.Storage_Unit;
      begin
         API.Named_Buffer_Sub_Data (Object.Reference.GL_Id,
                                    Low_Level.IntPtr (Offset_In_Bytes),
                                    Low_Level.SizeIPtr (Number_Of_Bytes), Data (Data'First)'Address);
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
      end Get_Sub_Data;

      procedure Invalidate_Sub_Data (Object : Buffer;
                                     Offset, Length : Types.Size) is
         Offset_In_Bytes : constant Int := Offset * Pointers.Element'Size / System.Storage_Unit;
         Number_Of_Bytes : constant Int := Length * Pointers.Element'Size / System.Storage_Unit;
      begin
         API.Invalidate_Buffer_Sub_Data (Object.Reference.GL_Id,
                                         Low_Level.IntPtr (Offset_In_Bytes),
                                         Low_Level.SizeIPtr (Number_Of_Bytes));
      end Invalidate_Sub_Data;

   end Buffer_Pointers;

end GL.Objects.Buffers;
