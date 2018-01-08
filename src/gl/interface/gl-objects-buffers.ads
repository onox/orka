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

with Interfaces.C.Pointers;

private with GL.Low_Level.Enums;

package GL.Objects.Buffers is
   pragma Preelaborate;

   type Access_Kind is (Read_Only, Write_Only, Read_Write);

   type Access_Bits is record
      Read              : Boolean := False;
      Write             : Boolean := False;
      Invalidate_Range  : Boolean := False;
      Invalidate_Buffer : Boolean := False;
      Flush_Explicit    : Boolean := False;
      Unsynchronized    : Boolean := False;
      Persistent        : Boolean := False;
      Coherent          : Boolean := False;
   end record;

   type Storage_Bits is record
      Read              : Boolean := False;
      Write             : Boolean := False;
      Persistent        : Boolean := False;
      Coherent          : Boolean := False;
      Dynamic_Storage   : Boolean := False;
      Client_Storage    : Boolean := False;
   end record;

   type Buffer_Target (<>) is tagged limited private;
   
   type Buffer is new GL_Object with private;

   procedure Bind (Target : Buffer_Target; Object : Buffer'Class);
   --  Bind the buffer object to the target

   procedure Bind_Base (Target : Buffer_Target; Object : Buffer'Class; Index : Natural);
   --  Bind the buffer object to the index of the target as well as to
   --  the target itself.
   --
   --  Target must be one of the following:
   --
   --    * Atomic_Counter_Buffer
   --    * Transform_Feedback_Buffer
   --    * Uniform_Buffer
   --    * Shader_Storage_Buffer

   procedure Allocate (Object : Buffer; Number_Of_Elements : Long;
                       Kind : Numeric_Type; Storage_Flags : Storage_Bits);
   --  Use this instead of Load_To_Immutable_Buffer when you don't want
   --  to copy any data

   function Current_Object (Target : Buffer_Target) return Buffer'Class;

   function Access_Type   (Object : Buffer) return Access_Kind;
   function Immutable     (Object : Buffer) return Boolean;
   function Mapped        (Object : Buffer) return Boolean;
   function Size          (Object : Buffer) return Long_Size;
   function Storage_Flags (Object : Buffer) return Storage_Bits;

   overriding
   procedure Initialize_Id (Object : in out Buffer);

   overriding
   procedure Delete_Id (Object : in out Buffer);

   overriding
   function Identifier (Object : Buffer) return Types.Debug.Identifier is
     (Types.Debug.Buffer);

   procedure Unmap (Object : in out Buffer);

   procedure Clear_With_Zeros (Object : Buffer);

   procedure Invalidate_Data (Object : in out Buffer);

   generic
      with package Pointers is new Interfaces.C.Pointers (<>);
   package Buffer_Pointers is

      subtype Pointer is Pointers.Pointer;

      procedure Bind_Range (Target : Buffer_Target; Object : Buffer'Class; Index : Natural;
                            Offset, Length : Types.Size);
      --  Bind a part of the buffer object to the index of the target as
      --  well as to the target itself.
      --
      --  Target must be one of the following:
      --
      --    * Atomic_Counter_Buffer
      --    * Transform_Feedback_Buffer
      --    * Uniform_Buffer
      --    * Shader_Storage_Buffer

      procedure Load_To_Immutable_Buffer (Object : Buffer;
                                          Data   : Pointers.Element_Array;
                                          Storage_Flags : Storage_Bits);

      procedure Map_Range (Object : in out Buffer; Access_Flags : Access_Bits;
                           Offset, Length : Types.Size;
                           Pointer : out Pointers.Pointer);

      function Get_Mapped_Data
        (Pointer : not null Pointers.Pointer;
         Offset, Length : Types.Size) return Pointers.Element_Array
      with Pre  => Length > 0,
           Post => Get_Mapped_Data'Result'Length = Length;

      procedure Set_Mapped_Data
        (Pointer : not null Pointers.Pointer;
         Offset  : Types.Size;
         Data    : Pointers.Element_Array)
      with Pre => Data'Length > 0;

      procedure Set_Mapped_Data
        (Pointer : not null Pointers.Pointer;
         Offset  : Types.Size;
         Value   : Pointers.Element);

      procedure Flush_Buffer_Range (Object : in out Buffer;
                                    Offset, Length : Types.Size);

      function To_Pointer (Object : Buffer) return Pointers.Pointer;

      procedure Clear_Data
        (Object : Buffer;
         Kind : Numeric_Type;
         Data : in out Pointers.Element_Array)
      with Pre => Data'Length in 1 .. 4
        and then Kind /= Double_Type
        and then (if Data'Length = 3 then
          Kind not in Byte_Type | UByte_Type | Short_Type | UShort_Type | Half_Type);

      procedure Clear_Sub_Data
        (Object : Buffer;
         Offset, Length : Types.Size;
         Kind : Numeric_Type;
         Data : in out Pointers.Element_Array)
      with Pre => Data'Length in 1 .. 4
        and then Kind /= Double_Type
        and then (if Data'Length = 3 then
          Kind not in Byte_Type | UByte_Type | Short_Type | UShort_Type | Half_Type);

      procedure Clear_With_Zeros (Object : Buffer; Offset, Length : Types.Size);

      procedure Copy_Sub_Data (Object, Target_Object : Buffer;
                               Read_Offset, Write_Offset, Length : Types.Size);

      procedure Set_Sub_Data (Object : Buffer;
                              Offset : Types.Size;
                              Data   : in out Pointers.Element_Array);

      procedure Get_Sub_Data (Object : Buffer;
                              Offset : Types.Size;
                              Data   : out Pointers.Element_Array);

      procedure Invalidate_Sub_Data (Object : Buffer;
                                     Offset, Length : Types.Size);

   end Buffer_Pointers;

   Array_Buffer              : constant Buffer_Target;
   Element_Array_Buffer      : constant Buffer_Target;
   Pixel_Pack_Buffer         : constant Buffer_Target;
   Pixel_Unpack_Buffer       : constant Buffer_Target;
   Uniform_Buffer            : constant Buffer_Target;
   Texture_Buffer            : constant Buffer_Target;
   Transform_Feedback_Buffer : constant Buffer_Target;
   Copy_Read_Buffer          : constant Buffer_Target;
   Copy_Write_Buffer         : constant Buffer_Target;
   Draw_Indirect_Buffer      : constant Buffer_Target;
   Parameter_Buffer          : constant Buffer_Target;
   Shader_Storage_Buffer     : constant Buffer_Target;
   Dispatch_Indirect_Buffer  : constant Buffer_Target;
   Query_Buffer              : constant Buffer_Target;
   Atomic_Counter_Buffer     : constant Buffer_Target;

private

   for Access_Kind use (Read_Only  => 16#88B8#,
                        Write_Only => 16#88B9#,
                        Read_Write => 16#88BA#);
   for Access_Kind'Size use Low_Level.Enum'Size;

   for Access_Bits use record
      Read              at 0 range 0 .. 0;
      Write             at 0 range 1 .. 1;
      Invalidate_Range  at 0 range 2 .. 2;
      Invalidate_Buffer at 0 range 3 .. 3;
      Flush_Explicit    at 0 range 4 .. 4;
      Unsynchronized    at 0 range 5 .. 5;
      Persistent        at 0 range 6 .. 6;
      Coherent          at 0 range 7 .. 7;
   end record;
   for Access_Bits'Size use Low_Level.Bitfield'Size;

   for Storage_Bits use record
      Read              at 0 range 0 .. 0;
      Write             at 0 range 1 .. 1;
      Persistent        at 0 range 6 .. 6;
      Coherent          at 0 range 7 .. 7;
      Dynamic_Storage   at 0 range 8 .. 8;
      Client_Storage    at 0 range 9 .. 9;
   end record;
   for Storage_Bits'Size use Low_Level.Bitfield'Size;

   type Buffer_Target (Kind : Low_Level.Enums.Buffer_Kind) is
     tagged limited null record;

   type Buffer is new GL_Object with null record;
   
   Array_Buffer              : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Array_Buffer);
   Element_Array_Buffer      : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Element_Array_Buffer);
   Pixel_Pack_Buffer         : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Pixel_Pack_Buffer);
   Pixel_Unpack_Buffer       : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Pixel_Unpack_Buffer);
   Uniform_Buffer            : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Uniform_Buffer);
   Texture_Buffer            : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Texture_Buffer);
   Transform_Feedback_Buffer : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Transform_Feedback_Buffer);
   Copy_Read_Buffer          : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Copy_Read_Buffer);
   Copy_Write_Buffer         : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Copy_Write_Buffer);
   Draw_Indirect_Buffer      : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Draw_Indirect_Buffer);
   Parameter_Buffer          : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Parameter_Buffer);
   Shader_Storage_Buffer     : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Shader_Storage_Buffer);
   Dispatch_Indirect_Buffer  : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Dispatch_Indirect_Buffer);
   Query_Buffer              : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Query_Buffer);
   Atomic_Counter_Buffer     : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Atomic_Counter_Buffer);

end GL.Objects.Buffers;
