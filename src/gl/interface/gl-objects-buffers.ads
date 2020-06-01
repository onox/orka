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

with Interfaces.C.Pointers;

private with GL.Enums;
private with GL.Low_Level;

package GL.Objects.Buffers is
   pragma Preelaborate;

   function Minimum_Alignment return Types.Size
     with Post => Minimum_Alignment'Result >= 64;
   --  Minimum byte alignment of pointers returned by Map_Range
   --  (at least 64 bytes to support SIMD CPU instructions)

   type Access_Bits is record
      Read              : Boolean := False;
      Write             : Boolean := False;
      Invalidate_Range  : Boolean := False;
      Invalidate_Buffer : Boolean := False;
      Flush_Explicit    : Boolean := False;
      Unsynchronized    : Boolean := False;
      Persistent        : Boolean := False;
      Coherent          : Boolean := False;
   end record
     with Dynamic_Predicate => (Access_Bits.Read or Access_Bits.Write)
       and (if Access_Bits.Flush_Explicit then Access_Bits.Write)
       and (if Access_Bits.Invalidate_Range or Access_Bits.Invalidate_Buffer then
              not Access_Bits.Read);

   type Storage_Bits is record
      Read              : Boolean := False;
      Write             : Boolean := False;
      Persistent        : Boolean := False;
      Coherent          : Boolean := False;
      Dynamic_Storage   : Boolean := False;
      Client_Storage    : Boolean := False;
   end record
     with Dynamic_Predicate => (if Storage_Bits.Coherent then Storage_Bits.Persistent)
       and (if Storage_Bits.Persistent then Storage_Bits.Read or Storage_Bits.Write);

   type Buffer_Target (<>) is tagged limited private;

   type Indexed_Buffer_Target is (Atomic_Counter, Shader_Storage, Uniform);

   function Kind (Target : Buffer_Target) return Indexed_Buffer_Target;
   
   type Buffer is new GL_Object with private;

   function Allocated (Object : Buffer) return Boolean;

   function Mapped (Object : Buffer) return Boolean;

   procedure Bind (Target : Buffer_Target; Object : Buffer'Class);
   --  Bind the buffer object to the target
   --
   --  The target must not be one of the targets that should be used with
   --  Bind_Base or Bind_Range.

   procedure Bind_Base (Target : Buffer_Target; Object : Buffer'Class; Index : Natural);
   --  Bind the buffer object to the index of the target as well as to
   --  the target itself.
   --
   --  Target must be one of the following:
   --
   --    * Atomic_Counter_Buffer
   --    * Uniform_Buffer
   --    * Shader_Storage_Buffer

   procedure Allocate_Storage
     (Object : in out Buffer;
      Length : Long;
      Kind   : Numeric_Type;
      Flags  : Storage_Bits)
   with Pre  => not Object.Allocated,
        Post => Object.Allocated;
   --  Use this instead of Allocate_And_Load_From_Data if you don't want
   --  to copy any data

   overriding
   procedure Initialize_Id (Object : in out Buffer);

   overriding
   procedure Delete_Id (Object : in out Buffer);

   overriding
   function Identifier (Object : Buffer) return Types.Debug.Identifier is
     (Types.Debug.Buffer);

   procedure Unmap (Object : in out Buffer);

   procedure Invalidate_Data (Object : in out Buffer);

   generic
      with package Pointers is new Interfaces.C.Pointers (<>);
   package Buffer_Pointers is

      subtype Pointer is Pointers.Pointer;

      procedure Bind_Range
        (Target : Buffer_Target;
         Object : Buffer'Class;
         Index  : Natural;
         Offset, Length : Types.Size);
      --  Bind a part of the buffer object to the index of the target as
      --  well as to the target itself
      --
      --  Target must be one of the following:
      --
      --    * Atomic_Counter_Buffer
      --    * Uniform_Buffer
      --    * Shader_Storage_Buffer

      procedure Allocate_And_Load_From_Data
        (Object : in out Buffer;
         Data   : Pointers.Element_Array;
         Flags  : Storage_Bits)
      with Pre  => not Object.Allocated,
           Post => Object.Allocated;

      procedure Map_Range
        (Object : in out Buffer;
         Flags  : Access_Bits;
         Offset, Length : Types.Size;
         Pointer : out Pointers.Pointer)
      with Pre => Object.Allocated and not Object.Mapped and Length > 0;

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

      procedure Clear_Sub_Data
        (Object : Buffer;
         Kind   : Numeric_Type;
         Offset, Length : Types.Size;
         Data : in out Pointers.Element_Array)
      with Pre => Data'Length <= 4
        and then Kind /= Double_Type
        and then (if Data'Length = 3 then
          Kind not in Byte_Type | UByte_Type | Short_Type | UShort_Type | Half_Type);

      procedure Copy_Sub_Data (Object, Target_Object : Buffer;
                               Read_Offset, Write_Offset, Length : Types.Size);

      procedure Set_Sub_Data (Object : Buffer;
                              Offset : Types.Size;
                              Data   : Pointers.Element_Array);

      procedure Get_Sub_Data (Object : Buffer;
                              Offset : Types.Size;
                              Data   : out Pointers.Element_Array);

      procedure Invalidate_Sub_Data (Object : Buffer;
                                     Offset, Length : Types.Size);

   end Buffer_Pointers;

   --  Array_Buffer, Texture_Buffer,
   --  Copy_Read_Buffer, and Copy_Write_Buffer are no longer needed
   --  since the GL.Objects.* packages use DSA
   --  Transform_Feedback_Buffer replaced by Shader_Storage_Buffer
   Element_Array_Buffer      : aliased constant Buffer_Target;
   Pixel_Pack_Buffer         : aliased constant Buffer_Target;
   Pixel_Unpack_Buffer       : aliased constant Buffer_Target;
   Draw_Indirect_Buffer      : aliased constant Buffer_Target;
   Parameter_Buffer          : aliased constant Buffer_Target;
   Dispatch_Indirect_Buffer  : aliased constant Buffer_Target;
   Query_Buffer              : aliased constant Buffer_Target;

   --  Buffer targets that must be binded to a specific index
   --  (specified in shaders)
   Uniform_Buffer            : aliased constant Buffer_Target;
   Shader_Storage_Buffer     : aliased constant Buffer_Target;
   Atomic_Counter_Buffer     : aliased constant Buffer_Target;

private

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

   type Buffer_Target (Kind : Enums.Buffer_Kind) is
     tagged limited null record;

   type Buffer is new GL_Object with record
      Allocated, Mapped : Boolean := False;
   end record;

   Element_Array_Buffer      : aliased constant Buffer_Target
     := Buffer_Target'(Kind => Enums.Element_Array_Buffer);
   Pixel_Pack_Buffer         : aliased constant Buffer_Target
     := Buffer_Target'(Kind => Enums.Pixel_Pack_Buffer);
   Pixel_Unpack_Buffer       : aliased constant Buffer_Target
     := Buffer_Target'(Kind => Enums.Pixel_Unpack_Buffer);
   Uniform_Buffer            : aliased constant Buffer_Target
     := Buffer_Target'(Kind => Enums.Uniform_Buffer);
   Draw_Indirect_Buffer      : aliased constant Buffer_Target
     := Buffer_Target'(Kind => Enums.Draw_Indirect_Buffer);
   Parameter_Buffer          : aliased constant Buffer_Target
     := Buffer_Target'(Kind => Enums.Parameter_Buffer);
   Shader_Storage_Buffer     : aliased constant Buffer_Target
     := Buffer_Target'(Kind => Enums.Shader_Storage_Buffer);
   Dispatch_Indirect_Buffer  : aliased constant Buffer_Target
     := Buffer_Target'(Kind => Enums.Dispatch_Indirect_Buffer);
   Query_Buffer              : aliased constant Buffer_Target
     := Buffer_Target'(Kind => Enums.Query_Buffer);
   Atomic_Counter_Buffer     : aliased constant Buffer_Target
     := Buffer_Target'(Kind => Enums.Atomic_Counter_Buffer);

end GL.Objects.Buffers;
