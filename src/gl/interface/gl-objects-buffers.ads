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

   type Buffer_Usage is (Stream_Draw, Stream_Read, Stream_Copy,
                         Static_Draw, Static_Read, Static_Copy,
                         Dynamic_Draw, Dynamic_Read, Dynamic_Copy);
   
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

   function Current_Object (Target : Buffer_Target) return Buffer'Class;
   
   generic
      with package Pointers is new Interfaces.C.Pointers (<>);
   procedure Load_To_Buffer (Object : Buffer;
                             Data   : Pointers.Element_Array;
                             Usage  : Buffer_Usage);

   procedure Allocate (Object : Buffer; Number_Of_Elements : Long;
                       Kind : Numeric_Type; Usage : Buffer_Usage);
   -- Use this instead of Load_To_Buffer when you don't want to copy any data

   generic
      with package Pointers is new Interfaces.C.Pointers (<>);
   procedure Load_To_Immutable_Buffer (Object : Buffer;
                                       Data   : Pointers.Element_Array;
                                       Storage_Flags : Storage_Bits);

   generic
      with package Pointers is new Interfaces.C.Pointers (<>);
   procedure Map (Object : in out Buffer; Access_Type : Access_Kind;
                  Pointer : out Pointers.Pointer);

   generic
      with package Pointers is new Interfaces.C.Pointers (<>);
   procedure Map_Range (Object : in out Buffer; Access_Flags : Access_Bits;
                        Offset, Length : Types.Size;
                        Pointer : out Pointers.Pointer);

   procedure Unmap (Object : in out Buffer);

   generic
      with package Pointers is new Interfaces.C.Pointers (<>);
   procedure Flush_Buffer_Range (Object : in out Buffer;
                                 Offset, Length : Types.Size);

   generic
      with package Pointers is new Interfaces.C.Pointers (<>);
   procedure Copy_Sub_Data (Object, Target_Object : in out Buffer;
                            Read_Offset, Write_Offset, Length : Types.Size);

   generic
      with package Pointers is new Interfaces.C.Pointers (<>);
   function Pointer (Object : Buffer) return Pointers.Pointer;

   generic
      with package Pointers is new Interfaces.C.Pointers (<>);
   procedure Set_Sub_Data (Object : Buffer;
                           Offset : Types.Size;
                           Data   : in out Pointers.Element_Array);

   generic
      with package Pointers is new Interfaces.C.Pointers (<>);
   procedure Get_Sub_Data (Object : Buffer;
                           Offset : Types.Size;
                           Data   : out Pointers.Element_Array);

   function Access_Type   (Object : Buffer) return Access_Kind;
   function Immutable     (Object : Buffer) return Boolean;
   function Mapped        (Object : Buffer) return Boolean;
   function Size          (Object : Buffer) return Size;
   function Storage_Flags (Object : Buffer) return Storage_Bits;
   function Usage         (Object : Buffer) return Buffer_Usage;

   procedure Draw_Elements (Mode : Connection_Mode; Count : Types.Size;
                            Index_Type : Unsigned_Numeric_Type);
   procedure Draw_Elements (Mode : Connection_Mode; Count : Types.Size;
                            Index_Type : Unsigned_Numeric_Type;
                            Instances  : Types.Size);

   procedure Draw_Elements_Base_Vertex (Mode : Connection_Mode; Count : Types.Size;
                            Index_Type : Unsigned_Numeric_Type;
                            Vertex_Offset, Index_Offset : Int);
   procedure Draw_Elements_Base_Vertex (Mode : Connection_Mode; Count : Types.Size;
                            Index_Type : Unsigned_Numeric_Type;
                            Instances  : Types.Size;
                            Vertex_Offset, Index_Offset : Int);

   procedure Draw_Multiple_Elements (Mode : Connection_Mode; Count : Size_Array;
                                     Index_Type : Unsigned_Numeric_Type);

   procedure Draw_Multiple_Elements_Base_Vertex (Mode : Connection_Mode; Count : Size_Array;
                                     Index_Type : Unsigned_Numeric_Type;
                                     Vertex_Offsets, Index_Offsets : Int_Array)
     with Pre => Count'Length = Vertex_Offsets'Length and
                 Count'Length = Index_Offsets'Length;

   overriding
   procedure Initialize_Id (Object : in out Buffer);
   
   overriding
   procedure Delete_Id (Object : in out Buffer);

   procedure Invalidate_Data (Object : in out Buffer);   

   generic
      with package Pointers is new Interfaces.C.Pointers (<>);
   procedure Invalidate_Sub_Data (Object : in out Buffer;
                                  Offset, Length : Types.Size);

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

   for Buffer_Usage use (Stream_Draw  => 16#88E0#,
                         Stream_Read  => 16#88E1#,
                         Stream_Copy  => 16#88E2#,
                         Static_Draw  => 16#88E4#,
                         Static_Read  => 16#88E5#,
                         Static_Copy  => 16#88E6#,
                         Dynamic_Draw => 16#88E8#,
                         Dynamic_Read => 16#88E9#,
                         Dynamic_Copy => 16#88EA#);
   for Buffer_Usage'Size use Low_Level.Enum'Size;

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
   Shader_Storage_Buffer     : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Shader_Storage_Buffer);
   Dispatch_Indirect_Buffer  : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Dispatch_Indirect_Buffer);
   Query_Buffer              : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Query_Buffer);
   Atomic_Counter_Buffer     : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Atomic_Counter_Buffer);

end GL.Objects.Buffers;
