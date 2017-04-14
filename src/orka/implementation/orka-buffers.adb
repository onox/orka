--  Copyright (c) 2016 onox <denkpadje@gmail.com>
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

with System;

package body Orka.Buffers is

   package Half_Pointers is new GL.Objects.Buffers.Buffer_Pointers
     (Half_Pointers);

   package Single_Pointers is new GL.Objects.Buffers.Buffer_Pointers
     (Single_Pointers);

   package UInt_Pointers is new GL.Objects.Buffers.Buffer_Pointers
     (UInt_Pointers);

   package Color_Pointers is new GL.Objects.Buffers.Buffer_Pointers
     (Colors.Basic_Color_Pointers);

   package Matrix4_Pointers is new GL.Objects.Buffers.Buffer_Pointers
     (Singles.Matrix4_Pointers);

   package Arrays_Command_Pointers is new GL.Objects.Buffers.Buffer_Pointers
     (Indirect.Arrays_Indirect_Command_Pointers);

   package Elements_Command_Pointers is new GL.Objects.Buffers.Buffer_Pointers
     (Indirect.Elements_Indirect_Command_Pointers);

   function Create_Buffer
     (Flags  : GL.Objects.Buffers.Storage_Bits;
      Kind   : Numeric_Type;
      Length : Natural) return Buffer is
   begin
      return Result : Buffer do
         Result.Buffer.Allocate (Long (Length), Kind, Flags);
         Result.Length := Length;
      end return;
   end Create_Buffer;

   function Create_Buffer
     (Flags  : GL.Objects.Buffers.Storage_Bits;
      Kind   : Orka.Types.Non_Numeric_Type;
      Length : Natural) return Buffer
   is
      use Orka.Types;

      Bytes : Natural;
   begin
      case Kind is
         when Single_Vector_Type =>
            return Create_Buffer (Flags, Single_Type, Length * 4);
         when Double_Vector_Type =>
            return Create_Buffer (Flags, Double_Type, Length * 4);
         when Single_Matrix_Type =>
            return Create_Buffer (Flags, Single_Type, Length * 16);
         when Double_Matrix_Type =>
            return Create_Buffer (Flags, Double_Type, Length * 16);
         when Arrays_Command_Type =>
            Bytes := Indirect.Arrays_Indirect_Command'Size / System.Storage_Unit;
            return Create_Buffer (Flags, Byte_Type, Length * Bytes);
         when Elements_Command_Type =>
            Bytes := Indirect.Elements_Indirect_Command'Size / System.Storage_Unit;
            return Create_Buffer (Flags, Byte_Type, Length * Bytes);
      end case;
   end Create_Buffer;

   function Create_Buffer
     (Flags  : GL.Objects.Buffers.Storage_Bits;
      Data   : Half_Array) return Buffer is
   begin
      return Result : Buffer do
         Half_Pointers.Load_To_Immutable_Buffer (Result.Buffer, Data, Flags);
         Result.Length := Data'Length;
      end return;
   end Create_Buffer;

   function Create_Buffer
     (Flags  : GL.Objects.Buffers.Storage_Bits;
      Data   : Single_Array) return Buffer is
   begin
      return Result : Buffer do
         Single_Pointers.Load_To_Immutable_Buffer (Result.Buffer, Data, Flags);
         Result.Length := Data'Length;
      end return;
   end Create_Buffer;

   function Create_Buffer
     (Flags  : GL.Objects.Buffers.Storage_Bits;
      Data   : UInt_Array) return Buffer is
   begin
      return Result : Buffer do
         UInt_Pointers.Load_To_Immutable_Buffer (Result.Buffer, Data, Flags);
         Result.Length := Data'Length;
      end return;
   end Create_Buffer;

   function Create_Buffer
     (Flags  : GL.Objects.Buffers.Storage_Bits;
      Data   : Colors.Basic_Color_Array) return Buffer is
   begin
      return Result : Buffer do
         Color_Pointers.Load_To_Immutable_Buffer (Result.Buffer, Data, Flags);
         Result.Length := Data'Length;
      end return;
   end Create_Buffer;

   function Create_Buffer
     (Flags  : GL.Objects.Buffers.Storage_Bits;
      Data   : Singles.Matrix4_Array) return Buffer is
   begin
      return Result : Buffer do
         Matrix4_Pointers.Load_To_Immutable_Buffer (Result.Buffer, Data, Flags);
         Result.Length := Data'Length;
      end return;
   end Create_Buffer;

   function Create_Buffer
     (Flags  : GL.Objects.Buffers.Storage_Bits;
      Data   : Indirect.Arrays_Indirect_Command_Array) return Buffer is
   begin
      return Result : Buffer do
         Arrays_Command_Pointers.Load_To_Immutable_Buffer (Result.Buffer, Data, Flags);
         Result.Length := Data'Length;
      end return;
   end Create_Buffer;

   function Create_Buffer
     (Flags  : GL.Objects.Buffers.Storage_Bits;
      Data   : Indirect.Elements_Indirect_Command_Array) return Buffer is
   begin
      return Result : Buffer do
         Elements_Command_Pointers.Load_To_Immutable_Buffer (Result.Buffer, Data, Flags);
         Result.Length := Data'Length;
      end return;
   end Create_Buffer;

   function GL_Buffer (Object : Buffer) return GL.Objects.Buffers.Buffer
     is (Object.Buffer);

   function Length (Object : Buffer) return Natural
     is (Object.Length);

   procedure Set_Data
     (Object : Buffer;
      Data   : in out Half_Array;
      Offset : Natural := 0) is
   begin
      Half_Pointers.Set_Sub_Data (Object.Buffer, Int (Offset), Data);
   end Set_Data;

   procedure Set_Data
     (Object : Buffer;
      Data   : in out Single_Array;
      Offset : Natural := 0) is
   begin
      Single_Pointers.Set_Sub_Data (Object.Buffer, Int (Offset), Data);
   end Set_Data;

   procedure Set_Data
     (Object : Buffer;
      Data   : in out UInt_Array;
      Offset : Natural := 0) is
   begin
      UInt_Pointers.Set_Sub_Data (Object.Buffer, Int (Offset), Data);
   end Set_Data;

   procedure Set_Data
     (Object : Buffer;
      Data   : in out Colors.Basic_Color_Array;
      Offset : Natural := 0) is
   begin
      Color_Pointers.Set_Sub_Data (Object.Buffer, Int (Offset), Data);
   end Set_Data;

   procedure Set_Data
     (Object : Buffer;
      Data   : in out Singles.Matrix4_Array;
      Offset : Natural := 0) is
   begin
      Matrix4_Pointers.Set_Sub_Data (Object.Buffer, Int (Offset), Data);
   end Set_Data;

   procedure Set_Data
     (Object : Buffer;
      Data   : in out Indirect.Arrays_Indirect_Command_Array;
      Offset : Natural := 0) is
   begin
      Arrays_Command_Pointers.Set_Sub_Data (Object.Buffer, Int (Offset), Data);
   end Set_Data;

   procedure Set_Data
     (Object : Buffer;
      Data   : in out Indirect.Elements_Indirect_Command_Array;
      Offset : Natural := 0) is
   begin
      Elements_Command_Pointers.Set_Sub_Data (Object.Buffer, Int (Offset), Data);
   end Set_Data;

end Orka.Buffers;
