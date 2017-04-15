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

with Orka.Buffers.Pointers;

package body Orka.Buffers is

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
      Kind   : Orka.Types.Composite_Type;
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
         Pointers.Half.Load_To_Immutable_Buffer (Result.Buffer, Data, Flags);
         Result.Length := Data'Length;
      end return;
   end Create_Buffer;

   function Create_Buffer
     (Flags  : GL.Objects.Buffers.Storage_Bits;
      Data   : Single_Array) return Buffer is
   begin
      return Result : Buffer do
         Pointers.Single.Load_To_Immutable_Buffer (Result.Buffer, Data, Flags);
         Result.Length := Data'Length;
      end return;
   end Create_Buffer;

   function Create_Buffer
     (Flags  : GL.Objects.Buffers.Storage_Bits;
      Data   : UInt_Array) return Buffer is
   begin
      return Result : Buffer do
         Pointers.UInt.Load_To_Immutable_Buffer (Result.Buffer, Data, Flags);
         Result.Length := Data'Length;
      end return;
   end Create_Buffer;

   function Create_Buffer
     (Flags  : GL.Objects.Buffers.Storage_Bits;
      Data   : Colors.Basic_Color_Array) return Buffer is
   begin
      return Result : Buffer do
         Pointers.Color.Load_To_Immutable_Buffer (Result.Buffer, Data, Flags);
         Result.Length := Data'Length;
      end return;
   end Create_Buffer;

   function Create_Buffer
     (Flags  : GL.Objects.Buffers.Storage_Bits;
      Data   : Singles.Matrix4_Array) return Buffer is
   begin
      return Result : Buffer do
         Pointers.Single_Matrix4.Load_To_Immutable_Buffer (Result.Buffer, Data, Flags);
         Result.Length := Data'Length;
      end return;
   end Create_Buffer;

   function Create_Buffer
     (Flags  : GL.Objects.Buffers.Storage_Bits;
      Data   : Indirect.Arrays_Indirect_Command_Array) return Buffer is
   begin
      return Result : Buffer do
         Pointers.Arrays_Command.Load_To_Immutable_Buffer (Result.Buffer, Data, Flags);
         Result.Length := Data'Length;
      end return;
   end Create_Buffer;

   function Create_Buffer
     (Flags  : GL.Objects.Buffers.Storage_Bits;
      Data   : Indirect.Elements_Indirect_Command_Array) return Buffer is
   begin
      return Result : Buffer do
         Pointers.Elements_Command.Load_To_Immutable_Buffer (Result.Buffer, Data, Flags);
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
      Pointers.Half.Set_Sub_Data (Object.Buffer, Int (Offset), Data);
   end Set_Data;

   procedure Set_Data
     (Object : Buffer;
      Data   : in out Single_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Single.Set_Sub_Data (Object.Buffer, Int (Offset), Data);
   end Set_Data;

   procedure Set_Data
     (Object : Buffer;
      Data   : in out UInt_Array;
      Offset : Natural := 0) is
   begin
      Pointers.UInt.Set_Sub_Data (Object.Buffer, Int (Offset), Data);
   end Set_Data;

   procedure Set_Data
     (Object : Buffer;
      Data   : in out Colors.Basic_Color_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Color.Set_Sub_Data (Object.Buffer, Int (Offset), Data);
   end Set_Data;

   procedure Set_Data
     (Object : Buffer;
      Data   : in out Singles.Matrix4_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Single_Matrix4.Set_Sub_Data (Object.Buffer, Int (Offset), Data);
   end Set_Data;

   procedure Set_Data
     (Object : Buffer;
      Data   : in out Indirect.Arrays_Indirect_Command_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Arrays_Command.Set_Sub_Data (Object.Buffer, Int (Offset), Data);
   end Set_Data;

   procedure Set_Data
     (Object : Buffer;
      Data   : in out Indirect.Elements_Indirect_Command_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Elements_Command.Set_Sub_Data (Object.Buffer, Int (Offset), Data);
   end Set_Data;

end Orka.Buffers;
