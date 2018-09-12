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

with Orka.Rendering.Buffers.Pointers;

package body Orka.Rendering.Buffers is

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
            return Result : Buffer := Create_Buffer (Flags, Single_Type, Length * 4) do
               Result.Length := Length;
            end return;
         when Double_Vector_Type =>
            return Result : Buffer := Create_Buffer (Flags, Double_Type, Length * 4) do
               Result.Length := Length;
            end return;
         when Single_Matrix_Type =>
            return Result : Buffer := Create_Buffer (Flags, Single_Type, Length * 16) do
               Result.Length := Length;
            end return;
         when Double_Matrix_Type =>
            return Result : Buffer := Create_Buffer (Flags, Double_Type, Length * 16) do
               Result.Length := Length;
            end return;
         when Arrays_Command_Type =>
            Bytes := Indirect.Arrays_Indirect_Command'Size / System.Storage_Unit;
            return Result : Buffer := Create_Buffer (Flags, Byte_Type, Length * Bytes) do
               Result.Length := Length;
            end return;
         when Elements_Command_Type =>
            Bytes := Indirect.Elements_Indirect_Command'Size / System.Storage_Unit;
            return Result : Buffer := Create_Buffer (Flags, Byte_Type, Length * Bytes) do
               Result.Length := Length;
            end return;
         when Dispatch_Command_Type =>
            Bytes := Indirect.Dispatch_Indirect_Command'Size / System.Storage_Unit;
            return Result : Buffer := Create_Buffer (Flags, Byte_Type, Length * Bytes) do
               Result.Length := Length;
            end return;
      end case;
   end Create_Buffer;

   -----------------------------------------------------------------------------

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
      Data   : Orka.Types.Singles.Vector4_Array) return Buffer is
   begin
      return Result : Buffer do
         Pointers.Single_Vector4.Load_To_Immutable_Buffer (Result.Buffer, Data, Flags);
         Result.Length := Data'Length;
      end return;
   end Create_Buffer;

   function Create_Buffer
     (Flags  : GL.Objects.Buffers.Storage_Bits;
      Data   : Orka.Types.Singles.Matrix4_Array) return Buffer is
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

   function Create_Buffer
     (Flags  : GL.Objects.Buffers.Storage_Bits;
      Data   : Indirect.Dispatch_Indirect_Command_Array) return Buffer is
   begin
      return Result : Buffer do
         Pointers.Dispatch_Command.Load_To_Immutable_Buffer (Result.Buffer, Data, Flags);
         Result.Length := Data'Length;
      end return;
   end Create_Buffer;

   -----------------------------------------------------------------------------

   function GL_Buffer (Object : Buffer) return GL.Objects.Buffers.Buffer
     is (Object.Buffer);

   function Length (Object : Buffer) return Natural
     is (Object.Length);

   -----------------------------------------------------------------------------

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
      Data   : in out Int_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Int.Set_Sub_Data (Object.Buffer, Int (Offset), Data);
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
      Data   : in out Orka.Types.Singles.Vector4_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Single_Vector4.Set_Sub_Data (Object.Buffer, Int (Offset), Data);
   end Set_Data;

   procedure Set_Data
     (Object : Buffer;
      Data   : in out Orka.Types.Singles.Matrix4_Array;
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

   procedure Set_Data
     (Object : Buffer;
      Data   : in out Indirect.Dispatch_Indirect_Command_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Dispatch_Command.Set_Sub_Data (Object.Buffer, Int (Offset), Data);
   end Set_Data;

   -----------------------------------------------------------------------------

   procedure Get_Data
     (Object : Buffer;
      Data   : in out Half_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Half.Get_Sub_Data (Object.Buffer, Int (Offset), Data);
   end Get_Data;

   procedure Get_Data
     (Object : Buffer;
      Data   : in out Single_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Single.Get_Sub_Data (Object.Buffer, Int (Offset), Data);
   end Get_Data;

   procedure Get_Data
     (Object : Buffer;
      Data   : in out Int_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Int.Get_Sub_Data (Object.Buffer, Int (Offset), Data);
   end Get_Data;

   procedure Get_Data
     (Object : Buffer;
      Data   : in out UInt_Array;
      Offset : Natural := 0) is
   begin
      Pointers.UInt.Get_Sub_Data (Object.Buffer, Int (Offset), Data);
   end Get_Data;

   -----------------------------------------------------------------------------

   procedure Copy_Data
     (Object : Buffer;
      Target : Buffer;
      Kind   : Numeric_Type) is
   begin
      case Kind is
         when Byte_Type =>
            raise Program_Error with "Not implemented yet";
         when UByte_Type =>
            raise Program_Error with "Not implemented yet";
         when Short_Type =>
            raise Program_Error with "Not implemented yet";
         when UShort_Type =>
            raise Program_Error with "Not implemented yet";
         when Int_Type =>
            Pointers.Int.Copy_Sub_Data (Object.Buffer, Target.Buffer, 0, 0, Size (Object.Length));
         when UInt_Type =>
            Pointers.UInt.Copy_Sub_Data (Object.Buffer, Target.Buffer, 0, 0, Size (Object.Length));
         when Half_Type =>
            Pointers.Half.Copy_Sub_Data (Object.Buffer, Target.Buffer, 0, 0, Size (Object.Length));
         when Single_Type =>
            Pointers.Single.Copy_Sub_Data (Object.Buffer, Target.Buffer, 0, 0, Size (Object.Length));
         when Double_Type =>
            Pointers.Double.Copy_Sub_Data (Object.Buffer, Target.Buffer, 0, 0, Size (Object.Length));
      end case;
   end Copy_Data;

end Orka.Rendering.Buffers;
