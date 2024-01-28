--  SPDX-License-Identifier: Apache-2.0
--
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

with GL.Objects.Buffers;
with GL.Types.Indirect;

with Orka.Types;

package Orka.Rendering.Buffers is
   pragma Preelaborate;

   subtype Storage_Bits is GL.Objects.Buffers.Storage_Bits;

   use GL.Types;
   use all type Orka.Types.Element_Type;

   -----------------------------------------------------------------------------

   type Bindable_Buffer is interface;

   type Indexed_Buffer_Target is (Shader_Storage, Uniform);
   --  Buffer targets that can be read/written in shaders

   type Buffer_Target is
     (Index, Dispatch_Indirect, Draw_Indirect, Parameter, Pixel_Pack, Pixel_Unpack, Query);

   procedure Bind
     (Object : Bindable_Buffer;
      Target : Indexed_Buffer_Target;
      Index  : Natural) is abstract;
   --  Bind the buffer object to the binding point at the given index of
   --  the target

   procedure Bind
     (Object : Bindable_Buffer;
      Target : Buffer_Target) is abstract;
   --  Bind the buffer object to the target

   function Length (Object : Bindable_Buffer) return Positive is abstract;

   -----------------------------------------------------------------------------

   type Buffer (Kind : Types.Element_Type) is new Bindable_Buffer with private;

   overriding function "=" (Left, Right : Buffer) return Boolean;

   function Create_Buffer
     (Flags  : Storage_Bits;
      Kind   : Types.Element_Type;
      Length : Positive) return Buffer
   with Post => Create_Buffer'Result.Length = Length;

   -----------------------------------------------------------------------------

   function Create_Buffer
     (Flags  : Storage_Bits;
      Data   : Float_16_Array) return Buffer
   with Post => Create_Buffer'Result.Length = Data'Length;

   function Create_Buffer
     (Flags  : Storage_Bits;
      Data   : Float_32_Array) return Buffer
   with Post => Create_Buffer'Result.Length = Data'Length;

   function Create_Buffer
     (Flags  : Storage_Bits;
      Data   : Float_64_Array) return Buffer
   with Post => Create_Buffer'Result.Length = Data'Length;

   function Create_Buffer
     (Flags  : Storage_Bits;
      Data   : Integer_8_Array) return Buffer
   with Post => Create_Buffer'Result.Length = Data'Length;

   function Create_Buffer
     (Flags  : Storage_Bits;
      Data   : Integer_16_Array) return Buffer
   with Post => Create_Buffer'Result.Length = Data'Length;

   function Create_Buffer
     (Flags  : Storage_Bits;
      Data   : Integer_32_Array) return Buffer
   with Post => Create_Buffer'Result.Length = Data'Length;

   function Create_Buffer
     (Flags  : Storage_Bits;
      Data   : Unsigned_8_Array) return Buffer
   with Post => Create_Buffer'Result.Length = Data'Length;

   function Create_Buffer
     (Flags  : Storage_Bits;
      Data   : Unsigned_16_Array) return Buffer
   with Post => Create_Buffer'Result.Length = Data'Length;

   function Create_Buffer
     (Flags  : Storage_Bits;
      Data   : Unsigned_32_Array) return Buffer
   with Post => Create_Buffer'Result.Length = Data'Length;

   function Create_Buffer
     (Flags  : Storage_Bits;
      Data   : Orka.Types.Singles.Vector4_Array) return Buffer
   with Post => Create_Buffer'Result.Length = Data'Length;

   function Create_Buffer
     (Flags  : Storage_Bits;
      Data   : Orka.Types.Singles.Matrix4_Array) return Buffer
   with Post => Create_Buffer'Result.Length = Data'Length;

   function Create_Buffer
     (Flags  : Storage_Bits;
      Data   : Orka.Types.Doubles.Vector4_Array) return Buffer
   with Post => Create_Buffer'Result.Length = Data'Length;

   function Create_Buffer
     (Flags  : Storage_Bits;
      Data   : Orka.Types.Doubles.Matrix4_Array) return Buffer
   with Post => Create_Buffer'Result.Length = Data'Length;

   function Create_Buffer
     (Flags  : Storage_Bits;
      Data   : Indirect.Arrays_Indirect_Command_Array) return Buffer
   with Post => Create_Buffer'Result.Length = Data'Length;

   function Create_Buffer
     (Flags  : Storage_Bits;
      Data   : Indirect.Elements_Indirect_Command_Array) return Buffer
   with Post => Create_Buffer'Result.Length = Data'Length;

   function Create_Buffer
     (Flags  : Storage_Bits;
      Data   : Indirect.Dispatch_Indirect_Command_Array) return Buffer
   with Post => Create_Buffer'Result.Length = Data'Length;

   -----------------------------------------------------------------------------

   overriding
   function Length (Object : Buffer) return Positive
     with Inline;

   overriding
   procedure Bind (Object : Buffer; Target : Indexed_Buffer_Target; Index : Natural);
   --  Bind the buffer object to the binding point at the given index of
   --  the target

   overriding
   procedure Bind (Object : Buffer; Target : Buffer_Target);
   --  Bind the buffer object to the target

   -----------------------------------------------------------------------------

   procedure Set_Data
     (Object : Buffer;
      Data   : Float_16_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Half_Type and Offset + Data'Length <= Object.Length;

   procedure Set_Data
     (Object : Buffer;
      Data   : Float_32_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Single_Type and Offset + Data'Length <= Object.Length;

   procedure Set_Data
     (Object : Buffer;
      Data   : Float_64_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Double_Type and Offset + Data'Length <= Object.Length;

   procedure Set_Data
     (Object : Buffer;
      Data   : Integer_8_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Byte_Type and Offset + Data'Length <= Object.Length;

   procedure Set_Data
     (Object : Buffer;
      Data   : Integer_16_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Short_Type and Offset + Data'Length <= Object.Length;

   procedure Set_Data
     (Object : Buffer;
      Data   : Integer_32_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Int_Type and Offset + Data'Length <= Object.Length;

   procedure Set_Data
     (Object : Buffer;
      Data   : Unsigned_8_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = UByte_Type and Offset + Data'Length <= Object.Length;

   procedure Set_Data
     (Object : Buffer;
      Data   : Unsigned_16_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = UShort_Type and Offset + Data'Length <= Object.Length;

   procedure Set_Data
     (Object : Buffer;
      Data   : Unsigned_32_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = UInt_Type and Offset + Data'Length <= Object.Length;

   procedure Set_Data
     (Object : Buffer;
      Data   : Orka.Types.Singles.Vector4_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Single_Vector_Type and Offset + Data'Length <= Object.Length;

   procedure Set_Data
     (Object : Buffer;
      Data   : Orka.Types.Singles.Matrix4_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Single_Matrix_Type and Offset + Data'Length <= Object.Length;

   procedure Set_Data
     (Object : Buffer;
      Data   : Orka.Types.Doubles.Vector4_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Double_Vector_Type and Offset + Data'Length <= Object.Length;

   procedure Set_Data
     (Object : Buffer;
      Data   : Orka.Types.Doubles.Matrix4_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Double_Matrix_Type and Offset + Data'Length <= Object.Length;

   procedure Set_Data
     (Object : Buffer;
      Data   : Indirect.Arrays_Indirect_Command_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Arrays_Command_Type and Offset + Data'Length <= Object.Length;

   procedure Set_Data
     (Object : Buffer;
      Data   : Indirect.Elements_Indirect_Command_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Elements_Command_Type and Offset + Data'Length <= Object.Length;

   procedure Set_Data
     (Object : Buffer;
      Data   : Indirect.Dispatch_Indirect_Command_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Dispatch_Command_Type and Offset + Data'Length <= Object.Length;

   -----------------------------------------------------------------------------

   procedure Get_Data
     (Object : Buffer;
      Data   : in out Float_16_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Half_Type and Offset + Data'Length <= Object.Length;

   procedure Get_Data
     (Object : Buffer;
      Data   : in out Float_32_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Single_Type and Offset + Data'Length <= Object.Length;

   procedure Get_Data
     (Object : Buffer;
      Data   : in out Float_64_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Double_Type and Offset + Data'Length <= Object.Length;

   procedure Get_Data
     (Object : Buffer;
      Data   : in out Integer_8_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Byte_Type and Offset + Data'Length <= Object.Length;

   procedure Get_Data
     (Object : Buffer;
      Data   : in out Integer_16_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Int_Type and Offset + Data'Length <= Object.Length;

   procedure Get_Data
     (Object : Buffer;
      Data   : in out Integer_32_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Int_Type and Offset + Data'Length <= Object.Length;

   procedure Get_Data
     (Object : Buffer;
      Data   : in out Unsigned_8_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = UByte_Type and Offset + Data'Length <= Object.Length;

   procedure Get_Data
     (Object : Buffer;
      Data   : in out Unsigned_16_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = UShort_Type and Offset + Data'Length <= Object.Length;

   procedure Get_Data
     (Object : Buffer;
      Data   : in out Unsigned_32_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = UInt_Type and Offset + Data'Length <= Object.Length;

   -----------------------------------------------------------------------------

   procedure Get_Data
     (Object : Buffer;
      Data   : in out Orka.Types.Singles.Vector4_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Single_Vector_Type and Offset + Data'Length <= Object.Length;

   procedure Get_Data
     (Object : Buffer;
      Data   : in out Orka.Types.Singles.Matrix4_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Single_Matrix_Type and Offset + Data'Length <= Object.Length;

   procedure Get_Data
     (Object : Buffer;
      Data   : in out Orka.Types.Doubles.Vector4_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Double_Vector_Type and Offset + Data'Length <= Object.Length;

   procedure Get_Data
     (Object : Buffer;
      Data   : in out Orka.Types.Doubles.Matrix4_Array;
      Offset : Natural := 0)
   with Pre => Object.Kind = Double_Matrix_Type and Offset + Data'Length <= Object.Length;

   -----------------------------------------------------------------------------

   procedure Clear_Data
     (Object : Buffer;
      Data   : Integer_8_Array)
   with Pre => Object.Kind = Byte_Type
     and Data'Length in 1 .. 4
     and Object.Length mod Data'Length = 0;

   procedure Clear_Data
     (Object : Buffer;
      Data   : Integer_16_Array)
   with Pre => Object.Kind = Short_Type
     and Data'Length in 1 .. 4
     and Object.Length mod Data'Length = 0;

   procedure Clear_Data
     (Object : Buffer;
      Data   : Integer_32_Array)
   with Pre => Object.Kind = Int_Type
     and Data'Length in 1 .. 4
     and Object.Length mod Data'Length = 0;

   procedure Clear_Data
     (Object : Buffer;
      Data   : Unsigned_8_Array)
   with Pre => Object.Kind = UByte_Type
     and Data'Length in 1 .. 4
     and Object.Length mod Data'Length = 0;

   procedure Clear_Data
     (Object : Buffer;
      Data   : Unsigned_16_Array)
   with Pre => Object.Kind = UShort_Type
     and Data'Length in 1 .. 4
     and Object.Length mod Data'Length = 0;

   procedure Clear_Data
     (Object : Buffer;
      Data   : Unsigned_32_Array)
   with Pre => Object.Kind = UInt_Type
     and Data'Length in 1 .. 4
     and Object.Length mod Data'Length = 0;

   procedure Clear_Data
     (Object : Buffer;
      Data   : Float_32_Array)
   with Pre => Object.Kind = Single_Type
     and Data'Length in 1 .. 4
     and Object.Length mod Data'Length = 0;

   procedure Clear_Data
     (Object : Buffer;
      Data   : Orka.Types.Singles.Vector4)
   with Pre => Object.Kind = Single_Vector_Type
     or else (Object.Kind = Single_Type and Object.Length mod 4 = 0);

   -----------------------------------------------------------------------------

   procedure Copy_Data
     (Object : Buffer;
      Target : Buffer)
   with Pre => Object.Kind = Target.Kind and then Object.Length = Target.Length;

   procedure Copy_Data
     (Object       : Buffer;
      Target       : Buffer;
      Read_Offset  : Natural;
      Write_Offset : Natural;
      Length       : Positive)
   with Pre => Object.Kind = Target.Kind and then
     (Read_Offset + Length <= Object.Length and Write_Offset + Length <= Target.Length);

private

   type Buffer (Kind : Types.Element_Type) is new Bindable_Buffer with record
      Buffer : GL.Objects.Buffers.Buffer;
      Length : Positive;
   end record;

   use type GL.Objects.Buffers.Buffer;

   overriding function "=" (Left, Right : Buffer) return Boolean is (Left.Buffer = Right.Buffer);

end Orka.Rendering.Buffers;
