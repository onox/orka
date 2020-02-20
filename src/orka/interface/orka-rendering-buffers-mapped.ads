--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

private with Orka.Rendering.Buffers.Pointers;

package Orka.Rendering.Buffers.Mapped is
   pragma Preelaborate;

   type IO_Mode is (Read, Write);

   type Mapped_Buffer
     (Kind : Orka.Types.Element_Type;
      Mode : IO_Mode) is abstract new Bindable_Buffer with private;

   function GL_Buffer (Object : Mapped_Buffer) return GL.Objects.Buffers.Buffer
     with Inline;

   overriding
   function Length (Object : Mapped_Buffer) return Natural;
   --  Number of elements in the buffer

   -----------------------------------------------------------------------------

   overriding
   procedure Bind
     (Object : Mapped_Buffer;
      Target : Indexable_Buffer_Target;
      Index  : Natural);
   --  Bind the buffer object to the index of the target as well as to
   --  the target itself.

   overriding
   procedure Bind (Object : Mapped_Buffer; Target : Buffer_Target);
   --  Bind the buffer object to the target

   -----------------------------------------------------------------------------

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : UByte_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : UShort_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : UInt_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Byte_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Short_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Int_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Half_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Single_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Double_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   -----------------------------------------------------------------------------

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Orka.Types.Singles.Vector4_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Orka.Types.Singles.Matrix4_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Orka.Types.Doubles.Vector4_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Orka.Types.Doubles.Matrix4_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Indirect.Arrays_Indirect_Command_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Indirect.Elements_Indirect_Command_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Indirect.Dispatch_Indirect_Command_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   -----------------------------------------------------------------------------

   procedure Write_Data
     (Object : Mapped_Buffer;
      Value  : Orka.Types.Singles.Vector4;
      Offset : Natural)
   with Pre => Object.Mode = Write and Offset < Object.Length;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Value  : Orka.Types.Singles.Matrix4;
      Offset : Natural)
   with Pre => Object.Mode = Write and Offset < Object.Length;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Value  : Orka.Types.Doubles.Vector4;
      Offset : Natural)
   with Pre => Object.Mode = Write and Offset < Object.Length;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Value  : Orka.Types.Doubles.Matrix4;
      Offset : Natural)
   with Pre => Object.Mode = Write and Offset < Object.Length;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Value  : Indirect.Arrays_Indirect_Command;
      Offset : Natural)
   with Pre => Object.Mode = Write and Offset < Object.Length;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Value  : Indirect.Elements_Indirect_Command;
      Offset : Natural)
   with Pre => Object.Mode = Write and Offset < Object.Length;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Value  : Indirect.Dispatch_Indirect_Command;
      Offset : Natural)
   with Pre => Object.Mode = Write and Offset < Object.Length;

   -----------------------------------------------------------------------------

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out UByte_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Read and Offset + Data'Length <= Object.Length;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out UShort_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Read and Offset + Data'Length <= Object.Length;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out UInt_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Read and Offset + Data'Length <= Object.Length;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Byte_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Read and Offset + Data'Length <= Object.Length;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Short_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Read and Offset + Data'Length <= Object.Length;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Int_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Read and Offset + Data'Length <= Object.Length;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Half_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Read and Offset + Data'Length <= Object.Length;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Single_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Read and Offset + Data'Length <= Object.Length;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Double_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Read and Offset + Data'Length <= Object.Length;

   -----------------------------------------------------------------------------

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Orka.Types.Singles.Vector4_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Read and Offset + Data'Length <= Object.Length;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Orka.Types.Singles.Matrix4_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Read and Offset + Data'Length <= Object.Length;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Orka.Types.Doubles.Vector4_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Read and Offset + Data'Length <= Object.Length;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Orka.Types.Doubles.Matrix4_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Read and Offset + Data'Length <= Object.Length;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Indirect.Arrays_Indirect_Command_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Read and Offset + Data'Length <= Object.Length;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Indirect.Elements_Indirect_Command_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Read and Offset + Data'Length <= Object.Length;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Indirect.Dispatch_Indirect_Command_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Read and Offset + Data'Length <= Object.Length;

private

   use Orka.Types;

   type Mapped_Buffer
     (Kind : Orka.Types.Element_Type; Mode : IO_Mode)
   is new Bindable_Buffer with record
      Buffer : Buffers.Buffer (Kind);

      Offset : Natural;
      --  Offset in number of elements to the start of the buffer
      --
      --  Initially zero and incremented by Length whenever the index is
      --  advanced if the buffer is mapped persistent.

      case Kind is
         --  Numeric types
         when UByte_Type =>
            Pointer_UByte  : Pointers.UByte.Pointer;
         when UShort_Type =>
            Pointer_UShort : Pointers.UShort.Pointer;
         when UInt_Type =>
            Pointer_UInt   : Pointers.UInt.Pointer;
         when Byte_Type =>
            Pointer_Byte   : Pointers.Byte.Pointer;
         when Short_Type =>
            Pointer_Short  : Pointers.Short.Pointer;
         when Int_Type =>
            Pointer_Int    : Pointers.Int.Pointer;
         when Half_Type =>
            Pointer_Half   : Pointers.Half.Pointer;
         when Single_Type =>
            Pointer_Single : Pointers.Single.Pointer;
         when Double_Type =>
            Pointer_Double : Pointers.Double.Pointer;
         --  Composite types
         when Single_Vector_Type =>
            Pointer_SV : Pointers.Single_Vector4.Pointer;
         when Double_Vector_Type =>
            Pointer_DV : Pointers.Double_Vector4.Pointer;
         when Single_Matrix_Type =>
            Pointer_SM : Pointers.Single_Matrix4.Pointer;
         when Double_Matrix_Type =>
            Pointer_DM : Pointers.Double_Matrix4.Pointer;
         when Arrays_Command_Type =>
            Pointer_AC : Pointers.Arrays_Command.Pointer;
         when Elements_Command_Type =>
            Pointer_EC : Pointers.Elements_Command.Pointer;
         when Dispatch_Command_Type =>
            Pointer_DC : Pointers.Dispatch_Command.Pointer;
      end case;
   end record;

   procedure Map
     (Object : in out Mapped_Buffer;
      Length : Size;
      Flags  : GL.Objects.Buffers.Access_Bits);

end Orka.Rendering.Buffers.Mapped;
