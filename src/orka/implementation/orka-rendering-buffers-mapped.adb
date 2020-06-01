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

package body Orka.Rendering.Buffers.Mapped is

   function GL_Buffer (Object : Mapped_Buffer) return GL.Objects.Buffers.Buffer is
     (Object.Buffer.GL_Buffer);

   overriding
   function Length (Object : Mapped_Buffer) return Natural is
     (Object.Buffer.Length);

   procedure Map
     (Object : in out Mapped_Buffer;
      Length : Size;
      Flags  : GL.Objects.Buffers.Access_Bits) is
   begin
      case Object.Kind is
         --  Numeric types
         when UByte_Type =>
            Pointers.UByte.Map_Range
              (Object.Buffer.Buffer, Flags, 0, Length, Object.Pointer_UByte);
         when UShort_Type =>
            Pointers.UShort.Map_Range
              (Object.Buffer.Buffer, Flags, 0, Length, Object.Pointer_UShort);
         when UInt_Type =>
            Pointers.UInt.Map_Range
              (Object.Buffer.Buffer, Flags, 0, Length, Object.Pointer_UInt);
         when Byte_Type =>
            Pointers.Byte.Map_Range
              (Object.Buffer.Buffer, Flags, 0, Length, Object.Pointer_Byte);
         when Short_Type =>
            Pointers.Short.Map_Range
              (Object.Buffer.Buffer, Flags, 0, Length, Object.Pointer_Short);
         when Int_Type =>
            Pointers.Int.Map_Range
              (Object.Buffer.Buffer, Flags, 0, Length, Object.Pointer_Int);
         when Half_Type =>
            Pointers.Half.Map_Range
              (Object.Buffer.Buffer, Flags, 0, Length, Object.Pointer_Half);
         when Single_Type =>
            Pointers.Single.Map_Range
              (Object.Buffer.Buffer, Flags, 0, Length, Object.Pointer_Single);
         when Double_Type =>
            Pointers.Double.Map_Range
              (Object.Buffer.Buffer, Flags, 0, Length, Object.Pointer_Double);
         --  Composite types
         when Single_Vector_Type =>
            Pointers.Single_Vector4.Map_Range
              (Object.Buffer.Buffer, Flags, 0, Length, Object.Pointer_SV);
         when Double_Vector_Type =>
            Pointers.Double_Vector4.Map_Range
              (Object.Buffer.Buffer, Flags, 0, Length, Object.Pointer_DV);
         when Single_Matrix_Type =>
            Pointers.Single_Matrix4.Map_Range
              (Object.Buffer.Buffer, Flags, 0, Length, Object.Pointer_SM);
         when Double_Matrix_Type =>
            Pointers.Double_Matrix4.Map_Range
              (Object.Buffer.Buffer, Flags, 0, Length, Object.Pointer_DM);
         when Arrays_Command_Type =>
            Pointers.Arrays_Command.Map_Range
              (Object.Buffer.Buffer, Flags, 0, Length, Object.Pointer_AC);
         when Elements_Command_Type =>
            Pointers.Elements_Command.Map_Range
              (Object.Buffer.Buffer, Flags, 0, Length, Object.Pointer_EC);
         when Dispatch_Command_Type =>
            Pointers.Dispatch_Command.Map_Range
              (Object.Buffer.Buffer, Flags, 0, Length, Object.Pointer_DC);
      end case;
   end Map;

   -----------------------------------------------------------------------------

   overriding
   procedure Bind
     (Object : Mapped_Buffer;
      Target : Indexed_Buffer_Target;
      Index  : Natural)
   is
      Buffer_Target : access constant GL.Objects.Buffers.Buffer_Target;

      Offset : constant Size := Size (Object.Offset);
      Length : constant Size := Size (Mapped_Buffer'Class (Object).Length);
   begin
      case Target is
         when Uniform =>
            Buffer_Target := GL.Objects.Buffers.Uniform_Buffer'Access;
         when Shader_Storage =>
            Buffer_Target := GL.Objects.Buffers.Shader_Storage_Buffer'Access;
         when Atomic_Counter =>
            Buffer_Target := GL.Objects.Buffers.Atomic_Counter_Buffer'Access;
      end case;

      case Object.Kind is
         --  Numeric types
         when UByte_Type =>
            Pointers.UByte.Bind_Range
              (Buffer_Target.all, Object.Buffer.Buffer, Index, Offset, Length);
         when UShort_Type =>
            Pointers.UShort.Bind_Range
              (Buffer_Target.all, Object.Buffer.Buffer, Index, Offset, Length);
         when UInt_Type =>
            Pointers.UInt.Bind_Range
              (Buffer_Target.all, Object.Buffer.Buffer, Index, Offset, Length);
         when Byte_Type =>
            Pointers.Byte.Bind_Range
              (Buffer_Target.all, Object.Buffer.Buffer, Index, Offset, Length);
         when Short_Type =>
            Pointers.Short.Bind_Range
              (Buffer_Target.all, Object.Buffer.Buffer, Index, Offset, Length);
         when Int_Type =>
            Pointers.Int.Bind_Range
              (Buffer_Target.all, Object.Buffer.Buffer, Index, Offset, Length);
         when Half_Type =>
            Pointers.Half.Bind_Range
              (Buffer_Target.all, Object.Buffer.Buffer, Index, Offset, Length);
         when Single_Type =>
            Pointers.Single.Bind_Range
              (Buffer_Target.all, Object.Buffer.Buffer, Index, Offset, Length);
         when Double_Type =>
            Pointers.Double.Bind_Range
              (Buffer_Target.all, Object.Buffer.Buffer, Index, Offset, Length);
         --  Composite types
         when Single_Vector_Type =>
            Pointers.Single_Vector4.Bind_Range
              (Buffer_Target.all, Object.Buffer.Buffer, Index, Offset, Length);
         when Double_Vector_Type =>
            Pointers.Double_Vector4.Bind_Range
              (Buffer_Target.all, Object.Buffer.Buffer, Index, Offset, Length);
         when Single_Matrix_Type =>
            Pointers.Single_Matrix4.Bind_Range
              (Buffer_Target.all, Object.Buffer.Buffer, Index, Offset, Length);
         when Double_Matrix_Type =>
            Pointers.Double_Matrix4.Bind_Range
              (Buffer_Target.all, Object.Buffer.Buffer, Index, Offset, Length);
         when Arrays_Command_Type =>
            Pointers.Arrays_Command.Bind_Range
              (Buffer_Target.all, Object.Buffer.Buffer, Index, Offset, Length);
         when Elements_Command_Type =>
            Pointers.Elements_Command.Bind_Range
              (Buffer_Target.all, Object.Buffer.Buffer, Index, Offset, Length);
         when Dispatch_Command_Type =>
            Pointers.Dispatch_Command.Bind_Range
              (Buffer_Target.all, Object.Buffer.Buffer, Index, Offset, Length);
      end case;
   end Bind;

   overriding
   procedure Bind (Object : Mapped_Buffer; Target : Buffer_Target) is
   begin
      case Target is
         when Index =>
            GL.Objects.Buffers.Element_Array_Buffer.Bind (Object.Buffer.Buffer);
         when Dispatch_Indirect =>
            GL.Objects.Buffers.Dispatch_Indirect_Buffer.Bind (Object.Buffer.Buffer);
         when Draw_Indirect =>
            GL.Objects.Buffers.Draw_Indirect_Buffer.Bind (Object.Buffer.Buffer);
         when Parameter =>
            GL.Objects.Buffers.Parameter_Buffer.Bind (Object.Buffer.Buffer);
         when Pixel_Pack =>
            GL.Objects.Buffers.Pixel_Pack_Buffer.Bind (Object.Buffer.Buffer);
         when Pixel_Unpack =>
            GL.Objects.Buffers.Pixel_Unpack_Buffer.Bind (Object.Buffer.Buffer);
         when Query =>
            GL.Objects.Buffers.Query_Buffer.Bind (Object.Buffer.Buffer);
      end case;
   end Bind;

   -----------------------------------------------------------------------------

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : UByte_Array;
      Offset : Natural := 0) is
   begin
      Pointers.UByte.Set_Mapped_Data
        (Object.Pointer_UByte, Size (Object.Offset + Offset), Data);
   end Write_Data;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : UShort_Array;
      Offset : Natural := 0) is
   begin
      Pointers.UShort.Set_Mapped_Data
        (Object.Pointer_UShort, Size (Object.Offset + Offset), Data);
   end Write_Data;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : UInt_Array;
      Offset : Natural := 0) is
   begin
      Pointers.UInt.Set_Mapped_Data
        (Object.Pointer_UInt, Size (Object.Offset + Offset), Data);
   end Write_Data;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Byte_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Byte.Set_Mapped_Data
        (Object.Pointer_Byte, Size (Object.Offset + Offset), Data);
   end Write_Data;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Short_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Short.Set_Mapped_Data
        (Object.Pointer_Short, Size (Object.Offset + Offset), Data);
   end Write_Data;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Int_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Int.Set_Mapped_Data
        (Object.Pointer_Int, Size (Object.Offset + Offset), Data);
   end Write_Data;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Half_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Half.Set_Mapped_Data
        (Object.Pointer_Half, Size (Object.Offset + Offset), Data);
   end Write_Data;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Single_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Single.Set_Mapped_Data
        (Object.Pointer_Single, Size (Object.Offset + Offset), Data);
   end Write_Data;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Double_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Double.Set_Mapped_Data
        (Object.Pointer_Double, Size (Object.Offset + Offset), Data);
   end Write_Data;

   -----------------------------------------------------------------------------

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Orka.Types.Singles.Vector4_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Single_Vector4.Set_Mapped_Data
        (Object.Pointer_SV, Size (Object.Offset + Offset), Data);
   end Write_Data;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Orka.Types.Singles.Matrix4_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Single_Matrix4.Set_Mapped_Data
        (Object.Pointer_SM, Size (Object.Offset + Offset), Data);
   end Write_Data;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Orka.Types.Doubles.Vector4_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Double_Vector4.Set_Mapped_Data
        (Object.Pointer_DV, Size (Object.Offset + Offset), Data);
   end Write_Data;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Orka.Types.Doubles.Matrix4_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Double_Matrix4.Set_Mapped_Data
        (Object.Pointer_DM, Size (Object.Offset + Offset), Data);
   end Write_Data;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Indirect.Arrays_Indirect_Command_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Arrays_Command.Set_Mapped_Data
        (Object.Pointer_AC, Size (Object.Offset + Offset), Data);
   end Write_Data;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Indirect.Elements_Indirect_Command_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Elements_Command.Set_Mapped_Data
        (Object.Pointer_EC, Size (Object.Offset + Offset), Data);
   end Write_Data;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Data   : Indirect.Dispatch_Indirect_Command_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Dispatch_Command.Set_Mapped_Data
        (Object.Pointer_DC, Size (Object.Offset + Offset), Data);
   end Write_Data;

   -----------------------------------------------------------------------------

   procedure Write_Data
     (Object : Mapped_Buffer;
      Value  : Orka.Types.Singles.Vector4;
      Offset : Natural) is
   begin
      Pointers.Single_Vector4.Set_Mapped_Data
        (Object.Pointer_SV, Size (Object.Offset + Offset), Value);
   end Write_Data;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Value  : Orka.Types.Singles.Matrix4;
      Offset : Natural) is
   begin
      Pointers.Single_Matrix4.Set_Mapped_Data
        (Object.Pointer_SM, Size (Object.Offset + Offset), Value);
   end Write_Data;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Value  : Orka.Types.Doubles.Vector4;
      Offset : Natural) is
   begin
      Pointers.Double_Vector4.Set_Mapped_Data
        (Object.Pointer_DV, Size (Object.Offset + Offset), Value);
   end Write_Data;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Value  : Orka.Types.Doubles.Matrix4;
      Offset : Natural) is
   begin
      Pointers.Double_Matrix4.Set_Mapped_Data
        (Object.Pointer_DM, Size (Object.Offset + Offset), Value);
   end Write_Data;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Value  : Indirect.Arrays_Indirect_Command;
      Offset : Natural) is
   begin
      Pointers.Arrays_Command.Set_Mapped_Data
        (Object.Pointer_AC, Size (Object.Offset + Offset), Value);
   end Write_Data;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Value  : Indirect.Elements_Indirect_Command;
      Offset : Natural) is
   begin
      Pointers.Elements_Command.Set_Mapped_Data
        (Object.Pointer_EC, Size (Object.Offset + Offset), Value);
   end Write_Data;

   procedure Write_Data
     (Object : Mapped_Buffer;
      Value  : Indirect.Dispatch_Indirect_Command;
      Offset : Natural) is
   begin
      Pointers.Dispatch_Command.Set_Mapped_Data
        (Object.Pointer_DC, Size (Object.Offset + Offset), Value);
   end Write_Data;

   -----------------------------------------------------------------------------

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out UByte_Array;
      Offset : Natural := 0) is
   begin
      Data := Pointers.UByte.Get_Mapped_Data
        (Object.Pointer_UByte, Size (Object.Offset + Offset), Data'Length);
   end Read_Data;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out UShort_Array;
      Offset : Natural := 0) is
   begin
      Data := Pointers.UShort.Get_Mapped_Data
        (Object.Pointer_UShort, Size (Object.Offset + Offset), Data'Length);
   end Read_Data;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out UInt_Array;
      Offset : Natural := 0) is
   begin
      Data := Pointers.UInt.Get_Mapped_Data
        (Object.Pointer_UInt, Size (Object.Offset + Offset), Data'Length);
   end Read_Data;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Byte_Array;
      Offset : Natural := 0) is
   begin
      Data := Pointers.Byte.Get_Mapped_Data
        (Object.Pointer_Byte, Size (Object.Offset + Offset), Data'Length);
   end Read_Data;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Short_Array;
      Offset : Natural := 0) is
   begin
      Data := Pointers.Short.Get_Mapped_Data
        (Object.Pointer_Short, Size (Object.Offset + Offset), Data'Length);
   end Read_Data;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Int_Array;
      Offset : Natural := 0) is
   begin
      Data := Pointers.Int.Get_Mapped_Data
        (Object.Pointer_Int, Size (Object.Offset + Offset), Data'Length);
   end Read_Data;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Half_Array;
      Offset : Natural := 0) is
   begin
      Data := Pointers.Half.Get_Mapped_Data
        (Object.Pointer_Half, Size (Object.Offset + Offset), Data'Length);
   end Read_Data;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Single_Array;
      Offset : Natural := 0) is
   begin
      Data := Pointers.Single.Get_Mapped_Data
        (Object.Pointer_Single, Size (Object.Offset + Offset), Data'Length);
   end Read_Data;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Double_Array;
      Offset : Natural := 0) is
   begin
      Data := Pointers.Double.Get_Mapped_Data
        (Object.Pointer_Double, Size (Object.Offset + Offset), Data'Length);
   end Read_Data;

   -----------------------------------------------------------------------------

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Orka.Types.Singles.Vector4_Array;
      Offset : Natural := 0) is
   begin
      Data := Pointers.Single_Vector4.Get_Mapped_Data
        (Object.Pointer_SV, Size (Object.Offset + Offset), Data'Length);
   end Read_Data;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Orka.Types.Singles.Matrix4_Array;
      Offset : Natural := 0) is
   begin
      Data := Pointers.Single_Matrix4.Get_Mapped_Data
        (Object.Pointer_SM, Size (Object.Offset + Offset), Data'Length);
   end Read_Data;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Orka.Types.Doubles.Vector4_Array;
      Offset : Natural := 0) is
   begin
      Data := Pointers.Double_Vector4.Get_Mapped_Data
        (Object.Pointer_DV, Size (Object.Offset + Offset), Data'Length);
   end Read_Data;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Orka.Types.Doubles.Matrix4_Array;
      Offset : Natural := 0) is
   begin
      Data := Pointers.Double_Matrix4.Get_Mapped_Data
        (Object.Pointer_DM, Size (Object.Offset + Offset), Data'Length);
   end Read_Data;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Indirect.Arrays_Indirect_Command_Array;
      Offset : Natural := 0) is
   begin
      Data := Pointers.Arrays_Command.Get_Mapped_Data
        (Object.Pointer_AC, Size (Object.Offset + Offset), Data'Length);
   end Read_Data;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Indirect.Elements_Indirect_Command_Array;
      Offset : Natural := 0) is
   begin
      Data := Pointers.Elements_Command.Get_Mapped_Data
        (Object.Pointer_EC, Size (Object.Offset + Offset), Data'Length);
   end Read_Data;

   procedure Read_Data
     (Object : Mapped_Buffer;
      Data   : out Indirect.Dispatch_Indirect_Command_Array;
      Offset : Natural := 0) is
   begin
      Data := Pointers.Dispatch_Command.Get_Mapped_Data
        (Object.Pointer_DC, Size (Object.Offset + Offset), Data'Length);
   end Read_Data;

end Orka.Rendering.Buffers.Mapped;
