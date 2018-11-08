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

package body Orka.Rendering.Buffers.Persistent_Mapped is

   function Create_Buffer
     (Kind   : Orka.Types.Composite_Type;
      Length : Natural;
      Mode   : IO_Mode) return Persistent_Mapped_Buffer
   is
      Storage_Flags : constant GL.Objects.Buffers.Storage_Bits :=
        (Write => Mode = Write, Read => Mode = Read,
         Persistent => True, Coherent => True, others => False);
      Access_Flags : constant GL.Objects.Buffers.Access_Bits :=
        (Write => Mode = Write, Read => Mode = Read,
         Persistent => True, Coherent => True, others => False);

      Total_Length : constant Natural := Length * Index_Type'Modulus;
   begin
      return Result : Persistent_Mapped_Buffer (Kind, Mode) do
         Result.Buffer := Buffers.Create_Buffer (Storage_Flags, Kind, Total_Length);
         Result.Index  := Index_Type'First;

         Result.Map (Kind, Total_Length, Access_Flags);
      end return;
   end Create_Buffer;

   function GL_Buffer (Object : Persistent_Mapped_Buffer) return GL.Objects.Buffers.Buffer is
     (Object.Buffer.GL_Buffer);

   function Length (Object : Persistent_Mapped_Buffer) return Natural is
     (Object.Buffer.Length / Index_Type'Modulus);

   function Index_Offset (Object : Persistent_Mapped_Buffer) return Natural is
     (Object.Length * Natural (Object.Index));

   procedure Advance_Index (Object : in out Persistent_Mapped_Buffer) is
   begin
      Object.Index := Object.Index + 1;
   end Advance_Index;

   procedure Map
     (Object : in out Persistent_Mapped_Buffer;
      Kind   : Orka.Types.Composite_Type;
      Length : Natural;
      Access_Flags : GL.Objects.Buffers.Access_Bits) is
   begin
      case Kind is
         when Single_Vector_Type =>
            Pointers.Single_Vector4.Map_Range
              (Object.Buffer.Buffer, Access_Flags, 0, Size (Length), Object.Pointer_SV);
         when Double_Vector_Type =>
            Pointers.Double_Vector4.Map_Range
              (Object.Buffer.Buffer, Access_Flags, 0, Size (Length), Object.Pointer_DV);
         when Single_Matrix_Type =>
            Pointers.Single_Matrix4.Map_Range
              (Object.Buffer.Buffer, Access_Flags, 0, Size (Length), Object.Pointer_SM);
         when Double_Matrix_Type =>
            Pointers.Double_Matrix4.Map_Range
              (Object.Buffer.Buffer, Access_Flags, 0, Size (Length), Object.Pointer_DM);
         when Arrays_Command_Type =>
            Pointers.Arrays_Command.Map_Range
              (Object.Buffer.Buffer, Access_Flags, 0, Size (Length), Object.Pointer_AC);
         when Elements_Command_Type =>
            Pointers.Elements_Command.Map_Range
              (Object.Buffer.Buffer, Access_Flags, 0, Size (Length), Object.Pointer_EC);
         when Dispatch_Command_Type =>
            Pointers.Dispatch_Command.Map_Range
              (Object.Buffer.Buffer, Access_Flags, 0, Size (Length), Object.Pointer_DC);
      end case;
   end Map;

   -----------------------------------------------------------------------------

   overriding
   procedure Bind_Base
     (Object : Persistent_Mapped_Buffer;
      Target : Buffer_Target;
      Index  : Natural)
   is
      Buffer_Target : access constant GL.Objects.Buffers.Buffer_Target;

      Offset : constant Size := Size (Object.Index_Offset);
      Length : constant Size := Size (Object.Length);
   begin
      case Target is
         when Uniform =>
            Buffer_Target := GL.Objects.Buffers.Uniform_Buffer'Access;
         when Transform_Feedback =>
            Buffer_Target := GL.Objects.Buffers.Transform_Feedback_Buffer'Access;
         when Shader_Storage =>
            Buffer_Target := GL.Objects.Buffers.Shader_Storage_Buffer'Access;
         when Atomic_Counter =>
            Buffer_Target := GL.Objects.Buffers.Atomic_Counter_Buffer'Access;
      end case;

      case Object.Kind is
         when Single_Vector_Type =>
            Pointers.Single_Vector4.Bind_Range
              (Buffer_Target.all, Object.GL_Buffer, Index, Offset, Length);
         when Double_Vector_Type =>
            Pointers.Double_Vector4.Bind_Range
              (Buffer_Target.all, Object.GL_Buffer, Index, Offset, Length);
         when Single_Matrix_Type =>
            Pointers.Single_Matrix4.Bind_Range
              (Buffer_Target.all, Object.GL_Buffer, Index, Offset, Length);
         when Double_Matrix_Type =>
            Pointers.Double_Matrix4.Bind_Range
              (Buffer_Target.all, Object.GL_Buffer, Index, Offset, Length);
         when Arrays_Command_Type =>
            Pointers.Arrays_Command.Bind_Range
              (Buffer_Target.all, Object.GL_Buffer, Index, Offset, Length);
         when Elements_Command_Type =>
            Pointers.Elements_Command.Bind_Range
              (Buffer_Target.all, Object.GL_Buffer, Index, Offset, Length);
         when Dispatch_Command_Type =>
            Pointers.Dispatch_Command.Bind_Range
              (Buffer_Target.all, Object.GL_Buffer, Index, Offset, Length);
      end case;
   end Bind_Base;

   -----------------------------------------------------------------------------

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Data   : Orka.Types.Singles.Vector4_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Single_Vector4.Set_Mapped_Data
        (Object.Pointer_SV, Size (Object.Index_Offset + Offset), Data);
   end Write_Data;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Data   : Orka.Types.Singles.Matrix4_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Single_Matrix4.Set_Mapped_Data
        (Object.Pointer_SM, Size (Object.Index_Offset + Offset), Data);
   end Write_Data;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Data   : Orka.Types.Doubles.Vector4_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Double_Vector4.Set_Mapped_Data
        (Object.Pointer_DV, Size (Object.Index_Offset + Offset), Data);
   end Write_Data;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Data   : Orka.Types.Doubles.Matrix4_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Double_Matrix4.Set_Mapped_Data
        (Object.Pointer_DM, Size (Object.Index_Offset + Offset), Data);
   end Write_Data;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Data   : Indirect.Arrays_Indirect_Command_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Arrays_Command.Set_Mapped_Data
        (Object.Pointer_AC, Size (Object.Index_Offset + Offset), Data);
   end Write_Data;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Data   : Indirect.Elements_Indirect_Command_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Elements_Command.Set_Mapped_Data
        (Object.Pointer_EC, Size (Object.Index_Offset + Offset), Data);
   end Write_Data;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Data   : Indirect.Dispatch_Indirect_Command_Array;
      Offset : Natural := 0) is
   begin
      Pointers.Dispatch_Command.Set_Mapped_Data
        (Object.Pointer_DC, Size (Object.Index_Offset + Offset), Data);
   end Write_Data;

   -----------------------------------------------------------------------------

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Value  : Orka.Types.Singles.Vector4;
      Offset : Natural) is
   begin
      Pointers.Single_Vector4.Set_Mapped_Data
        (Object.Pointer_SV, Size (Object.Index_Offset + Offset), Value);
   end Write_Data;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Value  : Orka.Types.Singles.Matrix4;
      Offset : Natural) is
   begin
      Pointers.Single_Matrix4.Set_Mapped_Data
        (Object.Pointer_SM, Size (Object.Index_Offset + Offset), Value);
   end Write_Data;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Value  : Orka.Types.Doubles.Vector4;
      Offset : Natural) is
   begin
      Pointers.Double_Vector4.Set_Mapped_Data
        (Object.Pointer_DV, Size (Object.Index_Offset + Offset), Value);
   end Write_Data;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Value  : Orka.Types.Doubles.Matrix4;
      Offset : Natural) is
   begin
      Pointers.Double_Matrix4.Set_Mapped_Data
        (Object.Pointer_DM, Size (Object.Index_Offset + Offset), Value);
   end Write_Data;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Value  : Indirect.Arrays_Indirect_Command;
      Offset : Natural) is
   begin
      Pointers.Arrays_Command.Set_Mapped_Data
        (Object.Pointer_AC, Size (Object.Index_Offset + Offset), Value);
   end Write_Data;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Value  : Indirect.Elements_Indirect_Command;
      Offset : Natural) is
   begin
      Pointers.Elements_Command.Set_Mapped_Data
        (Object.Pointer_EC, Size (Object.Index_Offset + Offset), Value);
   end Write_Data;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Value  : Indirect.Dispatch_Indirect_Command;
      Offset : Natural) is
   begin
      Pointers.Dispatch_Command.Set_Mapped_Data
        (Object.Pointer_DC, Size (Object.Index_Offset + Offset), Value);
   end Write_Data;

end Orka.Rendering.Buffers.Persistent_Mapped;
