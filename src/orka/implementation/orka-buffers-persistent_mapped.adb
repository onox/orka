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

package body Orka.Buffers.Persistent_Mapped is

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

         case Kind is
            when Single_Vector_Type =>
               Pointers.Single_Vector4.Map_Range
                 (Result.Buffer.Buffer, Access_Flags, 0, Size (Total_Length), Result.Pointer_SV);
            when Double_Vector_Type =>
               Pointers.Double_Vector4.Map_Range
                 (Result.Buffer.Buffer, Access_Flags, 0, Size (Total_Length), Result.Pointer_DV);
            when Single_Matrix_Type =>
               Pointers.Single_Matrix4.Map_Range
                 (Result.Buffer.Buffer, Access_Flags, 0, Size (Total_Length), Result.Pointer_SM);
            when Double_Matrix_Type =>
               Pointers.Double_Matrix4.Map_Range
                 (Result.Buffer.Buffer, Access_Flags, 0, Size (Total_Length), Result.Pointer_DM);
            when Arrays_Command_Type =>
               Pointers.Arrays_Command.Map_Range
                 (Result.Buffer.Buffer, Access_Flags, 0, Size (Total_Length), Result.Pointer_AC);
            when Elements_Command_Type =>
               Pointers.Elements_Command.Map_Range
                 (Result.Buffer.Buffer, Access_Flags, 0, Size (Total_Length), Result.Pointer_EC);
         end case;
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

end Orka.Buffers.Persistent_Mapped;
