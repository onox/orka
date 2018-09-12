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

generic
   type Index_Type is mod <>;
package Orka.Rendering.Buffers.Persistent_Mapped is
   pragma Preelaborate;

   type IO_Mode is (Read, Write);

   type Persistent_Mapped_Buffer
     (Kind : Orka.Types.Composite_Type; Mode : IO_Mode) is tagged private;

   function Create_Buffer
     (Kind   : Orka.Types.Composite_Type;
      Length : Natural;
      Mode   : IO_Mode) return Persistent_Mapped_Buffer
   with Post => Create_Buffer'Result.Length = Length;
   --  Create a persistent mapped buffer for only writes or only reads
   --
   --  The actual size of the buffer is n * Length where n is the number
   --  of regions. Each region has an index (>= 0).
   --
   --  After writing or reading, you must call Advance_Index (once per frame).

   function GL_Buffer (Object : Persistent_Mapped_Buffer) return GL.Objects.Buffers.Buffer
     with Inline;

   function Length (Object : Persistent_Mapped_Buffer) return Natural
     with Inline;
   --  Number of elements in the buffer
   --
   --  Will be less than the actual size of the buffer due to triple
   --  buffering.

   function Index_Offset (Object : Persistent_Mapped_Buffer) return Natural
     with Inline;
   --  Offset in number of elements to the start of the buffer
   --
   --  Initially zero and incremented by Length whenever the index is
   --  advanced.

   procedure Advance_Index (Object : in out Persistent_Mapped_Buffer);

   -----------------------------------------------------------------------------

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Data   : Orka.Types.Singles.Vector4_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Data   : Orka.Types.Singles.Matrix4_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Data   : Orka.Types.Doubles.Vector4_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Data   : Orka.Types.Doubles.Matrix4_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Data   : Indirect.Arrays_Indirect_Command_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Data   : Indirect.Elements_Indirect_Command_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Data   : Indirect.Dispatch_Indirect_Command_Array;
      Offset : Natural := 0)
   with Pre => Object.Mode = Write and Offset + Data'Length <= Object.Length;

   -----------------------------------------------------------------------------

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Value  : Orka.Types.Singles.Vector4;
      Offset : Natural)
   with Pre => Object.Mode = Write and Offset < Object.Length;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Value  : Orka.Types.Singles.Matrix4;
      Offset : Natural)
   with Pre => Object.Mode = Write and Offset < Object.Length;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Value  : Orka.Types.Doubles.Vector4;
      Offset : Natural)
   with Pre => Object.Mode = Write and Offset < Object.Length;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Value  : Orka.Types.Doubles.Matrix4;
      Offset : Natural)
   with Pre => Object.Mode = Write and Offset < Object.Length;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Value  : Indirect.Arrays_Indirect_Command;
      Offset : Natural)
   with Pre => Object.Mode = Write and Offset < Object.Length;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Value  : Indirect.Elements_Indirect_Command;
      Offset : Natural)
   with Pre => Object.Mode = Write and Offset < Object.Length;

   procedure Write_Data
     (Object : Persistent_Mapped_Buffer;
      Value  : Indirect.Dispatch_Indirect_Command;
      Offset : Natural)
   with Pre => Object.Mode = Write and Offset < Object.Length;

private

   use Orka.Types;

   type Persistent_Mapped_Buffer
     (Kind : Orka.Types.Composite_Type; Mode : IO_Mode)
   is tagged record
      Buffer : Buffers.Buffer;
      Index  : Index_Type;
      case Kind is
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

end Orka.Rendering.Buffers.Persistent_Mapped;
