--  Copyright (c) 2018 onox <denkpadje@gmail.com>
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

package body Orka.Rendering.Buffers.Mapped.Unsynchronized is

   function Create_Buffer
     (Kind   : Orka.Types.Element_Type;
      Length : Natural;
      Mode   : IO_Mode) return Unsynchronized_Mapped_Buffer
   is
      Storage_Flags : constant GL.Objects.Buffers.Storage_Bits :=
        (Write => Mode = Write, Read => Mode = Read, others => False);
   begin
      return Result : Unsynchronized_Mapped_Buffer (Kind, Mode) do
         Result.Buffer := Buffers.Create_Buffer (Storage_Flags, Kind, Length);
         Result.Offset := 0;
      end return;
   end Create_Buffer;

   procedure Map (Object : in out Unsynchronized_Mapped_Buffer) is
      Access_Flags : constant GL.Objects.Buffers.Access_Bits :=
        (Write => Object.Mode = Write, Read => Object.Mode = Read,
         Unsynchronized => True, Invalidate_Range => Object.Mode = Write,
         others => False);
   begin
      Object.Map (Size (Object.Length), Access_Flags);
   end Map;

   procedure Unmap (Object : in out Unsynchronized_Mapped_Buffer) is
   begin
      Object.Buffer.Buffer.Unmap;
   end Unmap;

   function Mapped (Object : in out Unsynchronized_Mapped_Buffer) return Boolean is
     (Object.Buffer.Buffer.Mapped);

   function Buffer (Object : in out Unsynchronized_Mapped_Buffer) return Buffers.Buffer is
     (Object.Buffer);

end Orka.Rendering.Buffers.Mapped.Unsynchronized;
