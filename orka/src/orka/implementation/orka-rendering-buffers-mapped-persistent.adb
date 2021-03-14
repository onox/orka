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

package body Orka.Rendering.Buffers.Mapped.Persistent is

   function Create_Buffer
     (Kind   : Orka.Types.Element_Type;
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
         Result.Offset := Length * Natural (Result.Index);

         Result.Map (Size (Total_Length), Access_Flags);
      end return;
   end Create_Buffer;

   overriding
   function Length (Object : Persistent_Mapped_Buffer) return Natural is
     (Object.Buffer.Length / Index_Type'Modulus);

   procedure Advance_Index (Object : in out Persistent_Mapped_Buffer) is
   begin
      Object.Index  := Object.Index + 1;
      Object.Offset := Object.Length * Natural (Object.Index);
   end Advance_Index;

end Orka.Rendering.Buffers.Mapped.Persistent;
