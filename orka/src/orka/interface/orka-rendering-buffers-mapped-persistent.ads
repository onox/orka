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

package Orka.Rendering.Buffers.Mapped.Persistent is
   pragma Preelaborate;

   type Persistent_Mapped_Buffer is new Mapped_Buffer with private;

   function Create_Buffer
     (Kind    : Orka.Types.Element_Type;
      Length  : Positive;
      Mode    : IO_Mode;
      Regions : Positive) return Persistent_Mapped_Buffer
   with Post => Create_Buffer'Result.Length = Length;
   --  Create a persistent mapped buffer for only writes or only reads
   --
   --  The actual size of the buffer is n * Length where n is the number
   --  of regions. Each region has an index (>= 0).
   --
   --  After writing or reading, you must call Advance_Index (once per frame).
   --
   --  If Mode = Write, then you must wait for a fence to complete before
   --  writing and then set the fence after the drawing or dispatch commands
   --  which uses the mapped buffer.
   --
   --  If Mode = Read, then you must set a fence after the drawing or
   --  dispatch commands that write to the buffer and then wait for the
   --  fence to complete before reading the data.

   overriding
   function Length (Object : Persistent_Mapped_Buffer) return Positive;
   --  Number of elements in the buffer
   --
   --  Will be less than the actual size of the buffer due to the n regions.

   procedure Advance_Index (Object : in out Persistent_Mapped_Buffer);

private

   type Persistent_Mapped_Buffer is new Mapped_Buffer with record
      Index   : Natural  := 0;
      Regions : Positive := 1;
   end record
     with Type_Invariant => Index < Regions;

end Orka.Rendering.Buffers.Mapped.Persistent;
