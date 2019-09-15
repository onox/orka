--  SPDX-License-Identifier: Apache-2.0
--
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

package Orka.Rendering.Buffers.Mapped.Unsynchronized is
   pragma Preelaborate;

   type Unsynchronized_Mapped_Buffer is new Mapped_Buffer with private;

   function Create_Buffer
     (Kind   : Orka.Types.Element_Type;
      Length : Natural;
      Mode   : IO_Mode) return Unsynchronized_Mapped_Buffer
   with Post => Create_Buffer'Result.Length = Length;

   procedure Map (Object : in out Unsynchronized_Mapped_Buffer)
     with Pre  => not Object.Mapped,
          Post => Object.Mapped;

   procedure Unmap (Object : in out Unsynchronized_Mapped_Buffer)
     with Pre  => Object.Mapped,
          Post => not Object.Mapped;

   function Mapped (Object : in out Unsynchronized_Mapped_Buffer) return Boolean;

   function Buffer (Object : in out Unsynchronized_Mapped_Buffer) return Buffers.Buffer
     with Pre => not Object.Mapped;

private

   type Unsynchronized_Mapped_Buffer is new Mapped_Buffer with null record;

end Orka.Rendering.Buffers.Mapped.Unsynchronized;
