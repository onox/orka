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

with Interfaces.C.Pointers;

with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Orka.Smart_Pointers;

package Orka.Resources is
   pragma Preelaborate;

   type Resource is limited interface;

   type Resource_Ptr is not null access all Resource'Class;

   -----------------------------------------------------------------------------

   subtype Byte_Array is Ada.Streams.Stream_Element_Array;

   type Byte_Array_Access is access Byte_Array;

   function Convert (Bytes : Byte_Array) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Byte_Array, Name => Byte_Array_Access);

   package Byte_Array_Pointers is new Orka.Smart_Pointers
     (Byte_Array, Byte_Array_Access, Free);

   Resource_Load_Error : exception;

   package SU renames Ada.Strings.Unbounded;

   package Byte_Pointers is new Interfaces.C.Pointers
     (Ada.Streams.Stream_Element_Offset, Ada.Streams.Stream_Element,
      Byte_Array, Ada.Streams.Stream_Element'Last);

end Orka.Resources;
