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

with Orka.Rendering.Buffers.Mapped.Unsynchronized;

package Orka.Rendering.Buffers.MDI is
   pragma Preelaborate;

   package UB renames Mapped.Unsynchronized;

   type Batch is tagged record
      --  Attributes
      Data : UB.Unsynchronized_Mapped_Buffer
        (Kind => Types.Half_Type,
         Mode => Mapped.Write);

      Indices   : UB.Unsynchronized_Mapped_Buffer
        (Kind => Types.UInt_Type,
         Mode => Mapped.Write);

      Commands  : UB.Unsynchronized_Mapped_Buffer
        (Kind => Types.Elements_Command_Type,
         Mode => Mapped.Write);

      Index_Offset  : Natural := 0;
      Vertex_Offset : Natural := 0;
      Index         : Natural := 0;
   end record;

   procedure Append
     (Object : in out Batch;
      Positions : not null Indirect.Half_Array_Access;
      Normals   : not null Indirect.Half_Array_Access;
      UVs       : not null Indirect.Half_Array_Access;
      Indices   : not null Indirect.UInt_Array_Access);

   function Create_Batch (Parts, Vertices, Indices : Positive) return Batch;

   procedure Finish_Batch (Object : in out Batch);

end Orka.Rendering.Buffers.MDI;
