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

   type Batch
     (Vertex_Kind : Types.Numeric_Type;
      Index_Kind  : Types.Index_Type)
   is tagged record
      --  Attributes
      Data : UB.Unsynchronized_Mapped_Buffer
        (Kind => Vertex_Kind,
         Mode => Mapped.Write);

      Indices : UB.Unsynchronized_Mapped_Buffer
        (Kind => Index_Kind,
         Mode => Mapped.Write);

      Commands : UB.Unsynchronized_Mapped_Buffer
        (Kind => Types.Elements_Command_Type,
         Mode => Mapped.Write);

      Index_Offset   : Natural := 0;
      Vertex_Offset  : Natural := 0;
      Draw_Index     : Natural := 0;
      Instance_Index : Natural := 0;
   end record;

   procedure Append
     (Object    : in out Batch;
      Instances : Natural;
      Vertices  : Natural;
      Indices   : Natural;
      Append_Vertices : not null access procedure (Offset, Count : Natural);
      Append_Indices  : not null access procedure (Offset, Count : Natural));

   function Create_Batch
     (Vertex_Kind : Types.Numeric_Type;
      Index_Kind  : Types.Index_Type;
      Parts, Vertex_Data, Indices : Positive) return Batch;

   procedure Finish_Batch (Object : in out Batch);

   -----------------------------------------------------------------------------

   function Create_Batch (Parts, Vertices, Indices : Positive) return Batch
     with Post => Create_Batch'Result.Vertex_Kind = Types.Half_Type and
                  Create_Batch'Result.Index_Kind  = Types.UInt_Type;

   procedure Append
     (Object : in out Batch;
      Positions : not null Indirect.Half_Array_Access;
      Normals   : not null Indirect.Half_Array_Access;
      UVs       : not null Indirect.Half_Array_Access;
      Indices   : not null Indirect.UInt_Array_Access)
   with Pre => Object.Vertex_Kind = Types.Half_Type and Object.Index_Kind = Types.UInt_Type;

end Orka.Rendering.Buffers.MDI;
