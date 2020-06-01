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

package body Orka.Rendering.Buffers.MDI is

   procedure Append
     (Object : in out Batch;
      Positions : not null Indirect.Half_Array_Access;
      Normals   : not null Indirect.Half_Array_Access;
      UVs       : not null Indirect.Half_Array_Access;
      Indices   : not null Indirect.UInt_Array_Access)
   is
      Index_Count  : constant Natural := Indices'Length;
      Vertex_Count : constant Natural := Positions'Length / 3;

      pragma Assert (Positions'Length = Normals'Length);
      pragma Assert (Vertex_Count = UVs'Length / 2);

      Commands : Indirect.Elements_Indirect_Command_Array (1 .. 1);
   begin
      Commands (1) :=
        (Count         => UInt (Index_Count),
         Instances     => 0,
         First_Index   => UInt (Object.Index_Offset),
         Base_Vertex   => UInt (Object.Vertex_Offset),
         Base_Instance => 0);

      --  Iterate over the vertices to interleave all the data into one buffer
      for I in 1 .. Size (Vertex_Count) loop
         declare
            Offset : constant Natural := (Object.Vertex_Offset + Natural (I) - 1) * 8;
         begin
            Object.Data.Write_Data (Positions.all (I * 3 - 2 .. I * 3), Offset => Offset + 0);
            Object.Data.Write_Data (Normals.all   (I * 3 - 2 .. I * 3), Offset => Offset + 3);
            Object.Data.Write_Data (UVs.all       (I * 2 - 1 .. I * 2), Offset => Offset + 6);
         end;
      end loop;

      --  Upload indices to IBO
      Object.Indices.Write_Data (Indices.all, Offset => Object.Index_Offset);

      --  Upload command to command buffer
      Object.Commands.Write_Data (Commands, Offset => Object.Index);

      Object.Index_Offset  := Object.Index_Offset  + Index_Count;
      Object.Vertex_Offset := Object.Vertex_Offset + Vertex_Count;
      Object.Index := Object.Index + 1;
   end Append;

   function Create_Batch (Parts, Vertices, Indices : Positive) return Batch is
      use all type Mapped.Unsynchronized.Unsynchronized_Mapped_Buffer;
   begin
      return Result : Batch do
         Result.Commands  := Create_Buffer
           (Types.Elements_Command_Type, Parts, Mapped.Write);

         Result.Data := Create_Buffer
           (Types.Half_Type, Vertices * 8, Mapped.Write);

         --  Indices
         Result.Indices := Create_Buffer (Types.UInt_Type, Indices, Mapped.Write);

         Result.Data.Map;
         Result.Indices.Map;
         Result.Commands.Map;
      end return;
   end Create_Batch;

   procedure Finish_Batch (Object : in out Batch) is
   begin
      Object.Data.Unmap;
      Object.Indices.Unmap;
      Object.Commands.Unmap;
   end Finish_Batch;

end Orka.Rendering.Buffers.MDI;
