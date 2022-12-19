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
     (Object    : in out Batch;
      Instances : Natural;
      Vertices  : Natural;
      Indices   : Natural;
      Append_Vertices : not null access procedure (Offset, Count : Natural);
      Append_Indices  : not null access procedure (Offset, Count : Natural))
   is
      Index_Count  : constant Natural := Indices;
      Vertex_Count : constant Natural := Vertices;

      Commands : Indirect.Elements_Indirect_Command_Array (1 .. 1);
   begin
      Commands (1) :=
        (Count         => UInt (Index_Count),
         Instances     => UInt (Instances),
         First_Index   => UInt (Object.Index_Offset),
         Base_Vertex   => UInt (Object.Vertex_Offset),
         Base_Instance => UInt (Object.Instance_Index));

      Append_Vertices (Offset => Object.Vertex_Offset, Count => Vertex_Count);
      Append_Indices  (Offset => Object.Index_Offset, Count => Index_Count);

      --  Upload command to command buffer
      Object.Commands.Write_Data (Commands, Offset => Object.Draw_Index);

      Object.Index_Offset  := Object.Index_Offset  + Index_Count;
      Object.Vertex_Offset := Object.Vertex_Offset + Vertex_Count;
      Object.Draw_Index := Object.Draw_Index + 1;
      Object.Instance_Index := Object.Instance_Index + Instances;
   end Append;

   function Create_Batch
     (Vertex_Kind : Types.Numeric_Type;
      Index_Kind  : Types.Index_Type;
      Parts, Vertex_Data, Indices : Positive) return Batch
   is
      use all type Mapped.Unsynchronized.Unsynchronized_Mapped_Buffer;
   begin
      return Result : Batch (Vertex_Kind, Index_Kind) do
         Result.Commands := Create_Buffer
           (Types.Elements_Command_Type, Parts, Mapped.Write);

         Result.Data := Create_Buffer
           (Vertex_Kind, Vertex_Data, Mapped.Write);

         --  Indices
         Result.Indices := Create_Buffer (Index_Kind, Indices, Mapped.Write);

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

   -----------------------------------------------------------------------------

   Interleaved_Half_Type_Elements : constant := 8;

   function Create_Batch (Parts, Vertices, Indices : Positive) return Batch is
     (Create_Batch (Types.Half_Type, Types.UInt_Type,
        Parts, Vertices * Interleaved_Half_Type_Elements, Indices));

   procedure Append
     (Object : in out Batch;
      Positions : not null Indirect.Half_Array_Access;
      Normals   : not null Indirect.Half_Array_Access;
      UVs       : not null Indirect.Half_Array_Access;
      Indices   : not null Indirect.UInt_Array_Access)
   is
      Vertex_Count : constant Natural := Positions'Length / 3;
      Index_Count  : constant Natural := Indices'Length;

      pragma Assert (Positions'Length = Normals'Length);
      pragma Assert (Vertex_Count = UVs'Length / 2);

      procedure Write_Vertices (Vertex_Offset, Vertex_Count : Natural) is
      begin
         --  Iterate over the vertices to interleave all the data into one buffer
         for I in 1 .. Size (Vertex_Count) loop
            declare
               Offset : constant Natural := (Vertex_Offset + Natural (I) - 1)
                 * Interleaved_Half_Type_Elements;
            begin
               Object.Data.Write_Data (Positions.all (I * 3 - 2 .. I * 3), Offset => Offset + 0);
               Object.Data.Write_Data (Normals.all   (I * 3 - 2 .. I * 3), Offset => Offset + 3);
               Object.Data.Write_Data (UVs.all       (I * 2 - 1 .. I * 2), Offset => Offset + 6);
            end;
         end loop;
      end Write_Vertices;

      procedure Write_Indices (Index_Offset, Index_Count : Natural) is
         pragma Assert (Indices'Length = Index_Count);
      begin
         Object.Indices.Write_Data (Indices.all, Offset => Index_Offset);
      end Write_Indices;
   begin
      Object.Append (0, Vertex_Count, Index_Count, Write_Vertices'Access, Write_Indices'Access);
   end Append;

end Orka.Rendering.Buffers.MDI;
