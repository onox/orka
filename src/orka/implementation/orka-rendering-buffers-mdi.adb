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
      --  TODO Don't hardcode

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

      --  Upload attributes to VBO's
      Object.Positions.Write_Data (Positions.all, Offset => Object.Vertex_Offset * 3);
      Object.Normals.Write_Data   (Normals.all,   Offset => Object.Vertex_Offset * 3);
      Object.UVs.Write_Data       (UVs.all,       Offset => Object.Vertex_Offset * 2);

      --  Upload indices to IBO
      Object.Indices.Write_Data (Indices.all, Offset => Object.Index_Offset);

      --  Upload command to command buffer
      Object.Commands.Write_Data (Commands, Offset => Object.Index);

      Object.Index_Offset  := Object.Index_Offset  + Index_Count;
      Object.Vertex_Offset := Object.Vertex_Offset + Vertex_Count;
      Object.Index := Object.Index + 1;
   end Append;

   function Create_Batch
     (Parts, Vertices, Indices : Positive;
      Format  : Rendering.Vertex_Formats.Vertex_Format) return Batch
   is
      use all type Mapped.Unsynchronized.Unsynchronized_Mapped_Buffer;
   begin
      return Result : Batch do
         Result.Commands  := Create_Buffer
           (Types.Elements_Command_Type, Parts, Mapped.Write);

         --  Attributes
         Result.Positions := Create_Buffer
           (Format.Attribute_Kind (1), Vertices * 3, Mapped.Write);
         Result.Normals   := Create_Buffer
           (Format.Attribute_Kind (2), Vertices * 3, Mapped.Write);
         Result.UVs       := Create_Buffer
           (Format.Attribute_Kind (3), Vertices * 2, Mapped.Write);
         --  TODO Don't hardcode vector size factors

         --  Indices
         Result.Indices := Create_Buffer (Format.Index_Kind, Indices, Mapped.Write);

         Result.Positions.Map;
         Result.Normals.Map;
         Result.UVs.Map;
         Result.Indices.Map;
         Result.Commands.Map;
      end return;
   end Create_Batch;

   procedure Finish_Batch (Object : in out Batch) is
   begin
      Object.Positions.Unmap;
      Object.Normals.Unmap;
      Object.UVs.Unmap;
      Object.Indices.Unmap;
      Object.Commands.Unmap;
   end Finish_Batch;

   procedure Bind_Buffers_To
     (Object : in out Batch;
      Format : in out Vertex_Formats.Vertex_Format) is
   begin
      Format.Set_Vertex_Buffer (1, Object.Positions.Buffer);
      Format.Set_Vertex_Buffer (2, Object.Normals.Buffer);
      Format.Set_Vertex_Buffer (3, Object.UVs.Buffer);

      Format.Set_Index_Buffer (Object.Indices.Buffer);
   end Bind_Buffers_To;

end Orka.Rendering.Buffers.MDI;
