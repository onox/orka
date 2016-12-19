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

with Ada.Containers.Vectors;

package Orka.Buffers.MDI is
   pragma Preelaborate;

   type MDI_Buffers is record
      Vertex_Buffer, Index_Buffer, Command_Buffer, Instances_Buffer : Buffer;
   end record;

   type Batch (Vertex_Length : Positive) is tagged limited private;

   procedure Append (Object : in out Batch;
                     Vertices : not null Indirect.Single_Array_Access;
                     Indices  : not null Indirect.UInt_Array_Access;
                     Instance_Index : out Natural)
     with Pre  => Indices'Length > 0 and Vertices'Length > 0
                    and (Vertices'Length mod Object.Vertex_Length) = 0;
     -- TODO Vertex_Length is number of elements. This only works if
     -- all elements have the same type. It should be number of bytes
     -- TODO add Indices'Length mod 3 = 0?

   procedure Clear (Object : in out Batch);
   --  Deallocate and remove all arrays that have been added via the
   --  Append procedure.

   function Length (Object : Batch) return Natural
     with Inline;

   function Create_Buffers
     (Object  : Batch;
      Flags   : GL.Objects.Buffers.Storage_Bits;
      Visible : Boolean := True) return MDI_Buffers
     with Pre => Object.Length > 0;
   --  Return a record containing a set of buffers containing the
   --  vertices and indices of all parts (batched), and the commands
   --  for drawing all these parts using just one draw call.

   function Create_Batch (Vertex_Length : Positive) return Batch;
   --  Create an empty batch. Parts can be added to this batch by
   --  calling Append. After having appended all parts to the batch,
   --  the vertex buffer, index buffer, and command buffer can be
   --  created by calling Create_Buffers.

private

   use type Indirect.Single_Array_Access;
   use type Indirect.UInt_Array_Access;

   subtype Positive_Size is Size range 1 .. Size'Last;

   package Single_Vectors is new Ada.Containers.Vectors (Positive_Size, Indirect.Single_Array_Access);
   package UInt_Vectors   is new Ada.Containers.Vectors (Positive_Size, Indirect.UInt_Array_Access);

   type Batch (Vertex_Length : Positive) is tagged limited record
      Vertices : Single_Vectors.Vector;
      Indices  : UInt_Vectors.Vector;
   end record;

end Orka.Buffers.MDI;
