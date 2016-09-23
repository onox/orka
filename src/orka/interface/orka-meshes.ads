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

with GL.Objects.Vertex_Arrays;
with GL.Types;

limited with Orka.Meshes.Attributes;
with Orka.Buffers;

package Orka.Meshes is
   pragma Preelaborate;

   type Mesh is tagged limited private;

   function Create_Mesh (Mode : GL.Types.Connection_Mode) return Mesh;

   function Add_Attribute_Buffer (Object : in out Mesh; Kind : GL.Types.Numeric_Type)
     return Orka.Meshes.Attributes.Attribute_Buffer;

   procedure Set_Index_Buffer (Object : in out Mesh; Buffer : Orka.Buffers.Buffer);

   procedure Draw (Object : Mesh; Offset, Count : GL.Types.Size);

   procedure Draw_Indirect (Object : Mesh; Buffer : Orka.Buffers.Buffer);

private

   type Mesh is tagged limited record
      Mode : GL.Types.Connection_Mode;
      Vertex_Array : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Attribute_Buffers_Count : Natural;
   end record;

end Orka.Meshes;
