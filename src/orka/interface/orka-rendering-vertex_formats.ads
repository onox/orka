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

private with Orka.Containers.Bounded_Vectors;

private with GL.Objects.Vertex_Arrays;

with GL.Types;

with Orka.Rendering.Buffers;
with Orka.Types;

package Orka.Rendering.Vertex_Formats is
   pragma Preelaborate;

   use all type Orka.Types.Element_Type;

   type Attribute_Buffer is tagged private;

   procedure Add_Attribute
     (Object : in out Attribute_Buffer;
      Index  : GL.Types.Attribute;
      Count  : GL.Types.Component_Count);

   procedure Set_Buffer (Object : Attribute_Buffer; Buffer : Buffers.Buffer);

   procedure Set_Per_Instance (Object : Attribute_Buffer; Per_Instance : Boolean);

   -----------------------------------------------------------------------------

   type Vertex_Format is tagged private;

   type Vertex_Format_Ptr is not null access Vertex_Format;

   function Create_Vertex_Format
     (Index_Kind : Types.Index_Type) return Vertex_Format;

   function Index_Kind (Object : Vertex_Format) return Types.Index_Type;

   function Attribute_Kind
     (Object : Vertex_Format;
      Index  : Positive) return Types.Numeric_Type;

   procedure Add_Attribute_Buffer
     (Object  : in out Vertex_Format;
      Kind    : Types.Numeric_Type;
      Process : not null access procedure (Buffer : in out Attribute_Buffer));

   procedure Set_Vertex_Buffer
     (Object : in out Vertex_Format;
      Index  : Positive;
      Buffer : Buffers.Buffer);

   procedure Set_Index_Buffer
     (Object : in out Vertex_Format;
      Buffer : Buffers.Buffer)
   with Pre => Object.Index_Kind = Buffer.Kind;

   procedure Bind (Object : Vertex_Format);

private

   type Attribute_Buffer is tagged record
      Vertex_Array     : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Kind             : Types.Numeric_Type;
      Attributes_Count : Natural;
      Binding_Index    : GL.Objects.Vertex_Arrays.Binding;
   end record;

   package Attribute_Buffers is new Orka.Containers.Bounded_Vectors (Positive, Attribute_Buffer);

   type Vertex_Format is tagged record
      Index_Kind   : Types.Index_Type;
      Vertex_Array : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Attributes   : Attribute_Buffers.Vector (Capacity => 8);
      --  Most hardware supports 16 separate buffers, but usually
      --  1 or 2 is sufficient. Ideally you should forget about
      --  vertex formats and just access the data as an SSBO in a shader.
   end record;

end Orka.Rendering.Vertex_Formats;
