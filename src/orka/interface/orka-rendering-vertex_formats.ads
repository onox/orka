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

with GL.Objects.Vertex_Arrays;
with GL.Types;

with Orka.Rendering.Buffers;

package Orka.Rendering.Vertex_Formats is
   pragma Preelaborate;

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
     (Mode       : GL.Types.Connection_Mode;
      Index_Kind : GL.Types.Unsigned_Numeric_Type) return Vertex_Format;

   function Index_Kind (Object : Vertex_Format) return GL.Types.Unsigned_Numeric_Type;

   function Attribute_Kind
     (Object : Vertex_Format;
      Index  : Positive) return GL.Types.Numeric_Type;

   procedure Add_Attribute_Buffer
     (Object  : in out Vertex_Format;
      Kind    : GL.Types.Numeric_Type;
      Process : not null access procedure (Buffer : in out Attribute_Buffer));

   procedure Set_Vertex_Buffer
     (Object : in out Vertex_Format;
      Index  : Positive;
      Buffer : Buffers.Buffer);

   procedure Set_Index_Buffer
     (Object : in out Vertex_Format;
      Buffer : Buffers.Buffer);

   procedure Draw (Object : Vertex_Format; Offset, Count : GL.Types.Size);

   procedure Draw_Indirect (Object : Vertex_Format; Buffer : Buffers.Buffer);

private

   type Attribute_Buffer is tagged record
      Vertex_Array     : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Kind             : GL.Types.Numeric_Type;
      Attributes_Count : Natural;
      Binding_Index    : GL.Objects.Vertex_Arrays.Binding;
   end record;

   package Attribute_Buffers is new Ada.Containers.Vectors (Positive, Attribute_Buffer);

   type Vertex_Format is tagged record
      Mode         : GL.Types.Connection_Mode;
      Index_Kind   : GL.Types.Unsigned_Numeric_Type;
      Vertex_Array : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Attributes   : Attribute_Buffers.Vector;
   end record;

end Orka.Rendering.Vertex_Formats;
