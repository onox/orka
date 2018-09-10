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

with GL.Drawing;
with GL.Objects.Buffers;

package body Orka.Rendering.Vertex_Formats is

   use GL.Types;

   function Create
     (Vertex_Array  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Kind          : Numeric_Type;
      Binding_Index : GL.Objects.Vertex_Arrays.Binding) return Attribute_Buffer is
   begin
      return Attribute_Buffer'(Vertex_Array, Kind, 0, Binding_Index);
   end Create;

   procedure Add_Attribute
     (Object : in out Attribute_Buffer;
      Index  : GL.Types.Attribute;
      Count  : Component_Count) is
   begin
      Object.Vertex_Array.Enable_Attribute (Index);
      Object.Vertex_Array.Set_Attribute_Format (Index, Count, Object.Kind,
                                                UInt (Object.Attributes_Count));
      Object.Vertex_Array.Set_Attribute_Binding (Index, Object.Binding_Index);
      Object.Attributes_Count := Object.Attributes_Count + Natural (Count);
   end Add_Attribute;

   procedure Set_Buffer (Object : Attribute_Buffer; Buffer : Buffers.Buffer) is
   begin
      Object.Vertex_Array.Bind_Vertex_Buffer (Object.Binding_Index, Buffer.GL_Buffer,
                                              Object.Kind, 0,
                                              Int (Object.Attributes_Count));
   end Set_Buffer;

   procedure Set_Per_Instance (Object : Attribute_Buffer; Per_Instance : Boolean) is
      Divisor : constant UInt := (if Per_Instance then 1 else 0);
   begin
      Object.Vertex_Array.Set_Attribute_Binding_Divisor (Object.Binding_Index, Divisor);
   end Set_Per_Instance;

   -----------------------------------------------------------------------------

   function Create_Vertex_Format
     (Mode       : GL.Types.Connection_Mode;
      Index_Kind : GL.Types.Unsigned_Numeric_Type) return Vertex_Format is
   begin
      return Result : Vertex_Format do
         Result.Mode       := Mode;
         Result.Index_Kind := Index_Kind;
      end return;
   end Create_Vertex_Format;

   function Index_Kind (Object : Vertex_Format) return GL.Types.Unsigned_Numeric_Type is
     (Object.Index_Kind);

   function Attribute_Kind
     (Object : Vertex_Format;
      Index  : Positive) return GL.Types.Numeric_Type
   is
      Result : GL.Types.Numeric_Type;

      procedure Query (Element : Attribute_Buffer) is
      begin
         Result := Element.Kind;
      end Query;
   begin
      Object.Attributes.Query_Element (Index, Query'Access);
      return Result;
   end Attribute_Kind;

   procedure Add_Attribute_Buffer
     (Object  : in out Vertex_Format;
      Kind    : GL.Types.Numeric_Type;
      Process : not null access procedure (Buffer : in out Attribute_Buffer))
   is
      use GL.Objects.Vertex_Arrays;

      Binding_Index : constant Binding := Binding (Object.Attributes.Length);
      Buffer        : Attribute_Buffer := Create (Object.Vertex_Array, Kind, Binding_Index);
   begin
      Process (Buffer);
      Object.Attributes.Append (Buffer);
   end Add_Attribute_Buffer;

   procedure Set_Vertex_Buffer
     (Object : in out Vertex_Format;
      Index  : Positive;
      Buffer : Buffers.Buffer)
   is
      procedure Update (Element : in out Attribute_Buffer) is
      begin
         Element.Set_Buffer (Buffer);
      end Update;
   begin
      Object.Attributes.Update_Element (Index, Update'Access);
   end Set_Vertex_Buffer;

   procedure Set_Index_Buffer (Object : in out Vertex_Format; Buffer : Buffers.Buffer) is
   begin
      Object.Vertex_Array.Bind_Element_Buffer (Buffer.GL_Buffer);
   end Set_Index_Buffer;

   procedure Draw (Object : Vertex_Format; Offset, Count : GL.Types.Size) is
   begin
      Object.Vertex_Array.Bind;
      GL.Drawing.Draw_Arrays (Object.Mode, Offset, Count);
   end Draw;

   procedure Draw_Indirect (Object : Vertex_Format; Buffer : Buffers.Buffer) is
   begin
      Object.Vertex_Array.Bind;
      GL.Objects.Buffers.Draw_Indirect_Buffer.Bind (Buffer.GL_Buffer);
      GL.Drawing.Draw_Multiple_Elements_Indirect
        (Object.Mode, Object.Index_Kind, GL.Types.Size (Buffer.Length));
   end Draw_Indirect;

end Orka.Rendering.Vertex_Formats;
