--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <contact@flyx.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with GL.Attributes;
with GL.Objects.Buffers;

package GL.Objects.Vertex_Arrays is
   pragma Preelaborate;

   type Binding is new UInt;

   type Vertex_Array_Object is new GL_Object with private;

   procedure Bind (Object : Vertex_Array_Object);

   procedure Enable_Attribute  (Object : Vertex_Array_Object; Index : Attributes.Attribute);
   procedure Disable_Attribute (Object : Vertex_Array_Object; Index : Attributes.Attribute);

   procedure Set_Attribute_Format (Object : Vertex_Array_Object;
                                   Index  : Attributes.Attribute;
                                   Count  : Component_Count;
                                   Kind   : Numeric_Type;
                                   Offset : UInt);

   procedure Set_Attribute_Binding (Object : Vertex_Array_Object;
                                    Index  : Attributes.Attribute;
                                    Binding_Index : Binding);

   procedure Bind_Vertex_Buffer (Object : Vertex_Array_Object;
                                 Binding_Index : Binding;
                                 Buffer : Objects.Buffers.Buffer;
                                 Kind   : Numeric_Type;
                                 Offset, Stride : Size);

   procedure Bind_Element_Buffer (Object : Vertex_Array_Object;
                                  Buffer : Objects.Buffers.Buffer);

   procedure Set_Attribute_Binding_Divisor (Object : Vertex_Array_Object;
                                            Binding_Index : Binding;
                                            Divisor : UInt);

   function Current_Array_Object return Vertex_Array_Object;

   overriding
   procedure Initialize_Id (Object : in out Vertex_Array_Object);

   overriding
   procedure Delete_Id (Object : in out Vertex_Array_Object);

   Null_Array_Object : constant Vertex_Array_Object;
   --  Bind this object to unbind the current array object

private

   type Vertex_Array_Object is new GL_Object with null record;

   Null_Array_Object : constant Vertex_Array_Object
     := Vertex_Array_Object'(Ada.Finalization.Controlled with
        Reference => null);

end GL.Objects.Vertex_Arrays;
