--  Copyright (c) 2012 Felix Krause <contact@flyx.org>
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

with GL.Objects.Buffers;

package GL.Objects.Vertex_Arrays is
   pragma Preelaborate;

   type Binding is new UInt;

   type Vertex_Array_Object is new GL_Object with private;

   procedure Bind (Object : Vertex_Array_Object);

   procedure Enable_Attribute  (Object : Vertex_Array_Object; Index : Attribute);
   procedure Disable_Attribute (Object : Vertex_Array_Object; Index : Attribute);

   procedure Set_Attribute_Format (Object : Vertex_Array_Object;
                                   Index  : Attribute;
                                   Count  : Component_Count;
                                   Kind   : Numeric_Type;
                                   Offset : UInt);

   procedure Set_Attribute_Binding (Object : Vertex_Array_Object;
                                    Index  : Attribute;
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

   overriding
   function Identifier (Object : Vertex_Array_Object) return Types.Debug.Identifier is
     (Types.Debug.Vertex_Array);

   function No_Vertex_Array_Object return Vertex_Array_Object;
   --  Bind this object to unbind the current array object

private

   type Vertex_Array_Object is new GL_Object with null record;

end GL.Objects.Vertex_Arrays;
