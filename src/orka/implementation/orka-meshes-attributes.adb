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

package body Orka.Meshes.Attributes is

   use GL.Types;

   function Create (Vertex_Array  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
                    Kind          : Numeric_Type;
                    Binding_Index : GL.Objects.Vertex_Arrays.Binding) return Attribute_Buffer is
   begin
      return Attribute_Buffer'(Vertex_Array, Kind, 0, Binding_Index);
   end Create;

   procedure Add_Attribute (Object : in out Attribute_Buffer;
                            Index  : GL.Attributes.Attribute;
                            Count  : Component_Count) is
   begin
      Object.Vertex_Array.Enable_Attribute (Index);
      Object.Vertex_Array.Set_Attribute_Format (Index, Count, Object.Kind,
                                                UInt (Object.Attributes_Count));
      Object.Vertex_Array.Set_Attribute_Binding (Index, Object.Binding_Index);
      Object.Attributes_Count := Object.Attributes_Count + Natural (Count);
   end Add_Attribute;

   procedure Set_Buffer (Object : Attribute_Buffer; Buffer : Orka.Buffers.Buffer) is
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

end Orka.Meshes.Attributes;
