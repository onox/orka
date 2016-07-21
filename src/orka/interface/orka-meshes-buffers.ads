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

with GL.Attributes;
with GL.Objects.Vertex_Arrays;
with GL.Types;

package Orka.Meshes.Buffers is
   pragma Preelaborate;

   type Attribute_Buffer is tagged limited private;

   function Create (Vertex_Array  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
                    Kind          : GL.Types.Numeric_Type;
                    Binding_Index : GL.Objects.Vertex_Arrays.Binding) return Attribute_Buffer;

   procedure Add_Attribute (Object : in out Attribute_Buffer;
                            Index  : GL.Attributes.Attribute;
                            Count  : GL.Types.Component_Count);

   procedure Set_Buffer (Object : Attribute_Buffer; Buffer : Orka.Buffers.Buffer);

   procedure Set_Per_Instance (Object : Attribute_Buffer; Per_Instance : Boolean);

private

   type Attribute_Buffer is tagged limited record
      Vertex_Array : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Kind : GL.Types.Numeric_Type;
      Attributes_Count : Natural;
      Binding_Index : GL.Objects.Vertex_Arrays.Binding;
   end record;

end Orka.Meshes.Buffers;
