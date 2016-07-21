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

with Orka.Meshes.Buffers;

package body Orka.Meshes is

   function Create_Mesh (Mode : GL.Types.Connection_Mode) return Mesh is
   begin
      return Result : Mesh do
         Result.Mode := Mode;
         Result.Vertex_Array.Initialize_Id;
         Result.Attribute_Buffers_Count := 0;
      end return;
   end Create_Mesh;

   function Add_Attribute_Buffer (Object : in out Mesh; Kind : GL.Types.Numeric_Type)
     return Orka.Meshes.Buffers.Attribute_Buffer is
      use GL.Objects.Vertex_Arrays;
      Binding_Index : constant Binding := Binding (Object.Attribute_Buffers_Count);
   begin
      Object.Attribute_Buffers_Count := Object.Attribute_Buffers_Count + 1;
      return Orka.Meshes.Buffers.Create (Object.Vertex_Array, Kind, Binding_Index);
   end Add_Attribute_Buffer;

   procedure Set_Index_Buffer (Object : in out Mesh; Buffer : Orka.Buffers.Buffer) is
   begin
      Object.Vertex_Array.Bind_Element_Buffer (Buffer.GL_Buffer);
   end Set_Index_Buffer;

   procedure Draw (Object : Mesh; Offset, Count : GL.Types.Size) is
   begin
      Object.Vertex_Array.Bind;
      GL.Drawing.Draw_Arrays (Object.Mode, Offset, Count);
   end Draw;

   procedure Draw_Indirect (Object : Mesh; Buffer : Orka.Buffers.Buffer) is
   begin
      Object.Vertex_Array.Bind;
      GL.Objects.Buffers.Draw_Indirect_Buffer.Bind (Buffer.GL_Buffer);
      GL.Drawing.Draw_Multiple_Elements_Indirect (Object.Mode, GL.Types.UInt_Type, GL.Types.Size (Buffer.Length));
   end Draw_Indirect;

end Orka.Meshes;
