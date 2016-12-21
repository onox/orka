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

with GL.Objects.Buffers;
with GL.Types;

with Orka.Meshes.Attributes;

package body Orka.Resources.Models is

   procedure Create_Mesh (Object : in out Model; Batch : Buffers.MDI.Batch) is
      use GL.Types;
      use GL.Objects.Buffers;
   begin
      Object.Buffers := Batch.Create_Buffers (Storage_Bits'(Dynamic_Storage => True, others => False));
      Object.Mesh := Orka.Meshes.Create_Vertex_Format (Triangles);

      declare
         Attributes : Meshes.Attributes.Attribute_Buffer := Object.Mesh.Add_Attribute_Buffer (Single_Type);
         Instances  : Meshes.Attributes.Attribute_Buffer := Object.Mesh.Add_Attribute_Buffer (UInt_Type);
      begin
         --  TODO Always assuming that attributes are VEC3, VEC3, VEC2 (each component = Single)
         --  total number of bytes: 12 + 12 + 8 = 32
         Attributes.Add_Attribute (0, 3);
         Attributes.Add_Attribute (1, 3);
         Attributes.Add_Attribute (2, 2);
         Attributes.Set_Buffer (Object.Buffers.Vertex_Buffer);

         Instances.Add_Attribute (3, 1);
         Instances.Set_Per_Instance (True);
         Instances.Set_Buffer (Object.Buffers.Instances_Buffer);
      end;
      Object.Mesh.Set_Index_Buffer (Object.Buffers.Index_Buffer);
   end Create_Mesh;

   function Scene_Tree (Object : in out Model) return Trees.Tree is
     (Object.Scene);

   procedure Update_World_Transforms (Object : in out Model) is
   begin
      Object.Scene.Update_Transforms;
   end Update_World_Transforms;

   function Shapes (Object : Model) return String_Vectors.Vector is
     (Object.Shapes);

   function Mesh (Object : Model) return Meshes.Vertex_Format is
     (Object.Mesh);

   function Command_Buffer (Object : Model) return Buffers.Buffer is
     (Object.Buffers.Command_Buffer);

end Orka.Resources.Models;
