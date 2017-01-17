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

with Ada.Containers.Indefinite_Vectors;

with Orka.Buffers.MDI;
with Orka.Meshes;
with Orka.Scenes.Generic_Scene_Trees;

generic
   with package Trees is new Orka.Scenes.Generic_Scene_Trees (<>);
package Orka.Resources.Models is
   pragma Preelaborate;

   package String_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Model is tagged limited private;

   function Scene_Tree (Object : in out Model) return Trees.Tree
     with Inline;

   procedure Update_World_Transforms (Object : in out Model);

   function Shapes (Object : Model) return String_Vectors.Vector
     with Inline;

   function Mesh (Object : Model) return Meshes.Vertex_Format
     with Inline;

   function Command_Buffer (Object : Model) return Buffers.Buffer
     with Inline;

   Model_Load_Error : exception renames Resource_Load_Error;

private

   type Model is tagged limited record
      Scene   : Trees.Tree;
      Shapes  : String_Vectors.Vector;
      Mesh    : Meshes.Vertex_Format;
      Buffers : Orka.Buffers.MDI.MDI_Buffers;
   end record;

   procedure Create_Mesh (Object : in out Model; Batch : Buffers.MDI.Batch);

end Orka.Resources.Models;
