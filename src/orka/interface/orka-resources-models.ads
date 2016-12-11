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

with Orka.Buffers.MDI;
with Orka.Meshes;
with Orka.Scenes.Generic_Scene_Trees;

generic
   with package Trees is new Orka.Scenes.Generic_Scene_Trees (<>);
package Orka.Resources.Models is
   pragma Preelaborate;

   type Model is tagged limited private;

   function Scene_Tree (Object : Model) return Trees.Tree
     with Inline;

   function Mesh (Object : Model) return Meshes.Mesh
     with Inline;

   function Command_Buffer (Object : Model) return Buffers.Buffer
     with Inline;

private

   type Model is tagged limited record
      Scene   : Trees.Tree;
      Mesh    : Meshes.Mesh;
      Buffers : Orka.Buffers.MDI.MDI_Buffers;
   end record;

   procedure Create_Mesh (Object : in out Model; Batch : Buffers.MDI.Batch);

end Orka.Resources.Models;
