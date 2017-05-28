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

with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Vectors;

with GL.Low_Level.Enums;
with GL.Objects.Textures;

with Orka.Behaviors;
with Orka.Buffers.MDI;
with Orka.Programs.Uniforms;
with Orka.Scenes.Singles.Trees;
with Orka.Vertex_Formats;

package Orka.Resources.Models is
   pragma Preelaborate;

   package Trees renames Scenes.Singles.Trees;

   package String_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Model is tagged limited private;

   function Shapes (Object : Model) return String_Vectors.Vector
     with Inline;

   function Create_Instance (Object : Model) return Behaviors.Behavior_Ptr;

   Model_Load_Error : exception renames Resource_Load_Error;

   type Model_Instance is limited new Behaviors.Behavior with private;

   function Scene_Tree (Object : in out Model_Instance) return Trees.Tree
     with Inline;

   overriding
   procedure Render (Object : in out Model_Instance);

   overriding
   function Position (Object : Model_Instance) return Behaviors.Transforms.Vector4 is
     (Behaviors.Null_Behavior.Position);
   --  TODO Should be based on root node's world transform

private

   type Model is tagged limited record
      Scene   : Trees.Tree;
      Shapes  : String_Vectors.Vector;
      Format  : not null access Vertex_Formats.Vertex_Format;
      Buffers : Orka.Buffers.MDI.MDI_Buffers;
      Uniform_WT : not null access Programs.Uniforms.Uniform_Sampler;
   end record;

   type Shape_Array is array (Positive range <>) of Scenes.Singles.Trees.Cursor;

   package Shape_Array_Holder is new Ada.Containers.Indefinite_Holders
      (Element_Type => Shape_Array);

   use GL.Objects.Textures;

   type Model_Instance is limited new Behaviors.Behavior with record
      Scene      : Trees.Tree;
      Shapes     : Shape_Array_Holder.Holder;
      Format     : not null access Vertex_Formats.Vertex_Format;
      Transforms : Buffers.Buffer;
      TBO        : Buffer_Texture (GL.Low_Level.Enums.Texture_Buffer);
      Uniform_WT : not null access Programs.Uniforms.Uniform_Sampler;
      Buffers    : Orka.Buffers.MDI.MDI_Buffers;
   end record;

end Orka.Resources.Models;
