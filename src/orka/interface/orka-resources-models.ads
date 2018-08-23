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

with GL.Objects.Textures;

with Orka.Behaviors;
with Orka.Rendering.Buffers.MDI;
with Orka.Rendering.Buffers.Persistent_Mapped;
with Orka.Rendering.Programs.Uniforms;
with Orka.Rendering.Vertex_Formats;
with Orka.Scenes.Singles.Trees;
with Orka.Transforms.Singles.Matrices;
with Orka.Types;

package Orka.Resources.Models is
   pragma Preelaborate;

   package Trees renames Scenes.Singles.Trees;
   package Transforms renames Orka.Transforms.Singles.Matrices;

   type Buffer_Region_Type is mod 4;

   package PMB is new Orka.Rendering.Buffers.Persistent_Mapped (Buffer_Region_Type);

   type Model is limited new Resource with private;

   function Create_Instance
     (Object   : in out Model;
      Position : Behaviors.Transforms.Vector4) return Behaviors.Behavior_Ptr;

   type Model_Ptr is not null access all Model;

   Model_Load_Error : exception renames Resource_Load_Error;

   type Model_Instance (<>) is limited new Behaviors.Behavior with private;

   overriding
   procedure After_Update
     (Object : in out Model_Instance;
      Delta_Time    : Duration;
      View_Position : Transforms.Vector4);

   overriding
   procedure Render (Object : in out Model_Instance);

   overriding
   procedure After_Render (Object : in out Model_Instance);

   overriding
   function Position (Object : Model_Instance) return Behaviors.Transforms.Vector4;

private

   use GL.Objects.Textures;

   type Cursor_Array is array (Positive range <>) of Scenes.Singles.Trees.Cursor;

   package Cursor_Array_Holder is new Ada.Containers.Indefinite_Holders
     (Element_Type => Cursor_Array);

   type Model_Scene is limited record
      Scene  : Trees.Tree;
      Shapes : Cursor_Array_Holder.Holder;
   end record;

   type Model_Scene_Ptr is not null access Model_Scene;

   type Model is limited new Resource with record
      Scene   : Model_Scene_Ptr;
      Batch   : Rendering.Buffers.MDI.Batch;
      Format  : not null access Rendering.Vertex_Formats.Vertex_Format;
      Bounds  : Rendering.Buffers.Buffer;
      TBO_BB  : Buffer_Texture;
      Uniform_WT : not null access Rendering.Programs.Uniforms.Uniform_Sampler;
      Uniform_IO : not null access Rendering.Programs.Uniforms.Uniform;
   end record;

   type Model_Instance (Model : access Orka.Resources.Models.Model) is
     limited new Behaviors.Behavior with record
      Scene      : Trees.Tree;
      Transforms : PMB.Persistent_Mapped_Buffer
        (Kind => Orka.Types.Single_Matrix_Type,
         Mode => PMB.Write);
      TBO_WT     : Buffer_Texture;
      Position   : Behaviors.Transforms.Vector4;
   end record;

end Orka.Resources.Models;
