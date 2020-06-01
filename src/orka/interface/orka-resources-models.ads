--  SPDX-License-Identifier: Apache-2.0
--
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

private with Ada.Containers.Indefinite_Holders;

private with Orka.Instances;
private with Orka.Rendering.Buffers.MDI;
private with Orka.Scenes.Singles.Trees;
private with Orka.Transforms.Singles.Matrices;
private with Orka.Types;

with Orka.Behaviors;
with Orka.Culling;

package Orka.Resources.Models is
   pragma Preelaborate;

   type Model_Instance is abstract limited new Behaviors.Behavior with private;

   type Model_Instance_Ptr is not null access all Model_Instance'Class;

   procedure Update_Transforms
     (Object : in out Model_Instance;
      View_Position : Behaviors.Vector4);

   -----------------------------------------------------------------------------

   type Model_Group is tagged limited private;

   type Group_Access is access Model_Group;

   procedure Add_Instance
     (Object   : access Model_Group;
      Instance : in out Model_Instance_Ptr);

   procedure Remove_Instance
     (Object   : in out Model_Group;
      Instance : in out Model_Instance_Ptr);

   procedure Cull (Object : in out Model_Group);

   procedure Render (Object : in out Model_Group);

   procedure After_Render (Object : in out Model_Group);

   -----------------------------------------------------------------------------

   type Model is limited new Resource with private;

   type Model_Ptr is not null access all Model;

   function Create_Group
     (Object   : aliased in out Model;
      Culler   : Culling.Culler_Ptr;
      Capacity : Positive) return Group_Access;

   Model_Load_Error : exception renames Resource_Load_Error;

private

   package Trees renames Scenes.Singles.Trees;
   package Transforms renames Orka.Transforms.Singles.Matrices;

   type Cursor_Array is array (Positive range <>) of Trees.Cursor;

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
      Bounds  : Rendering.Buffers.Buffer (Types.Single_Vector_Type);
   end record;

   type Partition_Index_Type is mod 4;
   package Model_Instances is new Orka.Instances (Partition_Index_Type);

   type Model_Group is tagged limited record
      Model     : access Orka.Resources.Models.Model;
      Instances : Model_Instances.Manager;

      Cull_Instance : Culling.Cull_Instance;

      Compacted_Transforms : Rendering.Buffers.Buffer (Types.Single_Matrix_Type);
      Compacted_Commands   : Rendering.Buffers.Buffer (Types.Elements_Command_Type);
   end record;

   type Model_Instance is abstract limited new Behaviors.Behavior with record
      Group    : access Model_Group;
      Scene    : Trees.Tree;
      Instance : Model_Instances.Cursor;
   end record;

end Orka.Resources.Models;
