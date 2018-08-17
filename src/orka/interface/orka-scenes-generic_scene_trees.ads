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

private with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Orka.Transforms.SIMD_Matrices;

generic
   with package Transforms is new Orka.Transforms.SIMD_Matrices (<>);
package Orka.Scenes.Generic_Scene_Trees is
   pragma Preelaborate;

   package SU renames Ada.Strings.Unbounded;

   subtype Matrix4 is Transforms.Matrix4;

   type Tree is tagged private;

   type Cursor is private;

   function To_Cursor (Object : Tree; Name : String) return Cursor;

   function Depth (Object : Tree) return Positive;

   function Width (Object : Tree; Level : Positive) return Natural
     with Pre => Level <= Object.Depth;

   procedure Update_Tree (Object : in out Tree);

   procedure Update_Tree (Object : in out Tree; Root_Transform : Transforms.Matrix4);

   procedure Set_Visibility (Object : in out Tree; Node : Cursor; Visible : Boolean);

   function Visibility (Object : Tree; Node : Cursor) return Boolean;

   procedure Set_Local_Transform (Object : in out Tree; Node : Cursor; Transform : Transforms.Matrix4);

   function World_Transform (Object : Tree; Node : Cursor) return Transforms.Matrix4;

   function Root_Name (Object : Tree) return String;

   function Create_Tree (Name : String) return Tree;

   procedure Add_Node (Object : in out Tree; Name, Parent : String);
   procedure Add_Node (Object : in out Tree; Name : SU.Unbounded_String; Parent : String);

   procedure Remove_Node (Object : in out Tree; Name : String);

   Unknown_Node_Error : exception;
   --  Exception raised if a given node does not exist

   Root_Removal_Error : exception;
   --  Exception raised if user tries to remove the root node

private

   type Node is record
      Offset : Positive;
      Count  : Natural;
      Name   : SU.Unbounded_String;
   end record;

   pragma Suppress (Tampering_Check);
   --  Disabling the tampering check speeds up execution of Add_Node,
   --  reducing the time to create a full scene tree to around 20 %.

   package Node_Vectors is new Ada.Containers.Vectors (Positive, Node);

   package Boolean_Vectors is new Ada.Containers.Vectors (Positive, Boolean);

   use type Transforms.Matrix4;
   package Matrix_Vectors is new Ada.Containers.Vectors (Positive, Transforms.Matrix4);

   type Level is record
      Nodes : Node_Vectors.Vector;
      Local_Transforms : Matrix_Vectors.Vector;
      World_Transforms : Matrix_Vectors.Vector;
      Local_Visibilities : Boolean_Vectors.Vector;
      World_Visibilities : Boolean_Vectors.Vector;
   end record;

   package Level_Vectors is new Ada.Containers.Vectors (Positive, Level);

   type Tree is tagged record
      Levels : Level_Vectors.Vector;
   end record;

   type Cursor is record
      Level, Offset : Positive;
   end record;

   function Depth (Object : Tree) return Positive is
     (Positive (Object.Levels.Length));

   function Width (Object : Tree; Level : Positive) return Natural is
     (Natural (Object.Levels (Level).Nodes.Length));

end Orka.Scenes.Generic_Scene_Trees;
