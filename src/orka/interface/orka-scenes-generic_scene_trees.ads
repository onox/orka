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
private with Ada.Strings.Unbounded;

generic
   type Matrix_Type is private;
   with function "*" (Left, Right : Matrix_Type) return Matrix_Type is <>;
   with function Identity_Value return Matrix_Type;
package Orka.Scenes.Generic_Scene_Trees is
   pragma Preelaborate;

   type Tree is tagged limited private;

   type Cursor (<>) is private;

   function To_Cursor (Object : Tree; Name : String) return Cursor;

   function Depth (Object : Tree) return Positive;

   function Width (Object : Tree; Level : Positive) return Natural
     with Pre => Level <= Object.Depth;

   procedure Update_Transforms (Object : in out Tree);

   procedure Set_Local_Transform (Object : in out Tree; Node : Cursor; Transform : Matrix_Type);

   function World_Transform (Object : Tree; Node : Cursor) return Matrix_Type;

   function Create_Tree (Name : String) return Tree;

   procedure Add_Node (Object : in out Tree; Name, Parent : String);

   procedure Remove_Node (Object : in out Tree; Name : String);

   Unknown_Node_Error : exception;

private

   package SU renames Ada.Strings.Unbounded;

   type Node is record
      Offset : Positive;
      Count  : Natural;
      Name   : SU.Unbounded_String;
   end record;

   package Node_Vectors is new Ada.Containers.Vectors (Positive, Node);

   package Matrix_Vectors is new Ada.Containers.Vectors (Positive, Matrix_Type);

   type Level is record
      Nodes : Node_Vectors.Vector;
      Local_Transforms : Matrix_Vectors.Vector;
      World_Transforms : Matrix_Vectors.Vector;
   end record;

   package Level_Vectors is new Ada.Containers.Vectors (Positive, Level);

   type Tree is tagged limited record
      Levels : Level_Vectors.Vector;
   end record;

   type Cursor (Level, Offset : Positive) is null record;

   function Depth (Object : Tree) return Positive is
     (Positive (Object.Levels.Length));

   function Width (Object : Tree; Level : Positive) return Natural is
     (Natural (Object.Levels (Level).Nodes.Length));

end Orka.Scenes.Generic_Scene_Trees;
