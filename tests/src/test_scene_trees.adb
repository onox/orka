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

with AUnit.Assertions;
with AUnit.Test_Caller;

with Orka.Scenes.Singles.Trees;
with Orka.Transforms.Singles.Matrices;

package body Test_Scene_Trees is

   use Orka.Scenes.Singles.Trees;

   use AUnit.Assertions;

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(Scene trees) ";
   begin
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Create_Tree function", Test_Create_Tree'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Add_Node procedure (leaf node)", Test_Add_Leaf_Node'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Add_Node procedure (non-leaf node)", Test_Add_Non_Leaf_Node'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test To_Cursor function (root node)", Test_To_Cursor_Root_Node'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test To_Cursor function (leaf node)", Test_To_Cursor_Leaf_Node'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test To_Cursor function (non-leaf node)", Test_To_Cursor_Non_Leaf_Node'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Raise Unknown_Node_Error in To_Cursor", Test_To_Cursor_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Remove_Node procedure (leaf node)", Test_Remove_Leaf_Node'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Remove_Node procedure (subtree)", Test_Remove_Subtree'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Raise Root_Removal_Error in Remove_Node", Test_Remove_Root_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Set_Local_Transform procedure", Test_Set_Local_Transform'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Update_Tree procedure", Test_Update_Tree'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test World_Transform function", Test_World_Transform'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Depth function", Test_Depth'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Width function", Test_Width'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Visibility function", Test_Visibility'Access));

      return Test_Suite'Access;
   end Suite;

   procedure Test_Create_Tree (Object : in out Test) is
      T : constant Tree := Create_Tree ("root");
      pragma Unreferenced (T);
   begin
      null;
   end Test_Create_Tree;

   procedure Test_Add_Leaf_Node (Object : in out Test) is
      T : Tree := Create_Tree ("root");
   begin
      T.Add_Node ("N1", "root");
      T.Add_Node ("N2", "N1");
   end Test_Add_Leaf_Node;

   procedure Test_Add_Non_Leaf_Node (Object : in out Test) is
      T : Tree := Create_Tree ("root");
   begin
      T.Add_Node ("N1", "root");
      T.Add_Node ("N2", "N1");

      --  Add nodes to non-leaf nodes
      T.Add_Node ("N3", "N1");
      T.Add_Node ("N4", "root");
   end Test_Add_Non_Leaf_Node;

   procedure Test_To_Cursor_Root_Node (Object : in out Test) is
      T : constant Tree := Create_Tree ("root");
      C : constant Cursor := T.To_Cursor ("root");
      pragma Unreferenced (C);
   begin
      null;
   end Test_To_Cursor_Root_Node;

   procedure Test_To_Cursor_Leaf_Node (Object : in out Test) is
      T : Tree := Create_Tree ("root");
   begin
      T.Add_Node ("N1", "root");
      declare
         C : constant Cursor := T.To_Cursor ("N1");
         pragma Unreferenced (C);
      begin
         null;
      end;
   end Test_To_Cursor_Leaf_Node;

   procedure Test_To_Cursor_Non_Leaf_Node (Object : in out Test) is
      T : Tree := Create_Tree ("root");
   begin
      T.Add_Node ("N1", "root");
      T.Add_Node ("N2", "N1");
      declare
         C : constant Cursor := T.To_Cursor ("N1");
         pragma Unreferenced (C);
      begin
         null;
      end;
   end Test_To_Cursor_Non_Leaf_Node;

   procedure Test_To_Cursor_Exception (Object : in out Test) is
      T : constant Tree := Create_Tree ("root");
   begin
      declare
         C : constant Cursor := T.To_Cursor ("N1");
         pragma Unreferenced (C);
      begin
         Assert (False, "Expected Unknown_Node_Error exception");
      end;
   exception
      when Unknown_Node_Error =>
         null;
   end Test_To_Cursor_Exception;

   procedure Test_Remove_Leaf_Node (Object : in out Test) is
      T : Tree := Create_Tree ("root");
   begin
      T.Add_Node ("N1", "root");
      T.Remove_Node ("N1");
      begin
         declare
            C : constant Cursor := T.To_Cursor ("N1");
            pragma Unreferenced (C);
         begin
            Assert (False, "Expected Unknown_Node_Error exception");
         end;
      exception
         when Unknown_Node_Error =>
            null;
      end;
   end Test_Remove_Leaf_Node;

   procedure Test_Remove_Subtree (Object : in out Test) is
      T : Tree := Create_Tree ("root");
   begin
      T.Add_Node ("N1", "root");
      T.Add_Node ("N2", "N1");
      T.Remove_Node ("N1");

      --  Test N1 has been removed
      begin
         declare
            C : constant Cursor := T.To_Cursor ("N1");
            pragma Unreferenced (C);
         begin
            Assert (False, "Expected Unknown_Node_Error exception");
         end;
      exception
         when Unknown_Node_Error =>
            null;
      end;

      --  Test N2 has been removed
      begin
         declare
            C : constant Cursor := T.To_Cursor ("N2");
            pragma Unreferenced (C);
         begin
            Assert (False, "Expected Unknown_Node_Error exception");
         end;
      exception
         when Unknown_Node_Error =>
            null;
      end;
   end Test_Remove_Subtree;

   procedure Test_Remove_Root_Exception (Object : in out Test) is
      T : Tree := Create_Tree ("root");
   begin
      T.Remove_Node ("root");
      Assert (False, "Excepted Root_Removal_Error exception");
   exception
      when Root_Removal_Error =>
         null;
   end Test_Remove_Root_Exception;

   procedure Test_Set_Local_Transform (Object : in out Test) is
      T : Tree := Create_Tree ("root");
      C : constant Cursor := T.To_Cursor ("root");

      package Transforms renames Orka.Transforms.Singles.Matrices;
      Offset : constant Transforms.Vector4 := (1.0, 2.0, 3.0, 1.0);
   begin
      T.Set_Local_Transform (C, Transforms.T (Offset));
   end Test_Set_Local_Transform;

   procedure Test_Update_Tree (Object : in out Test) is
      T : Tree := Create_Tree ("root");
   begin
      --  Depth 1
      T.Update_Tree;

      --  Depth 2
      T.Add_Node ("N1", "root");
      T.Update_Tree;

      --  Depth 3
      T.Add_Node ("N2", "N1");
      T.Update_Tree;
   end Test_Update_Tree;

   procedure Test_World_Transform (Object : in out Test) is
      T : Tree := Create_Tree ("root");
   begin
      T.Add_Node ("N1", "root");

      declare
         C1 : constant Cursor := T.To_Cursor ("root");
         C2 : constant Cursor := T.To_Cursor ("N1");

         use type Orka.Transforms.Singles.Matrices.Matrix4;
         use type Orka.Transforms.Singles.Matrices.Vector4;

         package Transforms renames Orka.Transforms.Singles.Matrices;
         Offset : constant Transforms.Vector4 := (1.0, 2.0, 3.0, 1.0);
      begin
         Assert (T.World_Transform (C2) = Transforms.Identity_Value, "Unexpected World_Transform");

         --  Update local transform of root node
         T.Set_Local_Transform (C1, Transforms.T (Offset));
         T.Update_Tree;

         --  Check world transform of node N1
         Assert (T.World_Transform (C2) (Orka.W) = Offset, "Unexpected World_Transform");
      end;
   end Test_World_Transform;

   procedure Test_Depth (Object : in out Test) is
      T : Tree := Create_Tree ("root");
   begin
      Assert (T.Depth = 1, "Unexpected Depth");

      T.Add_Node ("N1", "root");
      Assert (T.Depth = 2, "Unexpected Depth");
      T.Add_Node ("N2", "N1");
      Assert (T.Depth = 3, "Unexpected Depth");

      T.Remove_Node ("N1");
      Assert (T.Depth = 1, "Unexpected Depth");
   end Test_Depth;

   procedure Test_Width (Object : in out Test) is
      T : Tree := Create_Tree ("root");
   begin
      T.Add_Node ("N1", "root");
      T.Add_Node ("N2", "root");
      T.Add_Node ("N3", "N1");

      Assert (T.Width (1) = 1, "Unexpected Width");
      Assert (T.Width (2) = 2, "Unexpected Width");
      Assert (T.Width (3) = 1, "Unexpected Width");
   end Test_Width;

   procedure Test_Visibility (Object : in out Test) is
      T : Tree := Create_Tree ("root");
   begin
      T.Add_Node ("N1", "root");

      declare
         C1 : constant Cursor := T.To_Cursor ("root");
         C2 : constant Cursor := T.To_Cursor ("N1");
      begin
         Assert (T.Visibility (C2), "Unexpected Visibility");

         --  Update local visibility of root node
         T.Set_Visibility (C1, False);
         T.Update_Tree;

         --  Check visibility of node N1
         Assert (not T.Visibility (C2), "Unexpected Visibility");
      end;
   end Test_Visibility;

end Test_Scene_Trees;
