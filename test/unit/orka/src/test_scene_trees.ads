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

with Ahven.Framework;

package Test_Scene_Trees is

   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test);

private

   procedure Test_Create_Tree;
   procedure Test_Add_Leaf_Node;
   procedure Test_Add_Non_Leaf_Node;
   procedure Test_To_Cursor_Root_Node;
   procedure Test_To_Cursor_Leaf_Node;
   procedure Test_To_Cursor_Non_Leaf_Node;
   procedure Test_To_Cursor_Exception;
   procedure Test_Remove_Leaf_Node;
   procedure Test_Remove_Subtree;
   procedure Test_Remove_Root_Exception;
   procedure Test_Set_Local_Transform;
   procedure Test_Update_Tree;
   procedure Test_World_Transform;
   procedure Test_Depth;
   procedure Test_Width;
   procedure Test_Visibility;

end Test_Scene_Trees;
