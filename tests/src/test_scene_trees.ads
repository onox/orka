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

with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Test_Scene_Trees is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Create_Tree (Object : in out Test);
   procedure Test_Add_Leaf_Node (Object : in out Test);
   procedure Test_Add_Non_Leaf_Node (Object : in out Test);
   procedure Test_To_Cursor_Root_Node (Object : in out Test);
   procedure Test_To_Cursor_Leaf_Node (Object : in out Test);
   procedure Test_To_Cursor_Non_Leaf_Node (Object : in out Test);
   procedure Test_To_Cursor_Exception (Object : in out Test);
   procedure Test_Remove_Leaf_Node (Object : in out Test);
   procedure Test_Remove_Subtree (Object : in out Test);
   procedure Test_Remove_Root_Exception (Object : in out Test);
   procedure Test_Set_Local_Transform (Object : in out Test);
   procedure Test_Update_Tree (Object : in out Test);
   procedure Test_World_Transform (Object : in out Test);
   procedure Test_Depth (Object : in out Test);
   procedure Test_Width (Object : in out Test);
   procedure Test_Visibility (Object : in out Test);

end Test_Scene_Trees;
