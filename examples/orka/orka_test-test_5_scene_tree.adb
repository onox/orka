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

with Ada.Text_IO;
with Ada.Real_Time;

with Orka.Scenes.Singles.Trees;
--  with Orka.Scenes.Doubles.Trees;

procedure Orka_Test.Test_5_Scene_Tree is
   use type Ada.Real_Time.Time;

   package Trees renames Orka.Scenes.Singles.Trees;
--   package Trees renames Orka.Scenes.Doubles.Trees;

   T : Trees.Tree := Trees.Create_Tree ("root");
   A, B : Ada.Real_Time.Time;
begin
   A := Ada.Real_Time.Clock;
   for Index_One in 1 .. 25 loop
      declare
         Name_One : constant String := "N1" & Integer'Image (Index_One);
      begin
         T.Add_Node (Name_One, "root");

         for Index_Two in 1 .. 4 loop
            declare
               Name_Two : constant String := Name_One & "N2" & Integer'Image (Index_Two);
            begin
               T.Add_Node (Name_Two, Name_One);

               for Index_Three in 1 .. 5 loop
                  declare
                     Name_Three : constant String := Name_Two & "N3" & Integer'Image (Index_Three);
                  begin
                     T.Add_Node (Name_Three, Name_Two);

                     for Index_Four in 1 .. 20 loop
                        declare
                           Name_Four : constant String := Name_Three & "N4" & Integer'Image (Index_Four);
                        begin
                           T.Add_Node (Name_Four, Name_Three);
                        end;
                     end loop;
                  end;
               end loop;
            end;
         end loop;
      end;
   end loop;
   B := Ada.Real_Time.Clock;
   Ada.Text_IO.Put_Line (Duration'Image (1e3 * Ada.Real_Time.To_Duration (B - A)));

   for I in 1 .. 4 loop
      A := Ada.Real_Time.Clock;
      T.Update_Transforms;
      B := Ada.Real_Time.Clock;
      Ada.Text_IO.Put_Line (Duration'Image (1e3 * Ada.Real_Time.To_Duration (B - A)));
   end loop;

   Ada.Text_IO.Put_Line ("Depth: " & T.Depth'Img);
   for Level_Index in 1 .. T.Depth loop
      Ada.Text_IO.Put_Line ("Width: " & Natural'Image (T.Width (Level_Index)));
   end loop;
end Orka_Test.Test_5_Scene_Tree;
