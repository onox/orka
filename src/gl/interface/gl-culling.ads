--  Copyright (c) 2013 Felix Krause <contact@flyx.org>
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

private with GL.Low_Level;

package GL.Culling is
   pragma Preelaborate;

   type Orientation is (Clockwise, Counter_Clockwise);

   type Face_Selector is (Front, Back, Front_And_Back);

   procedure Set_Front_Face (Face : Orientation);
   function Front_Face return Orientation;

   procedure Set_Cull_Face (Selector : Face_Selector);
   function Cull_Face return Face_Selector;

private
   for Orientation use (Clockwise => 16#0900#, Counter_Clockwise => 16#0901#);
   for Orientation'Size use Low_Level.Enum'Size;

   for Face_Selector use (Front          => 16#0404#,
                          Back           => 16#0405#,
                          Front_And_Back => 16#0408#);
   for Face_Selector'Size use Low_Level.Enum'Size;

   pragma Convention (StdCall, Set_Cull_Face);
   pragma Convention (StdCall, Set_Front_Face);
end GL.Culling;
