--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2013 - 2014 Felix Krause <contact@flyx.org>
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

with GL.Types;

private with GL.Low_Level;

package GL.Rasterization is
   pragma Preelaborate;

   use GL.Types;

   type Polygon_Mode_Type is (Point, Line, Fill);

   procedure Set_Polygon_Mode (Value : Polygon_Mode_Type);
   function Polygon_Mode return Polygon_Mode_Type;

   procedure Set_Polygon_Offset (Factor, Units : Single; Clamp : Single := 0.0);
   --  Offset the depth values of a polygon
   --
   --  If Clamp /= 0.0 then the resulting offset is clamped.

   function Polygon_Offset_Factor return Single;
   function Polygon_Offset_Units return Single;
   function Polygon_Offset_Clamp return Single;

   -----------------------------------------------------------------------------
   --                                 Culling                                 --
   -----------------------------------------------------------------------------

   --  Enable culling by enabling Cull_Face in package GL.Toggles

   type Orientation is (Clockwise, Counter_Clockwise);

   type Face_Selector is (Front, Back, Front_And_Back);

   procedure Set_Front_Face (Face : Orientation);
   function Front_Face return Orientation;

   procedure Set_Cull_Face (Selector : Face_Selector);
   function Cull_Face return Face_Selector;

private

   for Polygon_Mode_Type use (Point => 16#1B00#,
                              Line => 16#1B01#,
                              Fill => 16#1B02#);
   for Polygon_Mode_Type'Size use Low_Level.Enum'Size;

   -----------------------------------------------------------------------------

   for Orientation use (Clockwise => 16#0900#, Counter_Clockwise => 16#0901#);
   for Orientation'Size use Low_Level.Enum'Size;

   for Face_Selector use (Front          => 16#0404#,
                          Back           => 16#0405#,
                          Front_And_Back => 16#0408#);
   for Face_Selector'Size use Low_Level.Enum'Size;

end GL.Rasterization;
