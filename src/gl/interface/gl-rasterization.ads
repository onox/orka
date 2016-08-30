--  Copyright (c) 2014 Felix Krause <contact@flyx.org>
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

   subtype Line_Width_Range is Singles.Vector2;

   type Polygon_Mode_Type is (Point, Line, Fill);

   procedure Set_Line_Width (Value : Single);
   function Line_Width return Single;

   function Aliased_Line_Width_Range return Line_Width_Range;
   function Smooth_Line_Width_Range  return Line_Width_Range;
   function Smooth_Line_Width_Granularity return Single;

   procedure Set_Polygon_Mode (Value : Polygon_Mode_Type);
   function Polygon_Mode return Polygon_Mode_Type;

private
   for Polygon_Mode_Type use (Point => 16#1B00#,
                              Line => 16#1B01#,
                              Fill => 16#1B02#);
   for Polygon_Mode_Type'Size use Low_Level.Enum'Size;
end GL.Rasterization;
