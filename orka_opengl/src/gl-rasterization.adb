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

with GL.API;
with GL.Enums;

package body GL.Rasterization is

   procedure Set_Provoking_Vertex (Value : Vertex_Convention) is
   begin
      API.Provoking_Vertex.Ref
        (case Value is
           when First_Vertex => Enums.First_Vertex_Convention,
           when Last_Vertex  => Enums.Last_Vertex_Convention);
   end Set_Provoking_Vertex;

   procedure Set_Polygon_Mode (Value : Polygon_Mode_Type) is
   begin
      API.Polygon_Mode.Ref (Front_And_Back, Value);
   end Set_Polygon_Mode;

   procedure Set_Polygon_Offset (Factor, Units : Single; Clamp : Single := 0.0) is
   begin
      API.Polygon_Offset_Clamp.Ref (Factor, Units, Clamp);
   end Set_Polygon_Offset;

   procedure Set_Front_Face (Face : Orientation) is
   begin
      API.Front_Face.Ref (Face);
   end Set_Front_Face;

   procedure Set_Cull_Face (Selector : Face_Selector) is
   begin
      API.Cull_Face.Ref (Selector);
   end Set_Cull_Face;

end GL.Rasterization;
