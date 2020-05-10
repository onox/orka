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
with GL.Enums.Getter;

package body GL.Rasterization is

   procedure Set_Polygon_Mode (Value : Polygon_Mode_Type) is
   begin
      API.Polygon_Mode.Ref (Front_And_Back, Value);
   end Set_Polygon_Mode;

   function Polygon_Mode return Polygon_Mode_Type is
      Ret : Polygon_Mode_Type := Fill;
   begin
      API.Get_Polygon_Mode.Ref (Enums.Getter.Polygon_Mode, Ret);
      return Ret;
   end Polygon_Mode;

   procedure Set_Polygon_Offset (Factor, Units : Single; Clamp : Single := 0.0) is
   begin
      API.Polygon_Offset_Clamp.Ref (Factor, Units, Clamp);
   end Set_Polygon_Offset;

   function Polygon_Offset_Factor return Single is
      Result : Single := 0.0;
   begin
      API.Get_Single.Ref (Enums.Getter.Polygon_Offset_Factor, Result);
      return Result;
   end Polygon_Offset_Factor;

   function Polygon_Offset_Units return Single is
      Result : Single := 0.0;
   begin
      API.Get_Single.Ref (Enums.Getter.Polygon_Offset_Units, Result);
      return Result;
   end Polygon_Offset_Units;

   function Polygon_Offset_Clamp return Single is
      Result : Single := 0.0;
   begin
      API.Get_Single.Ref (Enums.Getter.Polygon_Offset_Clamp, Result);
      return Result;
   end Polygon_Offset_Clamp;

   -----------------------------------------------------------------------------

   procedure Set_Front_Face (Face : Orientation) is
   begin
      API.Front_Face.Ref (Face);
   end Set_Front_Face;

   function Front_Face return Orientation is
      Ret : Orientation := Orientation'First;
   begin
      API.Get_Orientation.Ref (Enums.Getter.Cull_Face, Ret);
      return Ret;
   end Front_Face;

   procedure Set_Cull_Face (Selector : Face_Selector) is
   begin
      API.Cull_Face.Ref (Selector);
   end Set_Cull_Face;

   function Cull_Face return Face_Selector is
      Ret : Face_Selector := Face_Selector'First;
   begin
      API.Get_Face_Selector.Ref (Enums.Getter.Cull_Face_Mode, Ret);
      return Ret;
   end Cull_Face;

end GL.Rasterization;
