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

with GL.API;
with GL.Culling;
with GL.Enums.Getter;

package body GL.Rasterization is

   procedure Set_Line_Width (Value : Single) is
   begin
      API.Line_Width (Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Line_Width;

   function Line_Width return Single is
      Ret : aliased Single := 1.0;
   begin
      API.Get_Single (Enums.Getter.Line_Width, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Line_Width;

   function Aliased_Line_Width_Range return Line_Width_Range is
      Ret : Singles.Vector2 := (others => 0.0);
   begin
      API.Get_Single_Vec2 (Enums.Getter.Aliased_Line_Width_Range, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Aliased_Line_Width_Range;

   function Smooth_Line_Width_Range  return Line_Width_Range is
      Ret : Singles.Vector2 := (others => 0.0);
   begin
      API.Get_Single_Vec2 (Enums.Getter.Smooth_Line_Width_Range, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Smooth_Line_Width_Range;

   function Smooth_Line_Width_Granularity return Single is
      Ret : Single := 0.0;
   begin
      API.Get_Single (Enums.Getter.Smooth_Line_Width_Granularity, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Smooth_Line_Width_Granularity;

   procedure Set_Polygon_Mode (Value : Polygon_Mode_Type) is
   begin
      API.Polygon_Mode (Culling.Front_And_Back, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Polygon_Mode;

   function Polygon_Mode return Polygon_Mode_Type is
      Ret : Polygon_Mode_Type := Fill;
   begin
      API.Get_Polygon_Mode (Enums.Getter.Polygon_Mode, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Polygon_Mode;

   procedure Set_Polygon_Offset (Factor, Units : Single; Clamp : Single := 0.0) is
   begin
      API.Polygon_Offset_Clamp (Factor, Units, Clamp);
      Raise_Exception_On_OpenGL_Error;
   end Set_Polygon_Offset;

   function Polygon_Offset_Factor return Single is
      Result : Single := 0.0;
   begin
      API.Get_Single (Enums.Getter.Polygon_Offset_Factor, Result);
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Polygon_Offset_Factor;

   function Polygon_Offset_Units return Single is
      Result : Single := 0.0;
   begin
      API.Get_Single (Enums.Getter.Polygon_Offset_Units, Result);
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Polygon_Offset_Units;

   function Polygon_Offset_Clamp return Single is
      Result : Single := 0.0;
   begin
      API.Get_Single (Enums.Getter.Polygon_Offset_Clamp, Result);
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Polygon_Offset_Clamp;

end GL.Rasterization;
