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

with GL.API;
with GL.Enums.Getter;

package body GL.Viewports is

   function Maximum_Viewports return Size is
      Result : Size := 16;
   begin
      API.Get_Size (Enums.Getter.Max_Viewports, Result);
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Maximum_Viewports;

   function Viewport_Subpixel_Bits return Size is
      Result : Size := 0;
   begin
      API.Get_Size (Enums.Getter.Viewport_Subpixel_Bits, Result);
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Viewport_Subpixel_Bits;

   function Origin_Range return Singles.Vector2 is
      Result : Singles.Vector2 := (0.0, 0.0);
   begin
      API.Get_Single_Vec2 (Enums.Getter.Viewport_Bounds_Range, Result);
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Origin_Range;

   function Maximum_Extent return Singles.Vector2 is
      Result : Singles.Vector2 := (0.0, 0.0);
   begin
      API.Get_Single_Vec2 (Enums.Getter.Max_Viewport_Dims, Result);
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Maximum_Extent;

   procedure Set_Viewports (List : Viewport_List) is
   begin
      GL.API.Viewport_Array (List'First, List'Length, List);
      Raise_Exception_On_OpenGL_Error;
   end Set_Viewports;

   function Get_Viewport (Index : UInt) return Viewport is
      Result : Singles.Vector4;
   begin
      API.Get_Single_Vec4_I (Enums.Getter.Viewport, Index, Result);
      Raise_Exception_On_OpenGL_Error;
      return
        (X      => Result (X),
         Y      => Result (Y),
         Width  => Result (Z),
         Height => Result (W));
   end Get_Viewport;

   procedure Set_Depth_Ranges (List : Depth_Range_List) is
   begin
      API.Depth_Range_Array (List'First, List'Length, List);
      Raise_Exception_On_OpenGL_Error;
   end Set_Depth_Ranges;

   function Get_Depth_Range (Index : UInt) return Depth_Range is
      Result : Doubles.Vector2;
   begin
      API.Get_Double_Vec2_I (Enums.Getter.Depth_Range, Index, Result);
      Raise_Exception_On_OpenGL_Error;
      return
        (Near => Result (X),
         Far  => Result (Y));
   end Get_Depth_Range;

   procedure Set_Scissor_Rectangles (List : Scissor_Rectangle_List) is
   begin
      API.Scissor_Array (List'First, List'Length, List);
      Raise_Exception_On_OpenGL_Error;
   end Set_Scissor_Rectangles;

   function Get_Scissor_Rectangle (Index : UInt) return Scissor_Rectangle is
      Result : Ints.Vector4;
   begin
      API.Get_Int_Vec4_I (Enums.Getter.Scissor_Box, Index, Result);
      Raise_Exception_On_OpenGL_Error;
      return
        (Left   => Result (X),
         Bottom => Result (Y),
         Width  => Result (Z),
         Height => Result (W));
   end Get_Scissor_Rectangle;

end GL.Viewports;
