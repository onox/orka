--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2013 Felix Krause <contact@flyx.org>
--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

   -----------------------------------------------------------------------------
   --                                Viewports                                --
   -----------------------------------------------------------------------------

   function Maximum_Viewports return Size is
      Result : Size := 16;
   begin
      API.Get_Size.Ref (Enums.Getter.Max_Viewports, Result);
      return Result;
   end Maximum_Viewports;

   function Viewport_Subpixel_Bits return Size is
      Result : Size := 0;
   begin
      API.Get_Size.Ref (Enums.Getter.Viewport_Subpixel_Bits, Result);
      return Result;
   end Viewport_Subpixel_Bits;

   function Origin_Range return Singles.Vector2 is
      Result : Singles.Vector2 := (0.0, 0.0);
   begin
      API.Get_Single_Vec2.Ref (Enums.Getter.Viewport_Bounds_Range, Result);
      return Result;
   end Origin_Range;

   function Maximum_Extent return Singles.Vector2 is
      Result : Singles.Vector2 := (0.0, 0.0);
   begin
      API.Get_Single_Vec2.Ref (Enums.Getter.Max_Viewport_Dims, Result);
      return Result;
   end Maximum_Extent;

   procedure Set_Viewports (List : Viewport_List) is
   begin
      API.Viewport_Array.Ref (List'First, List'Length, List);
   end Set_Viewports;

   function Get_Viewport (Index : UInt) return Viewport is
      Result : Singles.Vector4;
   begin
      API.Get_Single_Vec4_I.Ref (Enums.Getter.Viewport, Index, Result);
      return
        (X      => Result (X),
         Y      => Result (Y),
         Width  => Result (Z),
         Height => Result (W));
   end Get_Viewport;

   procedure Set_Depth_Ranges (List : Depth_Range_List) is
   begin
      API.Depth_Range_Array.Ref (List'First, List'Length, List);
   end Set_Depth_Ranges;

   function Get_Depth_Range (Index : UInt) return Depth_Range is
      Result : Doubles.Vector2;
   begin
      API.Get_Double_Vec2_I.Ref (Enums.Getter.Depth_Range, Index, Result);
      return
        (Near => Result (X),
         Far  => Result (Y));
   end Get_Depth_Range;

   procedure Set_Scissor_Rectangles (List : Scissor_Rectangle_List) is
   begin
      API.Scissor_Array.Ref (List'First, List'Length, List);
   end Set_Scissor_Rectangles;

   function Get_Scissor_Rectangle (Index : UInt) return Scissor_Rectangle is
      Result : Ints.Vector4;
   begin
      API.Get_Int_Vec4_I.Ref (Enums.Getter.Scissor_Box, Index, Result);
      return
        (Left   => Result (X),
         Bottom => Result (Y),
         Width  => Result (Z),
         Height => Result (W));
   end Get_Scissor_Rectangle;

   -----------------------------------------------------------------------------
   --                                 Clipping                                --
   -----------------------------------------------------------------------------

   procedure Set_Clipping (Origin : Viewport_Origin; Depth : Depth_Mode) is
   begin
      API.Clip_Control.Ref (Origin, Depth);
   end Set_Clipping;

   function Origin return Viewport_Origin is
      Result : Viewport_Origin := Viewport_Origin'First;
   begin
      API.Get_Clip_Origin.Ref (Enums.Getter.Clip_Origin, Result);
      return Result;
   end Origin;

   function Depth return Depth_Mode is
      Result : Depth_Mode := Depth_Mode'First;
   begin
      API.Get_Clip_Depth_Mode.Ref (Enums.Getter.Clip_Depth_Mode, Result);
      return Result;
   end Depth;

end GL.Viewports;
