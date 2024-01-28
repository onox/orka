--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2022 onox <denkpadje@gmail.com>
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

with GL.Shading;
with GL.Toggles;

package body Orka.Rendering.States is

   procedure Apply_Changes (Previous, Current : State) is
      use all type GL.Blending.Logic_Op;
      use all type GL.Toggles.Toggle;

      use type GL.Blending.Blend_Equations;
      use type GL.Blending.Blend_Factors;
      use type GL.Rasterization.Polygon_Mode_Type;
      use type GL.Types.Colors.Color;
      use type GL.Types.Colors.Enabled_Color;
      use type GL.Types.Compare_Function;
      use type GL.Viewports.Scissor_Rectangle;

      Empty_Box : constant GL.Viewports.Scissor_Rectangle := (others => 0);
   begin
      if Current.Scissor_Box /= Previous.Scissor_Box then
         if Current.Scissor_Box /= Empty_Box xor Previous.Scissor_Box /= Empty_Box then
            GL.Toggles.Set (Scissor_Test, Current.Scissor_Box /= Empty_Box);
         end if;

         GL.Viewports.Set_Scissor_Rectangles ((0 => Current.Scissor_Box));
      end if;

      if Current.Cull_Face /= Previous.Cull_Face then
         if Current.Cull_Face /= None xor Previous.Cull_Face /= None then
            GL.Toggles.Set (Cull_Face, Current.Cull_Face /= None);
         end if;

         if Current.Cull_Face /= None then
            GL.Rasterization.Set_Cull_Face
              (case Current.Cull_Face is
                 when Front => GL.Rasterization.Front,
                 when Back  => GL.Rasterization.Back,
                 when None  => raise Program_Error);
         end if;
      end if;

      if Current.Depth_Clamp /= Previous.Depth_Clamp then
         GL.Toggles.Set (Depth_Clamp, Current.Depth_Clamp);
      end if;

      if Current.Depth_Func /= Previous.Depth_Func then
         GL.Buffers.Set_Depth_Function (Current.Depth_Func);
      end if;

      if Current.Blending /= Previous.Blending then
         GL.Toggles.Set (Blend, Current.Blending);
      end if;

      for I in Current.Blend_Functions'Range loop
         if Current.Blend_Functions (I) /= Previous.Blend_Functions (I) then
            GL.Blending.Set_Blend_Func (I, Current.Blend_Functions (I));
         end if;
      end loop;

      for I in Current.Blend_Functions'Range loop
         if Current.Blend_Equations (I) /= Previous.Blend_Equations (I) then
            GL.Blending.Set_Blend_Equation (I, Current.Blend_Equations (I));
         end if;
      end loop;

      if Current.Blend_Color /= Previous.Blend_Color then
         GL.Blending.Set_Blend_Color (Current.Blend_Color);
      end if;

      if Current.Logic_Operation /= Copy xor Previous.Logic_Operation /= Copy then
         GL.Toggles.Set (Color_Logic_Op, Current.Logic_Operation /= Copy);
      end if;

      if Current.Logic_Operation /= Previous.Logic_Operation then
         GL.Blending.Set_Logic_Op_Mode (Current.Logic_Operation);
      end if;

      if Current.Minimum_Sample_Shading /= Previous.Minimum_Sample_Shading then
         --  Enable shading per-sample. Applies if MSAA is enabled.
         --  Provides better anti-aliasing for certain cases like
         --  alpha-tested transparency
         GL.Toggles.Set (Sample_Shading, Current.Minimum_Sample_Shading > 0.0);

         if Current.Minimum_Sample_Shading > 0.0 then
            GL.Shading.Set_Minimum_Sample_Shading (Current.Minimum_Sample_Shading);
         end if;
      end if;

      for I in Current.Color_Mask'Range loop
         if Current.Color_Mask (I) /= Previous.Color_Mask (I) then
            GL.Buffers.Set_Color_Mask (I, Current.Color_Mask (I));
         end if;
      end loop;

      for Face in Current.Stenciling'Range loop
         if Current.Stenciling (Face) /= Previous.Stenciling (Face) then
            declare
               S : Stencil_Test renames Current.Stenciling (Face);

               GL_Face : constant GL.Rasterization.Face_Selector :=
                 (case Face is
                    when Front => GL.Rasterization.Front,
                    when Back  => GL.Rasterization.Back);
            begin
               GL.Buffers.Set_Stencil_Function (GL_Face, S.Test_Func, S.Reference, S.Test_Mask);
               GL.Buffers.Set_Stencil_Operation
                 (GL_Face,
                  S.Operations (Stencil_Fail),
                  S.Operations (Depth_Fail),
                  S.Operations (Depth_Pass));
               GL.Buffers.Set_Stencil_Mask (S.Write_Mask, GL_Face);
            end;
         end if;
      end loop;

      if Current.Polygon_Mode /= Previous.Polygon_Mode then
         GL.Rasterization.Set_Polygon_Mode (Current.Polygon_Mode);
      end if;

      declare
         Was_Enabled : constant Boolean :=
           Previous.Polygon_Offset.Factor > 0.0
             or Previous.Polygon_Offset.Units > 0.0
             or Previous.Polygon_Offset.Clamp > 0.0;

         Enabled_Now : constant Boolean :=
           Current.Polygon_Offset.Factor > 0.0
             or Current.Polygon_Offset.Units > 0.0
             or Current.Polygon_Offset.Clamp > 0.0;

         use all type GL.Rasterization.Polygon_Mode_Type;
      begin
         if Enabled_Now /= Was_Enabled or Current.Polygon_Mode /= Previous.Polygon_Mode then
            GL.Toggles.Set (Polygon_Offset_Point, Enabled_Now and Current.Polygon_Mode = Point);
            GL.Toggles.Set (Polygon_Offset_Line,  Enabled_Now and Current.Polygon_Mode = Line);
            GL.Toggles.Set (Polygon_Offset_Fill,  Enabled_Now and Current.Polygon_Mode = Fill);
         end if;

         if Current.Polygon_Offset /= Previous.Polygon_Offset then
            GL.Rasterization.Set_Polygon_Offset
              (Current.Polygon_Offset.Factor,
               Current.Polygon_Offset.Units,
               Current.Polygon_Offset.Clamp);
         end if;
      end;
   end Apply_Changes;

end Orka.Rendering.States;
