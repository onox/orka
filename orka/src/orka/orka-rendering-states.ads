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

with GL.Blending;
with GL.Buffers;
with GL.Rasterization;
with GL.Types.Colors;
with GL.Viewports;

package Orka.Rendering.States is
   pragma Preelaborate;

   type Cull_Face_Selector is (None, Front, Back);

   type Face is (Front, Back);

   ----------------------------------------------------------------------------

   type Stencil_Operation_Kind is (Stencil_Fail, Depth_Fail, Depth_Pass);

   type Stencil_Operations is array (Stencil_Operation_Kind) of GL.Buffers.Stencil_Action;

   type Stencil_Test is record
      Reference  : Orka.Integer_32           := 0;
      Test_Func  : GL.Types.Compare_Function := GL.Types.Always;
      Test_Mask  : Orka.Unsigned_32          := 16#FF#;
      Write_Mask : Orka.Unsigned_32          := 16#FF#;
      Operations : Stencil_Operations        := (others => GL.Buffers.Keep);
   end record;

   type Stencil_Tests is array (Face) of Stencil_Test;

   ----------------------------------------------------------------------------

   type Polygon_Offset_Type is record
      Factor, Units, Clamp : Orka.Float_32 := 0.0;
   end record;

   ----------------------------------------------------------------------------

   type Color_Masks is array (GL.Buffers.Draw_Buffer_Index) of GL.Types.Colors.Enabled_Color;

   type Blending_Functions is array (GL.Buffers.Draw_Buffer_Index) of GL.Blending.Blend_Factors;
   type Blending_Equations is array (GL.Buffers.Draw_Buffer_Index) of GL.Blending.Blend_Equations;

   use all type GL.Blending.Blend_Factor;
   use all type GL.Blending.Equation;

   type State is record
      Scissor_Box : GL.Viewports.Scissor_Rectangle := (others => 0);

      Depth_Clamp : Boolean := False;
      Depth_Func  : GL.Types.Compare_Function := GL.Types.Greater;
      Stenciling  : Stencil_Tests;

      Minimum_Sample_Shading : GL.Types.Normalized_Single := 0.0;

      Cull_Face             : Cull_Face_Selector := Back;
      --  Do not render triangles in the first place if you want to cull both Front and Back

      Polygon_Mode   : GL.Rasterization.Polygon_Mode_Type := GL.Rasterization.Fill;
      Polygon_Offset : Polygon_Offset_Type;
      --  Enabled if any Polygon_Offset.* > 0.0

      Color_Mask : Color_Masks := (others => (others => True));

      Blending        : Boolean               := False;
      Blend_Functions : Blending_Functions    := (others => (One, Zero, One, Zero));
      Blend_Equations : Blending_Equations    := (others => (Func_Add, Func_Add));
      Blend_Color     : GL.Types.Colors.Color := (others => 0.0);

      Logic_Operation : GL.Blending.Logic_Op  := GL.Blending.Copy;
      --  Enabled if /= Copy, disables blending when enabled.
      --  Has no effect on floating-point buffers.
   end record;

   --  TODO Sample_Mask, Clip_Distance

   procedure Apply_Changes (Previous, Current : State);

end Orka.Rendering.States;
