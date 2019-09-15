--  SPDX-License-Identifier: Apache-2.0
--
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

with GL.Buffers;
with GL.Types.Colors;

private with GL.Low_Level;

package GL.Blending is
   pragma Preelaborate;

   -----------------------------------------------------------------------------
   --                                 Blending                                --
   -----------------------------------------------------------------------------

   --  Enable blending by enabling Blend in package GL.Toggles

   type Blend_Factor is (Zero, One, Src_Color, One_Minus_Src_Color, Src_Alpha,
                         One_Minus_Src_Alpha, Dst_Alpha, One_Minus_Dst_Alpha,
                         Dst_Color, One_Minus_Dst_Color, Src_Alpha_Saturate,
                         Constant_Color, One_Minus_Constant_Color,
                         Constant_Alpha, One_Minus_Constant_Alpha, Src1_Alpha,
                         Src1_Color, One_Minus_Src1_Color,
                         One_Minus_Src1_Alpha);
   --  Table 17.2 of the OpenGL specification

   type Equation is (Func_Add, Min, Max, Func_Subtract, Func_Reverse_Substract);

   procedure Set_Blend_Func
     (Src_RGB, Dst_RGB, Src_Alpha, Dst_Alpha : Blend_Factor);
   procedure Set_Blend_Func
     (Draw_Buffer : Buffers.Draw_Buffer_Index;
      Src_RGB, Dst_RGB, Src_Alpha, Dst_Alpha : Blend_Factor);

   function Blend_Func_Src_RGB return Blend_Factor;
   function Blend_Func_Src_Alpha return Blend_Factor;
   function Blend_Func_Dst_RGB return Blend_Factor;
   function Blend_Func_Dst_Alpha return Blend_Factor;

   procedure Set_Blend_Color (Value : Types.Colors.Color);
   function Blend_Color return Types.Colors.Color;

   procedure Set_Blend_Equation (RGB, Alpha : Equation);
   procedure Set_Blend_Equation
     (Draw_Buffer : Buffers.Draw_Buffer_Index; RGB, Alpha : Equation);

   function Blend_Equation_RGB return Equation;
   function Blend_Equation_Alpha return Equation;

   -----------------------------------------------------------------------------
   --                            Logical Operation                            --
   -----------------------------------------------------------------------------

   --  Enable logical operations by enabling Color_Logic_Op in
   --  package GL.Toggles. Enabling logical operation will disable
   --  blending. The logical operation has no effect if the attached
   --  textures of the framebuffer are floating-point or sRGB.

   type Logic_Op is (Clear, And_Op, And_Reverse, Copy, And_Inverted, Noop,
                     Xor_Op, Or_Op, Nor, Equiv, Invert, Or_Reverse,
                     Copy_Inverted, Or_Inverted, Nand, Set);
   --  Table 17.3 of the OpenGL specification

   procedure Set_Logic_Op_Mode (Value : Logic_Op);
   --  Set the logical operation to be applied to the color of the
   --  fragment and the current colors in the color buffers which
   --  are enabled for writing

   function Logic_Op_Mode return Logic_Op;

private

   for Blend_Factor use (Zero                     => 0,
                         One                      => 1,
                         Src_Color                => 16#0300#,
                         One_Minus_Src_Color      => 16#0301#,
                         Src_Alpha                => 16#0302#,
                         One_Minus_Src_Alpha      => 16#0303#,
                         Dst_Alpha                => 16#0304#,
                         One_Minus_Dst_Alpha      => 16#0305#,
                         Dst_Color                => 16#0306#,
                         One_Minus_Dst_Color      => 16#0307#,
                         Src_Alpha_Saturate       => 16#0308#,
                         Constant_Color           => 16#8001#,
                         One_Minus_Constant_Color => 16#8002#,
                         Constant_Alpha           => 16#8003#,
                         One_Minus_Constant_Alpha => 16#8004#,
                         Src1_Alpha               => 16#8589#,
                         Src1_Color               => 16#88F9#,
                         One_Minus_Src1_Color     => 16#88FA#,
                         One_Minus_Src1_Alpha     => 16#88FB#);
   for Blend_Factor'Size use Low_Level.Enum'Size;

   for Equation use (Func_Add               => 16#8006#,
                     Min                    => 16#8007#,
                     Max                    => 16#8008#,
                     Func_Subtract          => 16#800A#,
                     Func_Reverse_Substract => 16#800B#);
   for Equation'Size use Low_Level.Enum'Size;

   -----------------------------------------------------------------------------

   for Logic_Op use (Clear         => 16#1500#,
                     And_Op        => 16#1501#,
                     And_Reverse   => 16#1502#,
                     Copy          => 16#1503#,
                     And_Inverted  => 16#1504#,
                     Noop          => 16#1505#,
                     Xor_Op        => 16#1506#,
                     Or_Op         => 16#1507#,
                     Nor           => 16#1508#,
                     Equiv         => 16#1509#,
                     Invert        => 16#150A#,
                     Or_Reverse    => 16#150B#,
                     Copy_Inverted => 16#150C#,
                     Or_Inverted   => 16#150D#,
                     Nand          => 16#150E#,
                     Set           => 16#150F#);
   for Logic_Op'Size use Low_Level.Enum'Size;

end GL.Blending;
