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

with GL.API;
with GL.Enums.Getter;

package body GL.Blending is

   procedure Set_Blend_Func
     (Factors : Blend_Factors) is
   begin
      API.Blend_Func_Separate
        (Factors.Src_RGB, Factors.Dst_RGB, Factors.Src_Alpha, Factors.Dst_Alpha);
   end Set_Blend_Func;

   procedure Set_Blend_Func
     (Draw_Buffer : Buffers.Draw_Buffer_Index;
      Factors     : Blend_Factors) is
   begin
      API.Blend_Func_Separate_I (Draw_Buffer,
        Factors.Src_RGB, Factors.Dst_RGB, Factors.Src_Alpha, Factors.Dst_Alpha);
   end Set_Blend_Func;

   function Blend_Func return Blend_Factors is
      Src_RGB, Src_Alpha : Blend_Factor := One;
      Dst_RGB, Dst_Alpha : Blend_Factor := Zero;
   begin
      API.Get_Blend_Factor (Enums.Getter.Blend_Src_RGB, Src_RGB);
      API.Get_Blend_Factor (Enums.Getter.Blend_Src_Alpha, Src_Alpha);
      API.Get_Blend_Factor (Enums.Getter.Blend_Dst_RGB, Dst_RGB);
      API.Get_Blend_Factor (Enums.Getter.Blend_Dst_Alpha, Dst_Alpha);

      return (Src_RGB, Dst_RGB, Src_Alpha, Dst_Alpha);
   end Blend_Func;

   procedure Set_Blend_Color (Value : Types.Colors.Color) is
      use Types.Colors;
   begin
      API.Blend_Color (Value (R), Value (G), Value (B), Value (A));
   end Set_Blend_Color;

   function Blend_Color return Types.Colors.Color is
      Ret : Types.Colors.Color;
   begin
      API.Get_Color (Enums.Getter.Blend_Color, Ret);
      return Ret;
   end Blend_Color;

   procedure Set_Blend_Equation (Equations : Blend_Equations) is
   begin
      API.Blend_Equation_Separate (Equations.RGB, Equations.Alpha);
   end Set_Blend_Equation;

   procedure Set_Blend_Equation
     (Draw_Buffer : Buffers.Draw_Buffer_Index;
      Equations   : Blend_Equations) is
   begin
      API.Blend_Equation_Separate_I (Draw_Buffer, Equations.RGB, Equations.Alpha);
   end Set_Blend_Equation;

   function Blend_Equation return Blend_Equations is
      RGB, Alpha : Equation := Equation'First;
   begin
      API.Get_Blend_Equation (Enums.Getter.Blend_Equation_RGB, RGB);
      API.Get_Blend_Equation (Enums.Getter.Blend_Equation_Alpha, Alpha);

      return (RGB, Alpha);
   end Blend_Equation;

   -----------------------------------------------------------------------------

   procedure Set_Logic_Op_Mode (Value : Logic_Op) is
   begin
      API.Logic_Op (Value);
   end Set_Logic_Op_Mode;

   function Logic_Op_Mode return Logic_Op is
      Ret : Logic_Op := Logic_Op'First;
   begin
      API.Get_Logic_Op (Enums.Getter.Logic_Op_Mode, Ret);
      return Ret;
   end Logic_Op_Mode;

end GL.Blending;
