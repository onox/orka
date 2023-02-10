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

package body GL.Blending is

   procedure Set_Blend_Func
     (Factors : Blend_Factors) is
   begin
      API.Blend_Func_Separate.Ref
        (Factors.Src_RGB, Factors.Dst_RGB, Factors.Src_Alpha, Factors.Dst_Alpha);
   end Set_Blend_Func;

   procedure Set_Blend_Func
     (Draw_Buffer : Buffers.Draw_Buffer_Index;
      Factors     : Blend_Factors) is
   begin
      API.Blend_Func_Separate_I.Ref (Draw_Buffer,
        Factors.Src_RGB, Factors.Dst_RGB, Factors.Src_Alpha, Factors.Dst_Alpha);
   end Set_Blend_Func;

   procedure Set_Blend_Color (Value : Types.Colors.Color) is
      use Types.Colors;
   begin
      API.Blend_Color.Ref (Value (R), Value (G), Value (B), Value (A));
   end Set_Blend_Color;

   procedure Set_Blend_Equation (Equations : Blend_Equations) is
   begin
      API.Blend_Equation_Separate.Ref (Equations.RGB, Equations.Alpha);
   end Set_Blend_Equation;

   procedure Set_Blend_Equation
     (Draw_Buffer : Buffers.Draw_Buffer_Index;
      Equations   : Blend_Equations) is
   begin
      API.Blend_Equation_Separate_I.Ref (Draw_Buffer, Equations.RGB, Equations.Alpha);
   end Set_Blend_Equation;

   -----------------------------------------------------------------------------

   procedure Set_Logic_Op_Mode (Value : Logic_Op) is
   begin
      API.Logic_Op.Ref (Value);
   end Set_Logic_Op_Mode;

end GL.Blending;
