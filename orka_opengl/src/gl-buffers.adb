--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2012 Felix Krause <contact@flyx.org>
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

with Ada.Unchecked_Conversion;

with GL.API;

package body GL.Buffers is

   use type Rasterization.Face_Selector;

   procedure Set_Color_Mask (Value : Colors.Enabled_Color) is
   begin
      API.Color_Mask.Ref
        (Low_Level.Bool (Value (Colors.R)), Low_Level.Bool (Value (Colors.G)),
         Low_Level.Bool (Value (Colors.B)), Low_Level.Bool (Value (Colors.A)));
   end Set_Color_Mask;

   procedure Set_Color_Mask
     (Index : Draw_Buffer_Index;
      Value : Colors.Enabled_Color) is
   begin
      API.Color_Mask_Indexed.Ref
        (Index,
         Low_Level.Bool (Value (Colors.R)), Low_Level.Bool (Value (Colors.G)),
         Low_Level.Bool (Value (Colors.B)), Low_Level.Bool (Value (Colors.A)));
   end Set_Color_Mask;

   procedure Set_Depth_Function (Func : Compare_Function) is
   begin
      API.Depth_Func.Ref (Func);
   end Set_Depth_Function;

   procedure Set_Depth_Mask (Enabled : Boolean) is
   begin
      API.Depth_Mask.Ref (Low_Level.Bool (Enabled));
   end Set_Depth_Mask;

   procedure Set_Stencil_Function
     (Face : Rasterization.Face_Selector;
      Func : Compare_Function;
      Ref  : Int;
      Mask : UInt) is
   begin
      API.Stencil_Func_Separate.Ref (Face, Func, Ref, Mask);
   end Set_Stencil_Function;

   procedure Set_Stencil_Operation
     (Face         : Rasterization.Face_Selector;
      Stencil_Fail : Buffers.Stencil_Action;
      Depth_Fail   : Buffers.Stencil_Action;
      Depth_Pass   : Buffers.Stencil_Action) is
   begin
      API.Stencil_Op_Separate.Ref (Face, Stencil_Fail, Depth_Fail, Depth_Pass);
   end Set_Stencil_Operation;

   procedure Set_Stencil_Mask
     (Value : UInt;
      Face  : Rasterization.Face_Selector := Rasterization.Front_And_Back) is
   begin
      API.Stencil_Mask_Separate.Ref (Face, Value);
   end Set_Stencil_Mask;

end GL.Buffers;
