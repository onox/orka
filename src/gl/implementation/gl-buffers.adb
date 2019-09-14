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
with GL.Enums.Getter;

package body GL.Buffers is

   use type Rasterization.Face_Selector;

   procedure Color_Mask (Value : Colors.Enabled_Color) is
   begin
      API.Color_Mask
        (Low_Level.Bool (Value (Colors.R)), Low_Level.Bool (Value (Colors.G)),
         Low_Level.Bool (Value (Colors.B)), Low_Level.Bool (Value (Colors.A)));
      Raise_Exception_On_OpenGL_Error;
   end Color_Mask;

   procedure Color_Mask
     (Index : Draw_Buffer_Index;
      Value : Colors.Enabled_Color) is
   begin
      API.Color_Mask_Indexed
        (Index,
         Low_Level.Bool (Value (Colors.R)), Low_Level.Bool (Value (Colors.G)),
         Low_Level.Bool (Value (Colors.B)), Low_Level.Bool (Value (Colors.A)));
      Raise_Exception_On_OpenGL_Error;
   end Color_Mask;

   function Color_Mask (Index : Draw_Buffer_Index) return Colors.Enabled_Color is
      Value : Colors.Enabled_Color;
   begin
      API.Get_Enabled_Color (Enums.Getter.Color_Writemask, Index, Value);
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Color_Mask;

   procedure Set_Depth_Function (Func : Compare_Function) is
   begin
      API.Depth_Func (Func);
      Raise_Exception_On_OpenGL_Error;
   end Set_Depth_Function;

   function Depth_Function return Compare_Function is
      Value : Compare_Function := Compare_Function'First;
   begin
      API.Get_Compare_Function (Enums.Getter.Depth_Func, Value);
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Depth_Function;

   procedure Set_Depth_Mask (Enabled : Boolean) is
   begin
      API.Depth_Mask (Low_Level.Bool (Enabled));
      Raise_Exception_On_OpenGL_Error;
   end Set_Depth_Mask;

   function Depth_Mask return Boolean is
      Value : Low_Level.Bool := Low_Level.Bool (True);
   begin
      API.Get_Boolean (Enums.Getter.Depth_Writemask, Value);
      Raise_Exception_On_OpenGL_Error;
      return Boolean (Value);
   end Depth_Mask;

   procedure Set_Stencil_Function
     (Face : Rasterization.Face_Selector;
      Func : Compare_Function;
      Ref  : Int;
      Mask : UInt) is
   begin
      API.Stencil_Func_Separate (Face, Func, Ref, Mask);
      Raise_Exception_On_OpenGL_Error;
   end Set_Stencil_Function;

   function Stencil_Function (Face : Single_Face_Selector) return Compare_Function is
      Value : Compare_Function := Compare_Function'First;
   begin
      if Face = Rasterization.Front then
         API.Get_Compare_Function (Enums.Getter.Stencil_Func, Value);
      else
         API.Get_Compare_Function (Enums.Getter.Stencil_Back_Func, Value);
      end if;
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Stencil_Function;

   function Stencil_Reference_Value (Face : Single_Face_Selector) return Int is
      Value : Int := 0;
   begin
      if Face = Rasterization.Front then
         API.Get_Integer (Enums.Getter.Stencil_Ref, Value);
      else
         API.Get_Integer (Enums.Getter.Stencil_Back_Ref, Value);
      end if;
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Stencil_Reference_Value;

   function Stencil_Value_Mask (Face : Single_Face_Selector) return UInt is
      Value : UInt := 0;
   begin
      if Face = Rasterization.Front then
         API.Get_Unsigned_Integer (Enums.Getter.Stencil_Value_Mask, Value);
      else
         API.Get_Unsigned_Integer (Enums.Getter.Stencil_Back_Value_Mask, Value);
      end if;
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Stencil_Value_Mask;

   procedure Set_Stencil_Operation
     (Face         : Rasterization.Face_Selector;
      Stencil_Fail : Buffers.Stencil_Action;
      Depth_Fail   : Buffers.Stencil_Action;
      Depth_Pass   : Buffers.Stencil_Action) is
   begin
      API.Stencil_Op_Separate (Face, Stencil_Fail, Depth_Fail, Depth_Pass);
      Raise_Exception_On_OpenGL_Error;
   end Set_Stencil_Operation;

   function Stencil_Operation_Stencil_Fail
     (Face : Single_Face_Selector) return Buffers.Stencil_Action
   is
      Value : Buffers.Stencil_Action := Buffers.Stencil_Action'First;
   begin
      if Face = Rasterization.Front then
         API.Get_Stencil_Action (Enums.Getter.Stencil_Fail, Value);
      else
         API.Get_Stencil_Action (Enums.Getter.Stencil_Back_Fail, Value);
      end if;
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Stencil_Operation_Stencil_Fail;

   function Stencil_Operation_Depth_Fail
     (Face : Single_Face_Selector) return Buffers.Stencil_Action
   is
      Value : Buffers.Stencil_Action := Buffers.Stencil_Action'First;
   begin
      if Face = Rasterization.Front then
         API.Get_Stencil_Action (Enums.Getter.Stencil_Pass_Depth_Fail, Value);
      else
         API.Get_Stencil_Action (Enums.Getter.Stencil_Back_Pass_Depth_Fail, Value);
      end if;
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Stencil_Operation_Depth_Fail;

   function Stencil_Operation_Depth_Pass
     (Face : Single_Face_Selector) return Buffers.Stencil_Action
   is
      Value : Buffers.Stencil_Action := Buffers.Stencil_Action'First;
   begin
      if Face = Rasterization.Front then
         API.Get_Stencil_Action (Enums.Getter.Stencil_Pass_Depth_Pass, Value);
      else
         API.Get_Stencil_Action (Enums.Getter.Stencil_Back_Pass_Depth_Pass, Value);
      end if;
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Stencil_Operation_Depth_Pass;

   procedure Set_Stencil_Mask (Value : UInt) is
      Face : constant Rasterization.Face_Selector := Rasterization.Front_And_Back;
   begin
      Set_Stencil_Mask (Face, Value);
   end Set_Stencil_Mask;

   procedure Set_Stencil_Mask (Face  : Rasterization.Face_Selector;
                               Value : UInt) is
   begin
      API.Stencil_Mask_Separate (Face, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Stencil_Mask;

   function Stencil_Mask (Face : Single_Face_Selector) return UInt is
      Value : UInt := 0;
   begin
      if Face = Rasterization.Front then
         API.Get_Unsigned_Integer (Enums.Getter.Stencil_Writemask, Value);
      else
         API.Get_Unsigned_Integer (Enums.Getter.Stencil_Back_Writemask, Value);
      end if;
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Stencil_Mask;

end GL.Buffers;
