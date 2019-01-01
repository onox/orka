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

package body GL.Framebuffer is

   function Read_Buffer return Buffers.Color_Buffer_Selector is
      Ret : Buffers.Color_Buffer_Selector := Buffers.Color_Buffer_Selector'First;
   begin
      API.Get_Read_Buffer_Selector (Enums.Getter.Read_Buffer, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Read_Buffer;

   procedure Set_Logic_Op_Mode (Value : Logic_Op) is
   begin
      API.Logic_Op (Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Logic_Op_Mode;

   function Logic_Op_Mode return Logic_Op is
      Ret : Logic_Op := Logic_Op'First;
   begin
      API.Get_Logic_Op (Enums.Getter.Logic_Op_Mode, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Logic_Op_Mode;

end GL.Framebuffer;
