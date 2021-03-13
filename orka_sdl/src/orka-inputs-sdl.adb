--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2019 onox <denkpadje@gmail.com>
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

with SDL.Inputs.Mice;

package body Orka.Inputs.SDL is

   overriding
   procedure Set_Cursor_Mode
     (Object  : in out SDL_Pointer_Input;
      Mode    : Pointers.Default.Cursor_Mode)
   is
      use all type Pointers.Default.Cursor_Mode;
   begin
      case Mode is
         when Normal =>
            Standard.SDL.Inputs.Mice.Set_Relative_Mode (False);
            Standard.SDL.Inputs.Mice.Show_Cursor (True);
         when Hidden =>
            Standard.SDL.Inputs.Mice.Set_Relative_Mode (False);
            Standard.SDL.Inputs.Mice.Show_Cursor (False);
         when Disabled =>
            Standard.SDL.Inputs.Mice.Set_Relative_Mode (True);
      end case;
   end Set_Cursor_Mode;

   procedure Set_Button_State
     (Object  : in out SDL_Pointer_Input;
      Subject : Standard.SDL.Events.Mice.Buttons;
      State   : Pointers.Button_State)
   is
      use Standard.SDL.Events;
      use all type Inputs.Pointers.Button;
   begin
      case Subject is
         when Mice.Left =>
            Object.Set_Button_State (Left, State);
         when Mice.Right =>
            Object.Set_Button_State (Right, State);
         when Mice.Middle =>
            Object.Set_Button_State (Middle, State);
         when others =>
            raise Program_Error with "Invalid mouse button";
      end case;
   end Set_Button_State;

   procedure Set_Window
     (Object  : in out SDL_Pointer_Input;
      Window  : Standard.SDL.Video.Windows.Window) is
   begin
      Object.Window := Window.Get_ID;
   end Set_Window;

   function Create_Pointer_Input return Inputs.Pointers.Pointer_Input_Ptr is
   begin
      return new SDL_Pointer_Input'
        (Pointers.Default.Abstract_Pointer_Input with others => <>);
   end Create_Pointer_Input;

end Orka.Inputs.SDL;
