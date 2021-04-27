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

with SDL.Events.Mice;
with SDL.Video.Windows;

with Orka.Inputs.Pointers.Default;

package Orka.Inputs.SDL is
   pragma Preelaborate;

   type SDL_Pointer_Input is new Pointers.Default.Abstract_Pointer_Input with private;

   overriding
   procedure Set_Cursor_Mode
     (Object  : in out SDL_Pointer_Input;
      Mode    : Pointers.Default.Cursor_Mode);

   procedure Set_Button_State
     (Object  : in out SDL_Pointer_Input;
      Subject : Standard.SDL.Events.Mice.Buttons;
      State   : Pointers.Button_State);

   procedure Set_Window
     (Object  : in out SDL_Pointer_Input;
      Window  : Standard.SDL.Video.Windows.Window);

   function Create_Pointer_Input return Inputs.Pointers.Pointer_Input_Ptr;

private

   type SDL_Pointer_Input is new Pointers.Default.Abstract_Pointer_Input with record
      Window  : Standard.SDL.Video.Windows.ID;
   end record;

end Orka.Inputs.SDL;
