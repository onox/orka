--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2021 onox <denkpadje@gmail.com>
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

package AWT.Inputs with SPARK_Mode => On is
   pragma Preelaborate;

   type Fixed is delta 2.0 ** (-8) range -(2.0 ** 23) .. +(2.0 ** 23 - 1.0);
   for Fixed'Size use Integer'Size;

   type Button_State is (Released, Pressed);

   ----------------------------------------------------------------------------
   --                              Drag-and-drop                              -
   ----------------------------------------------------------------------------

   type Actions is record
      Copy : Boolean := False;
      Move : Boolean := False;
      Ask  : Boolean := False;
   end record;

   type Action_Kind is (Copy, Move, Ask, None);

   ----------------------------------------------------------------------------
   --                                 Pointer                                 -
   ----------------------------------------------------------------------------

   type Pointer_Button is (Left, Right, Middle, Unknown);

   type Pointer_Buttons is array (Pointer_Button) of Button_State;

   type Pointer_Mode is (Visible, Hidden, Locked);

   package Cursors is
      --  Cursors based on XDG cursor spec and CSS3 UI spec
      type Pointer_Cursor is
        (Default,

         --  Links and status
         Context_Menu,
         Help,
         Pointer,
         Progress,
         Wait,

         --  Selection
         Cell,
         Crosshair,
         Text,
         Vertical_Text,

         --  Drag and drop
         Alias,
         Copy,
         Move,      --  CSS3 UI
         No_Drop,
         Not_Allowed,
         Grab,      --  CSS3 UI
         Grabbing,  --  CSS3 UI

         --  Resizing and scrolling
         All_Scroll,
         Row_Resize,
         Col_Resize,
         N_Resize,
         E_Resize,
         S_Resize,
         W_Resize,
         NE_Resize,
         NW_Resize,
         SE_Resize,
         SW_Resize,
         EW_Resize,
         NS_Resize,
         NESW_Resize,
         NWSE_Resize,

         --  Zooming
         Zoom_In,    --  CSS3 UI
         Zoom_Out);  --  CSS3 UI
   end Cursors;

   type Dimension is (X, Y);

   type Coordinate is array (Dimension) of Fixed;

   type Pointer_State is record
      Buttons   : Pointer_Buttons := (others => Released);

      Focused   : Boolean         := False;
      Scrolling : Boolean         := False;
      --  If Focused is True and Scrolling transitions from True to False, then,
      --  and only then, you should activate kinetic scrolling

      Mode      : Pointer_Mode    := Visible;
      Position  : Coordinate      := (others => 0.0);
      Relative  : Coordinate      := (others => 0.0);
      Scroll    : Coordinate      := (others => 0.0);
   end record;

   ----------------------------------------------------------------------------
   --                                Keyboard                                 -
   ----------------------------------------------------------------------------

   type Keyboard_Modifiers is record
      Shift       : Boolean := False;
      Caps_Lock   : Boolean := False;
      Ctrl        : Boolean := False;
      Alt         : Boolean := False;
      Num_Lock    : Boolean := False;
      Logo        : Boolean := False;
   end record;

   --  Keys based on the standard 104 keys US QWERTY keyboard layout
   type Keyboard_Button is
     (Key_Unknown,

      Key_Tab,
      Key_Left_Shift,
      Key_Left_Ctrl,
      Key_Left_Alt,
      Key_Left_Logo,
      Key_Right_Logo,
      Key_Right_Alt,
      Key_Right_Ctrl,
      Key_Right_Shift,
      Key_Enter,

      Key_Space,
      Key_Comma,
      Key_Period,
      Key_Slash,
      Key_Semicolon,
      Key_Apostrophe,
      Key_Left_Bracket,
      Key_Right_Bracket,
      Key_Backtick,
      Key_Backslash,
      Key_Backspace,
      Key_Minus,
      Key_Equal,

      Key_Arrow_Up,
      Key_Arrow_Down,
      Key_Arrow_Left,
      Key_Arrow_Right,

      Key_Home,
      Key_End,
      Key_Page_Up,
      Key_Page_Down,
      Key_Delete,
      Key_Insert,

      Key_Caps_Lock,
      Key_Num_Lock,
      Key_Scroll_Lock,
      Key_Print_Screen,
      Key_Pause,

      Key_Escape,
      Key_F1,
      Key_F2,
      Key_F3,
      Key_F4,
      Key_F5,
      Key_F6,
      Key_F7,
      Key_F8,
      Key_F9,
      Key_F10,
      Key_F11,
      Key_F12,

      Key_1,
      Key_2,
      Key_3,
      Key_4,
      Key_5,
      Key_6,
      Key_7,
      Key_8,
      Key_9,
      Key_0,

      Key_A,
      Key_B,
      Key_C,
      Key_D,
      Key_E,
      Key_F,
      Key_G,
      Key_H,
      Key_I,
      Key_J,
      Key_K,
      Key_L,
      Key_M,
      Key_N,
      Key_O,
      Key_P,
      Key_Q,
      Key_R,
      Key_S,
      Key_T,
      Key_U,
      Key_V,
      Key_W,
      Key_X,
      Key_Y,
      Key_Z,

      Key_Numpad_Slash,
      Key_Numpad_Asterisk,
      Key_Numpad_Minus,
      Key_Numpad_Plus,
      Key_Numpad_Enter,
      Key_Numpad_Period,
      Key_Numpad_Delete,
      Key_Numpad_Insert,
      Key_Numpad_1,
      Key_Numpad_2,
      Key_Numpad_3,
      Key_Numpad_4,
      Key_Numpad_5,
      Key_Numpad_6,
      Key_Numpad_7,
      Key_Numpad_8,
      Key_Numpad_9,
      Key_Numpad_0);

   type Keyboard_Buttons is array (Keyboard_Button) of Button_State
     with Component_Size => 1;

   type Changed_Buttons is array (Keyboard_Button) of Boolean
     with Component_Size => 1;

   type Keyboard_State is record
      Buttons   : Keyboard_Buttons := (others => Released);
      Pressed   : Changed_Buttons  := (others => False);
      Released  : Changed_Buttons  := (others => False);

      Modifiers : Keyboard_Modifiers;
      Focused   : Boolean := False;

      Last_Pressed : Keyboard_Button := Key_Unknown;
      Repeat_Rate  : Natural  := 0;
      Repeat_Delay : Duration := 0.0;
   end record;

   function Keyboard_Has_Focus return Boolean;
   --  Return True if some window has keyboard focus, False otherwise

   --  TODO Add keyboard UTF-8 text input

   --  TODO Add function to return key name based on Keyboard_Button and
   --  current keyboard layout.
   --
   --  With US QWERTY layout Key_Q will return "q".
   --  With AZERTY layout Key_Q will return "a".

private

   type Unsigned_32 is mod 2 ** Integer'Size
     with Size => Integer'Size;

   for Keyboard_Modifiers use record
      Shift       at 0 range 0 .. 0;
      Caps_Lock   at 0 range 1 .. 1;
      Ctrl        at 0 range 2 .. 2;
      Alt         at 0 range 3 .. 3;
      Num_Lock    at 0 range 4 .. 4;
      Logo        at 0 range 6 .. 6;
   end record;
   for Keyboard_Modifiers'Size use Unsigned_32'Size;

end AWT.Inputs;
