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

with Ada.Unchecked_Conversion;

package body AWT.OS is

   use all type AWT.Inputs.Pointer_Button;
   use all type AWT.Inputs.Keyboard_Button;

   function Code_To_Button (Code : Unsigned_32) return AWT.Inputs.Pointer_Button is
     (case Code is
        when 272 => Left,
        when 273 => Right,
        when 274 => Middle,
        when others => Unknown);

   function Code_To_Button (Code : Unsigned_32) return AWT.Inputs.Keyboard_Button is
     (case Code is
        when 1 => Key_Escape,
        when 2 => Key_1,
        when 3 => Key_2,
        when 4 => Key_3,
        when 5 => Key_4,
        when 6 => Key_5,
        when 7 => Key_6,
        when 8 => Key_7,
        when 9 => Key_8,
        when 10 => Key_9,
        when 11 => Key_0,
        when 12 => Key_Minus,
        when 13 => Key_Equal,
        when 14 => Key_Backspace,
        when 15 => Key_Tab,
        when 16 => Key_Q,
        when 17 => Key_W,
        when 18 => Key_E,
        when 19 => Key_R,
        when 20 => Key_T,
        when 21 => Key_Y,
        when 22 => Key_U,
        when 23 => Key_I,
        when 24 => Key_O,
        when 25 => Key_P,
        when 26 => Key_Left_Bracket,
        when 27 => Key_Right_Bracket,
        when 28 => Key_Enter,
        when 29 => Key_Left_Ctrl,
        when 30 => Key_A,
        when 31 => Key_S,
        when 32 => Key_D,
        when 33 => Key_F,
        when 34 => Key_G,
        when 35 => Key_H,
        when 36 => Key_J,
        when 37 => Key_K,
        when 38 => Key_L,
        when 39 => Key_Semicolon,
        when 40 => Key_Apostrophe,
        when 41 => Key_Backtick,
        when 42 => Key_Left_Shift,
        when 43 => Key_Backslash,
        when 44 => Key_Z,
        when 45 => Key_X,
        when 46 => Key_C,
        when 47 => Key_V,
        when 48 => Key_B,
        when 49 => Key_N,
        when 50 => Key_M,
        when 51 => Key_Comma,
        when 52 => Key_Period,
        when 53 => Key_Slash,
        when 54 => Key_Right_Shift,
        when 55 => Key_Numpad_Asterisk,
        when 56 => Key_Left_Alt,
        when 57 => Key_Space,
        when 58 => Key_Caps_Lock,
        when 59 => Key_F1,
        when 60 => Key_F2,
        when 61 => Key_F3,
        when 62 => Key_F4,
        when 63 => Key_F5,
        when 64 => Key_F6,
        when 65 => Key_F7,
        when 66 => Key_F8,
        when 67 => Key_F9,
        when 68 => Key_F10,
        when 69 => Key_Num_Lock,
        when 70 => Key_Scroll_Lock,
        when 71 => Key_Numpad_7,
        when 72 => Key_Numpad_8,
        when 73 => Key_Numpad_9,
        when 74 => Key_Numpad_Minus,
        when 75 => Key_Numpad_4,
        when 76 => Key_Numpad_5,
        when 77 => Key_Numpad_6,
        when 78 => Key_Numpad_Plus,
        when 79 => Key_Numpad_1,
        when 80 => Key_Numpad_2,
        when 81 => Key_Numpad_3,
        when 82 => Key_Numpad_0,
        when 83 => Key_Numpad_Period,
        --  84 .. 86
        when 87 => Key_F11,
        when 88 => Key_F12,
        --  89 .. 95
        when 96 => Key_Numpad_Enter,
        when 97 => Key_Right_Ctrl,
        when 98 => Key_Numpad_Slash,
        when 99 => Key_Print_Screen,
        when 100 => Key_Right_Alt,
        --  101
        when 102 => Key_Home,
        when 103 => Key_Arrow_Up,
        when 104 => Key_Page_Up,
        when 105 => Key_Arrow_Left,
        when 106 => Key_Arrow_Right,
        when 107 => Key_End,
        when 108 => Key_Arrow_Down,
        when 109 => Key_Page_Down,
        when 110 => Key_Insert,
        when 111 => Key_Delete,
        --  112 .. 118
        when 119 => Key_Pause,
        --  120 .. 124
        when 125 => Key_Left_Logo,
        when 126 => Key_Right_Logo,
        when others => Key_Unknown);

   procedure Create_Pipe (Result : out Pipe) is
      type File_Descriptor_Array is array (1 .. 2) of Standard.Wayland.File_Descriptor
        with Convention => C;

      function C_Pipe
        (File_Descriptors : in out File_Descriptor_Array) return Integer
      with Import, Convention => C, External_Name => "pipe";

      File_Descriptors : File_Descriptor_Array;
   begin
      if C_Pipe (File_Descriptors) = 0 then
         Result := (Read => File_Descriptors (1), Write => File_Descriptors (2));
      else
         raise Constraint_Error;
      end if;
   end Create_Pipe;

   function Read (Object : in out File) return Ada.Streams.Stream_Element_Array is
      Content : Ada.Streams.Stream_Element_Array (1 .. 1024);

      Result : constant C_Binding.Read_Result := C_Binding.Read (Object.Handle, Content);

      use all type C_Binding.Read_Result_Kind_Id;
   begin
      case Result.Kind_Id is
         when Read_Success =>
            return Content (1 .. Result.Element_Count);
         when End_Of_File_Reached =>
            return Content (1 .. 0);
         when Read_Failure =>
            raise Constraint_Error;
      end case;
   end Read;

   procedure Write (Object : in out File; Value : String) is
      subtype Byte_Array is Ada.Streams.Stream_Element_Array (1 .. Value'Length);

      function Convert is new Ada.Unchecked_Conversion
        (Source => String, Target => Byte_Array);
   begin
      C_Binding.Write (Object.Handle, Convert (Value));
   end Write;

   procedure Close (Object : in out File) is
   begin
      C_Binding.Close (Object.Handle);
   end Close;

end AWT.OS;
