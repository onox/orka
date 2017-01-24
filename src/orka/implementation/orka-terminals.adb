--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;

package body Orka.Terminals is

   Style_Codes : constant array (Style) of Natural :=
     (Default   => 0,
      Bold      => 1,
      Dark      => 2,
      Italic    => 3,
      Underline => 4,
      Blink     => 5,
      Reversed  => 7,
      Cross_Out => 9);

   Foreground_Color_Codes : constant array (Color) of Natural :=
     (Default => 0,
      Grey    => 30,
      Red     => 31,
      Green   => 32,
      Yellow  => 33,
      Blue    => 34,
      Magenta => 35,
      Cyan    => 36,
      White   => 37);

   Background_Color_Codes : constant array (Color) of Natural :=
     (Default => 0,
      Grey    => 40,
      Red     => 41,
      Green   => 42,
      Yellow  => 43,
      Blue    => 44,
      Magenta => 45,
      Cyan    => 46,
      White   => 47);

   package L renames Ada.Characters.Latin_1;
   package SF renames Ada.Strings.Fixed;

   Reset : constant String := L.ESC & "[0m";

   function Sequence (Code : Natural) return String is
      Image : constant String := SF.Trim (Natural'Image (Code), Ada.Strings.Both);
   begin
      return (if Code /= 0 then L.ESC & "[" & Image & "m" else "");
   end Sequence;

   function Colorize (Text : String; Foreground, Background : Color := Default;
                      Attribute : Style := Default) return String is
      FG : constant String := Sequence (Foreground_Color_Codes (Foreground));
      BG : constant String := Sequence (Background_Color_Codes (Background));
      ST : constant String := Sequence (Style_Codes (Attribute));
   begin
      return Reset & FG & BG & ST & Text & Reset;
   end Colorize;

end Orka.Terminals;
