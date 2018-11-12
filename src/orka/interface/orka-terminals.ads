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

private package Orka.Terminals is

   type Style is (Default, Bold, Dark, Italic, Underline, Blink, Reversed, Cross_Out);

   type Color is (Default, Grey, Red, Green, Yellow, Blue, Magenta, Cyan, White);

   function Colorize (Text : String; Foreground, Background : Color := Default;
                      Attribute : Style := Default) return String;
   --  Colorize the given text with a foreground color, background color,
   --  and/or style using ANSI escape sequences

   function Time_Image return String;

   function Strip_Line_Term (Value : String) return String;

end Orka.Terminals;
