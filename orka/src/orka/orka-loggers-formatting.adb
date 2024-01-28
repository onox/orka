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

with Ada.Strings.Fixed;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with Orka.Terminals;

package body Orka.Loggers.Formatting is

   package SF renames Ada.Strings.Fixed;

   package UTF renames Ada.Strings.UTF_Encoding;

   function Unicode (Number : Long_Integer) return UTF.UTF_8_String is
     (UTF.Wide_Wide_Strings.Encode ("" & Wide_Wide_Character'Val (Number)));

   Length_Level  : constant := 7;

   function Format_Message
     (From     : String;
      Level    : Severity;
      Message  : String;
      Colorize : Boolean) return String
   is
      Level_Color : constant Terminals.Color
        := (case Level is
              when Error   => Terminals.Red,
              when Warning => Terminals.Yellow,
              when Failure => Terminals.Red,
              when Success => Terminals.Green,
              when Info    => Terminals.Blue,
              when Debug   => Terminals.Default);

      Level_Icon : constant UTF.UTF_8_String
        := (case Level is
              when Error   => Unicode (16#F06A#),  --  
              when Warning => Unicode (16#F071#),  --  
              when Failure => Unicode (16#F05C#),  --  
              when Success => Unicode (16#F05D#),  --  
              when Info    => Unicode (16#F05A#),  --  
              when Debug   => " ");
              --  Symbols from font FontAwesome

      Level_Image  : String (1 .. Length_Level);

      function Colorize_Text (Text : String; Color : Terminals.Color) return String is
        (if Colorize then Terminals.Colorize (Text, Color) else Text);
   begin
      SF.Move (Level'Image, Level_Image, Justify => Ada.Strings.Center);

      return
        Colorize_Text (Terminals.Time_Image, Terminals.Grey) &
        " " &
        Colorize_Text ("[" & Level_Icon & " " & Level_Image & "]", Level_Color) &
        " " &
        Colorize_Text ("[" & From & "]", Terminals.Magenta) &
        " " & Terminals.Strip_Line_Term (Message);
   end Format_Message;

end Orka.Loggers.Formatting;
