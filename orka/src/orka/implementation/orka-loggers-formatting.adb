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

with Orka.Terminals;

package body Orka.Loggers.Formatting is

   function Format_Message
     (From     : Source;
      Kind     : Message_Type;
      Level    : Severity;
      Message  : String;
      Colorize : Boolean) return String
   is
      Level_Color : constant Terminals.Color
        := (case Level is
              when Error   => Terminals.Red,
              when Warning => Terminals.Yellow,
              when Info    => Terminals.Blue,
              when Debug   => Terminals.Green);

      Time_Level : constant String := "[" & Terminals.Time_Image & " " & Level'Image & "]";
      From_Kind  : constant String := "[" & From'Image & ":" & Kind'Image & "]";
   begin
      if Colorize then
         return
           Terminals.Colorize (Time_Level, Level_Color) &
           " " &
           Terminals.Colorize (From_Kind, Terminals.Magenta) &
           " " & Terminals.Strip_Line_Term (Message);
      else
         return Time_Level & " " & From_Kind & " " & Terminals.Strip_Line_Term (Message);
      end if;
   end Format_Message;

end Orka.Loggers.Formatting;
