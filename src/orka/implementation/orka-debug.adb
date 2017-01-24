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

with Ada.Calendar.Formatting;
with Ada.Strings.Fixed;

with Orka.Terminals;

package body Orka.Debug is

   package SF renames Ada.Strings.Fixed;

   function Time_Image return String is
      use Ada.Calendar;
      use Ada.Calendar.Formatting;

      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration;
   begin
      Split (Seconds (Clock), Hour, Minute, Second, Sub_Second);

      declare
         --  Remove first character (space) from ' hhmmss' image and then pad it to six digits
         Image1 : constant String := Natural'Image (Hour * 1e4 + Minute * 1e2 + Second);
         Image2 : constant String := SF.Tail (Image1 (2 .. Image1'Last), 6, '0');

         --  Insert ':' characters to get 'hh:mm:ss'
         Image3 : constant String := SF.Insert (Image2, 5, ":");
         Image4 : constant String := SF.Insert (Image3, 3, ":");

         --  Take image without first character (space) and then pad it to six digits
         Image5 : constant String := Natural'Image (Natural (Sub_Second * 1e6));
         Image6 : constant String := SF.Tail (Image5 (2 .. Image5'Last), 6, '0');
      begin
         return Image4 & "." & Image6;
      end;
   end Time_Image;

   function Format_Message
     (From    : Source;
      Kind    : Message_Type;
      ID      : GL.Types.UInt;
      Level   : Severity;
      Message : String) return String
   is
      Level_Color : constant Terminals.Color
        := (case Level is
               when High         => Terminals.Red,
               when Medium       => Terminals.Yellow,
               when Low          => Terminals.Blue,
               when Notification => Terminals.Green);

      Level_Image  : String renames Severity'Image (Level);
      Type_Image   : String renames Message_Type'Image (Kind);
      Source_Image : String renames Source'Image (From);
      ID_Image     : String renames GL.Types.UInt'Image (ID);
   begin
      return Terminals.Colorize ("[" & Time_Image & " " & Level_Image & "]", Level_Color) &
             " " &
             Terminals.Colorize ("[" & Source_Image & ":" & Type_Image & "]", Terminals.Magenta) &
             ID_Image & ": " &
             Message;
   end Format_Message;

end Orka.Debug;
