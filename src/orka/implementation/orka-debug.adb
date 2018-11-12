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
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GL.Debug.Logs;

with Orka.Terminals;

package body Orka.Debug is

   package SF renames Ada.Strings.Fixed;

   package L renames Ada.Characters.Latin_1;

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

   function Strip_Line_Term (Value : String) return String is
      Last_Index : Natural := Value'Last;
   begin
      for Index in reverse Value'Range loop
         exit when Value (Index) not in L.LF | L.CR;
         Last_Index := Last_Index - 1;
      end loop;
      return Value (Value'First .. Last_Index);
   end Strip_Line_Term;

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
   begin
      return Terminals.Colorize ("[" & Time_Image & " " & Level'Image & "]", Level_Color) &
             " " &
             Terminals.Colorize ("[" & From'Image & ":" & Kind'Image & "]", Terminals.Magenta) &
             ID'Image & ": " & Strip_Line_Term (Message);
   end Format_Message;

   function Logged_Messages return Natural is (Natural (GL.Debug.Logs.Logged_Messages));

   procedure Flush_Log is
   begin
      for M of GL.Debug.Logs.Message_Log loop
         Ada.Text_IO.Put_Line
           (Format_Message (M.From, M.Kind, M.ID, M.Level, M.Message.Element));
      end loop;
   end Flush_Log;

   procedure Print_Debug_Message
     (From      : Source;
      Kind      : Message_Type;
      Level     : Severity;
      ID        : GL.Types.UInt;
      Message   : String) is
   begin
      Ada.Text_IO.Put_Line (Format_Message (From, Kind, ID, Level, Message));
   end Print_Debug_Message;

   procedure Enable_Print_Callback is
   begin
      GL.Debug.Set_Message_Callback (Print_Debug_Message'Access);
      GL.Debug.Set (GL.Debug.Low, True);
   end Enable_Print_Callback;

end Orka.Debug;
