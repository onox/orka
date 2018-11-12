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

with Ada.Text_IO;

with GL.Debug.Logs;

with Orka.Terminals;

package body Orka.Debug is

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

      Time_Image : constant String := Terminals.Time_Image;
   begin
      return Terminals.Colorize ("[" & Time_Image & " " & Level'Image & "]", Level_Color) &
             " " &
             Terminals.Colorize ("[" & From'Image & ":" & Kind'Image & "]", Terminals.Magenta) &
             ID'Image & ": " & Terminals.Strip_Line_Term (Message);
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
