--  SPDX-License-Identifier: Apache-2.0
--
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

with GL.Errors;
with GL.Debug.Logs;
with GL.Toggles;

with Orka.Loggers;
with Orka.Logging;

package body Orka.Debug is

   use GL.Debug;

   Has_Seen_API_Error : Boolean := False
     with Volatile => True;

   Debug_Synchronous : Boolean := False
     with Volatile => True;

   procedure Log_Debug_Message
     (From      : Source;
      Kind      : Message_Type;
      Level     : Severity;
      ID        : Unsigned_32;
      Message   : String)
   is
      use all type Orka.Loggers.Source;
      use all type Orka.Loggers.Severity;

      Source : constant Orka.Loggers.Source
        := (case From is
               when OpenGL          => OpenGL,
               when Window_System   => Window_System,
               when Shader_Compiler => Shader_Compiler,
               when Third_Party     => Third_Party,
               when Application     => Application,
               when Other           => Other);

      Severity : constant Orka.Loggers.Severity
        := (case Level is
               when High         => Loggers.Error,
               when Medium       => Loggers.Warning,
               when Low          => Loggers.Info,
               when Notification => Loggers.Debug);
   begin
      if Debug_Synchronous and then Has_Seen_API_Error then
         Has_Seen_API_Error := False;
         GL.Errors.Raise_Exception_On_OpenGL_Error;
      end if;

      Logging.Log (Source, Loggers.Message_Type (Kind), Severity, Message);

      if Debug_Synchronous and then (Source = OpenGL and Kind = Error) then
         GL.Errors.Raise_Exception_On_OpenGL_Error;
         Has_Seen_API_Error := True;
      end if;
   end Log_Debug_Message;

   procedure Set_Log_Messages (Enable : Boolean; Raise_API_Error : Boolean := False) is
   begin
      if Enable then
         --  Enable synchronous output to prevent interleaving messages while
         --  flushing the log below
         GL.Toggles.Enable (GL.Toggles.Debug_Output_Synchronous);
         Debug_Synchronous := Raise_API_Error;

         GL.Toggles.Enable (GL.Toggles.Debug_Output);
         GL.Debug.Set_Message_Callback (Log_Debug_Message'Access);
         GL.Debug.Set (GL.Debug.Low, True);

         --  At this point, a callback has been set, which guarantees that no
         --  new messages will be stored in the log while we are flushing it
         declare
            Count     : constant Natural := Natural (GL.Debug.Logs.Logged_Messages);
            Remaining :          Natural := Count;
         begin
            if Count > 0 then
               Log_Debug_Message (Application, Other, Notification, 0,
                 "Flushing" & Count'Image & " messages in the debug log:");

               --  Use a loop because GL may fetch less messages than stored in the log
               while Remaining > 0 loop
                  declare
                     Messages : constant GL.Debug.Logs.Message_Array := GL.Debug.Logs.Message_Log;
                  begin
                     for M of Messages loop
                        Log_Debug_Message (M.From, M.Kind, M.Level, M.ID, M.Message.Element);
                     end loop;
                     Remaining := Remaining - Messages'Length;
                  end;
               end loop;

               Log_Debug_Message (Application, Other, Notification, 0, "End of messages in log");
            end if;
         end;

         GL.Toggles.Set (GL.Toggles.Debug_Output_Synchronous,
           (if Raise_API_Error then GL.Toggles.Enabled else GL.Toggles.Disabled));
      else
         GL.Toggles.Disable (GL.Toggles.Debug_Output);
         GL.Debug.Disable_Message_Callback;
      end if;
   end Set_Log_Messages;

end Orka.Debug;
