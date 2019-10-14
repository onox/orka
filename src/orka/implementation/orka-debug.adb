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

with GL.Debug.Logs;
with GL.Types;

with Orka.Logging;

package body Orka.Debug is

   use GL.Debug;

   procedure Log_Debug_Message
     (From      : Source;
      Kind      : Message_Type;
      Level     : Severity;
      ID        : GL.Types.UInt;
      Message   : String)
   is
      use all type Orka.Logging.Source;
      use all type Orka.Logging.Severity;

      Source : constant Orka.Logging.Source
        := (case From is
               when OpenGL          => OpenGL,
               when Window_System   => Window_System,
               when Shader_Compiler => Shader_Compiler,
               when Third_Party     => Third_Party,
               when Application     => Application,
               when Other           => Other);

      Severity : constant Orka.Logging.Severity
        := (case Level is
               when High         => Logging.Error,
               when Medium       => Logging.Warning,
               when Low          => Logging.Info,
               when Notification => Logging.Debug);
   begin
      Logging.Log (Source, Logging.Message_Type (Kind), Severity, Natural (ID), Message);
   end Log_Debug_Message;

   procedure Set_Log_Messages (Enable : Boolean) is
   begin
      if Enable then
         declare
            Count : constant Natural := Natural (GL.Debug.Logs.Logged_Messages);
         begin
            if Count > 0 then
               Log_Debug_Message (Application, Other, Notification, 0,
                 "Flushing" & Count'Image & " messages in the debug log:");
            end if;
         end;

         for M of GL.Debug.Logs.Message_Log loop
            Log_Debug_Message (M.From, M.Kind, M.Level, M.ID, M.Message.Element);
         end loop;

         GL.Debug.Set_Message_Callback (Log_Debug_Message'Access);
         GL.Debug.Set (GL.Debug.Low, True);
      else
         GL.Debug.Disable_Message_Callback;
      end if;
   end Set_Log_Messages;

end Orka.Debug;
