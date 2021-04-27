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

with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Strings.Unbounded;

with Orka.Containers.Ring_Buffers;
with Orka.Loggers.Formatting;
with Orka.Loggers.Terminal;
with Orka.OS;

package body Orka.Loggers.Location is

   package L  renames Ada.Characters.Latin_1;
   package SU renames Ada.Strings.Unbounded;

   type Log_Request is record
      Path    : SU.Unbounded_String;
      Message : SU.Unbounded_String;
   end record;

   package Buffers is new Orka.Containers.Ring_Buffers (Log_Request);

   protected Queue is
      procedure Enqueue
        (Path    : SU.Unbounded_String;
         From    : Source;
         Kind    : Message_Type;
         Level   : Severity;
         ID      : Natural;
         Message : String);

      entry Dequeue (Request : out Log_Request; Stop : out Boolean);

      procedure Shutdown;
   private
      Messages    : Buffers.Buffer (Capacity_Queue);

      Should_Stop : Boolean := False;
      Has_Stopped : Boolean := False;
   end Queue;

   protected body Queue is
      procedure Enqueue
        (Path    : SU.Unbounded_String;
         From    : Source;
         Kind    : Message_Type;
         Level   : Severity;
         ID      : Natural;
         Message : String) is
      begin
         if not Messages.Is_Full and not Has_Stopped then
            Messages.Add_Last
              ((Path    => Path,
                Message => SU.To_Unbounded_String
                  (Formatting.Format_Message_No_Color (From, Kind, Level, ID, Message) & L.LF)));
         else
            Orka.Loggers.Terminal.Logger.Log (From, Kind, Level, ID, Message);
         end if;
      end Enqueue;

      entry Dequeue
        (Request : out Log_Request;
         Stop    : out Boolean) when not Messages.Is_Empty or else Should_Stop is
      begin
         Stop := Should_Stop and Messages.Is_Empty;
         if Stop then
            Has_Stopped := True;
            return;
         end if;

         Request := Messages.Remove_First;
      end Dequeue;

      procedure Shutdown is
      begin
         Should_Stop := True;
      end Shutdown;
   end Queue;

   procedure Shutdown is
   begin
      Queue.Shutdown;
   end Shutdown;

   task Logger_Task;

   task body Logger_Task is
      Name : String renames Task_Name;

      Request : Log_Request;
      Stop    : Boolean;
   begin
      Orka.OS.Set_Task_Name (Name);

      loop
         Queue.Dequeue (Request, Stop);

         exit when Stop;

         Location.Append_Data
           (Path => SU.To_String (Request.Path),
            Data => Orka.Resources.Convert (SU.To_String (Request.Message)));
      end loop;
   exception
      when Error : others =>
         Orka.OS.Put_Line (Name & ": " & Ada.Exceptions.Exception_Information (Error));
   end Logger_Task;

   protected type Location_Logger (Min_Level : Severity) is new Logger with
      overriding
      procedure Log
        (From    : Source;
         Kind    : Message_Type;
         Level   : Severity;
         ID      : Natural;
         Message : String);

      procedure Set_Path (Path : String);
   private
      File_Path : SU.Unbounded_String;
   end Location_Logger;

   protected body Location_Logger is
      procedure Log
        (From    : Source;
         Kind    : Message_Type;
         Level   : Severity;
         ID      : Natural;
         Message : String) is
      begin
         if Level <= Min_Level then
            Queue.Enqueue (File_Path, From, Kind, Level, ID, Message);
         end if;
      end Log;

      procedure Set_Path (Path : String) is
      begin
         File_Path := SU.To_Unbounded_String (Path);
      end Set_Path;
   end Location_Logger;

   function Create_Logger (Path : String; Level : Severity := Debug) return Logger_Ptr is
   begin
      return Result : constant Logger_Ptr := new Location_Logger (Min_Level => Level) do
         Location_Logger (Result.all).Set_Path (Path);
      end return;
   end Create_Logger;

end Orka.Loggers.Location;
