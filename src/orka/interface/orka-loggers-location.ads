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

with Orka.Resources.Locations;

generic
   Location : Orka.Resources.Locations.Writable_Location_Ptr;

   Capacity_Queue : Positive;
   --  Capacity of the message queue
   --
   --  A background task will dequeue messages from this queue
   --  and write it to the file at the given path by the logger.
   --
   --  Messages that cannot be enqueued will be printed to the terminal.

   Task_Name : String := "File Logger";
package Orka.Loggers.Location is

   function Create_Logger (Path : String; Level : Severity := Debug) return Logger_Ptr
     with Pre => Path'Length > 0;
   --  Return a logger that logs messages to a writable location

   procedure Shutdown;
   --  Shutdown the background task that logs the messages
   --
   --  The task will terminate after it has dequeued the remaining
   --  messages in the queue. Any system that tries to log new messages
   --  after the task has terminated will log these messages to the
   --  terminal.

end Orka.Loggers.Location;
