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

with Ada.Text_IO;

with Orka.Loggers.Formatting;

package body Orka.Loggers.Terminal is

   protected type Logger_Object (Min_Level : Severity) is new Orka.Loggers.Logger with
      overriding
      procedure Log
        (From    : Source;
         Kind    : Message_Type;
         Level   : Severity;
         ID      : Natural;
         Message : String);
   end Logger_Object;

   protected body Logger_Object is
      procedure Log
        (From    : Source;
         Kind    : Message_Type;
         Level   : Severity;
         ID      : Natural;
         Message : String)
      is
         package IO renames Ada.Text_IO;
      begin
         if Level <= Min_Level then
            IO.Put_Line ((if Level = Error then IO.Standard_Error else IO.Standard_Output),
              Formatting.Format_Message (From, Kind, Level, ID, Message));
         end if;
      end Log;
   end Logger_Object;

   Default_Logger : aliased Logger_Object (Min_Level => Debug);

   function Logger return Logger_Ptr is (Default_Logger'Access);

   function Create_Logger (Level : Severity := Debug) return Logger_Ptr is
   begin
      return new Logger_Object (Min_Level => Level);
   end Create_Logger;

end Orka.Loggers.Terminal;
