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

with Orka.Loggers.Formatting;
with Orka.OS;

package body Orka.Loggers.Terminal is

   protected body Logger_Object is
      procedure Log
        (From    : Source;
         Level   : Severity;
         Message : String)
      is
         use all type Orka.OS.File_Kind;
      begin
         if Level <= Min_Level then
            Orka.OS.Put_Line
              (Formatting.Format_Message (From, Level, Message, Colorize => True),
               (if Level = Error then Standard_Error else Standard_Output));
         end if;
      end Log;
   end Logger_Object;

   function Create_Logger (Level : Severity := Debug) return Logger_Ptr is
   begin
      return new Logger_Object (Min_Level => Level);
   end Create_Logger;

end Orka.Loggers.Terminal;
