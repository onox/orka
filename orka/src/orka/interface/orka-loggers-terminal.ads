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

package Orka.Loggers.Terminal is
   pragma Preelaborate;

   protected type Logger_Object (Min_Level : Severity) is new Orka.Loggers.Logger with
      overriding
      procedure Log
        (From    : Source;
         Level   : Severity;
         Message : String);
   end Logger_Object;

   Logger : aliased Logger_Object (Min_Level => Debug);
   --  A logger that logs messages to the terminal

   function Create_Logger (Level : Severity := Debug) return Logger_Ptr;
   --  Create and return a logger that only logs messages to the terminal
   --  with the minimum specified severity

end Orka.Loggers.Terminal;
