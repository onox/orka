--  Copyright (c) 2018 onox <denkpadje@gmail.com>
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

package Orka.Logging is

   type Source is (Executor, Game_Loop, Resource_Loader, Other);

   type Severity is (Error, Warning, Info, Debug);

   procedure Insert_Message
     (From       : Source;
      Level      : Severity;
      Identifier : Natural;
      Message    : String);
   --  Generate a new message
   --
   --  Instantiate the generic package Messages below if you need to
   --  print multiple messages with the same source.
   --
   --  The generated messages will be printed in the terminal.

   generic
      From : Source;
   package Messages is
      procedure Insert (Level : Severity; Message : String);
      --  Generate a new debug message and increment the internal identifier

      procedure Reset_Identifier (Value : Natural);
      --  Reset the internal identifier to the given value
   end Messages;

end Orka.Logging;
