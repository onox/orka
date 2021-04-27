--  SPDX-License-Identifier: Apache-2.0
--
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

with Orka.Loggers;
with Orka.Transforms.Singles.Vectors;
with Orka.Transforms.Doubles.Vectors;

package Orka.Logging is
   pragma Preelaborate;

   subtype Source is Loggers.Source;

   subtype Message_Type is Loggers.Message_Type;

   subtype Severity is Loggers.Severity;

   -----------------------------------------------------------------------------

   function Image (Value : Duration) return String;
   --  Return the image of the given duration with an appropriate suffix

   function Image (Value : Orka.Transforms.Singles.Vectors.Vector4) return String;
   function Image (Value : Orka.Transforms.Doubles.Vectors.Vector4) return String;

   function Trim (Value : String) return String;

   -----------------------------------------------------------------------------

   procedure Set_Logger (Logger : Loggers.Logger_Ptr);

   procedure Log
     (From    : Source;
      Kind    : Message_Type;
      Level   : Severity;
      Message : String);
   --  Log the message using the logger
   --
   --  If no logger has been set, it will log the message to the terminal.

   generic
      From : Source;
   package Messages is
      procedure Log (Level : Severity; Message : String);
   end Messages;

end Orka.Logging;
