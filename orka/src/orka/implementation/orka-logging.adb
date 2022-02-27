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

with Ada.Finalization;

with GL.Debug;

with Orka.Loggers.Terminal;
with Orka.OS;
with Orka.Terminals;

package body Orka.Logging is

   function Image (Value : Duration) return String renames Terminals.Image;

   function Trim (Value : String) return String renames Terminals.Trim;

   function Image (Value : Orka.Transforms.Singles.Vectors.Vector4) return String is
   begin
      return "(" &
        Trim (Value (X)'Image) & ", " &
        Trim (Value (Y)'Image) & ", " &
        Trim (Value (Z)'Image) & ", " &
        Trim (Value (W)'Image) &
      ")";
   end Image;

   function Image (Value : Orka.Transforms.Doubles.Vectors.Vector4) return String is
   begin
      return "(" &
        Trim (Value (X)'Image) & ", " &
        Trim (Value (Y)'Image) & ", " &
        Trim (Value (Z)'Image) & ", " &
        Trim (Value (W)'Image) &
      ")";
   end Image;

   -----------------------------------------------------------------------------

   type Clock_Initializer is new Ada.Finalization.Limited_Controlled with null record;

   overriding
   procedure Initialize (Object : in out Clock_Initializer) is
   begin
      Orka.Terminals.Set_Time_Zero (Orka.OS.Monotonic_Clock);
   end Initialize;

   Initialize_Clock : Clock_Initializer;

   -----------------------------------------------------------------------------

   Current_Logger : Loggers.Logger_Ptr := Orka.Loggers.Terminal.Logger'Access;

   procedure Set_Logger (Logger : Loggers.Logger_Ptr) is
   begin
      Current_Logger := Logger;
   end Set_Logger;

   procedure Log
     (From    : Source;
      Kind    : Message_Type;
      Level   : Severity;
      Message : String) is
   begin
      Current_Logger.Log (From, Kind, Level, Message);
   end Log;

   procedure Generic_Log (Level : Severity; Message : String) is
      Kind : constant Message_Type := Message_Type (GL.Debug.Message_Type'(GL.Debug.Other));
   begin
      Orka.Logging.Log (From, Kind, Level, Message);
   end Generic_Log;

end Orka.Logging;
