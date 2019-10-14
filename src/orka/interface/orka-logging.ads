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

with Ada.Real_Time;

with GL.Debug;

package Orka.Logging is

   type Source is
     (Worker, Game_Loop, Resource_Loader,
      OpenGL, Window_System, Shader_Compiler, Third_Party, Application, Other);

   type Message_Type is new GL.Debug.Message_Type;

   type Severity is (Error, Warning, Info, Debug);

   -----------------------------------------------------------------------------

   function Image (Value : Ada.Real_Time.Time_Span) return String;
   --  Return the image of the given duration with an appropriate suffix

   function Trim (Value : String) return String;

   -----------------------------------------------------------------------------

   procedure Log
     (From    : Source;
      Kind    : Message_Type;
      Level   : Severity;
      ID      : Natural;
      Message : String);
   --  Log the message to the terminal

   generic
      From : Source;
      ID   : Natural := 0;
   package Messages is
      procedure Log (Level : Severity; Message : String);
   end Messages;

end Orka.Logging;
