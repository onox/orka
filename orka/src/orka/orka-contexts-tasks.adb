--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2024 onox <denkpadje@gmail.com>
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

with Ada.Exceptions;

with Orka.Loggers;
with Orka.Logging.Default;

package body Orka.Contexts.Tasks is

   use all type Orka.Logging.Default_Module;
   use all type Orka.Logging.Severity;

   procedure Log is new Orka.Logging.Default.Generic_Log (Resource_Loader);

   task body Render_To_Window_Task is
      Context : Orka.Contexts.Surface_Context_Access;
   begin
      accept Move_Context
        (Context : not null Orka.Contexts.Surface_Context_Access;
         Window  : in out Orka.Windows.Window'Class)
      do
         Context.Make_Current (Window);
         Render_To_Window_Task.Context := Context;
      end Move_Context;

      Handler.all;
      Context.Make_Not_Current;
   exception
      when Error : others =>
         Log (Orka.Loggers.Error, "Error: " & Ada.Exceptions.Exception_Information (Error));
         Context.Make_Not_Current;
         raise;
   end Render_To_Window_Task;

   task body Render_Task is
      Context : Orka.Contexts.Context_Access;
   begin
      accept Move_Context (Context : not null Orka.Contexts.Context_Access) do
         Context.Make_Current;
         Render_Task.Context := Context;
      end Move_Context;

      Handler.all;
      Context.Make_Not_Current;
   exception
      when Error : others =>
         Log (Orka.Loggers.Error, "Error: " & Ada.Exceptions.Exception_Information (Error));
         Context.Make_Not_Current;
         raise;
   end Render_Task;


end Orka.Contexts.Tasks;
