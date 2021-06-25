--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2021 onox <denkpadje@gmail.com>
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

with AWT.Registry;

package body AWT.Monitors is

   procedure Log_Information (Monitor : AWT.Monitors.Monitor'Class) is separate;

   function Monitors return AWT.Monitors.Monitor_Array renames AWT.Registry.Monitors;

   overriding procedure Initialize (Object : in out Monitor_Event_Listener) is
   begin
      if AWT.Registry.Monitor_Listener /= null then
         raise Program_Error;
      end if;

      AWT.Registry.Monitor_Listener := Object'Unchecked_Access;
   end Initialize;

   overriding procedure Finalize (Object : in out Monitor_Event_Listener) is
   begin
      if AWT.Registry.Monitor_Listener /= null
        and AWT.Registry.Monitor_Listener /= Object'Unchecked_Access
      then
         raise Program_Error;
      end if;

      AWT.Registry.Monitor_Listener := null;
   end Finalize;

end AWT.Monitors;
