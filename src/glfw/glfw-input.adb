--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2013 Felix Krause <contact@flyx.org>
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

with Ada.Task_Identification;

with Glfw.API;

package body Glfw.Input is

   use Ada.Task_Identification;

   procedure Poll_Events is
      pragma Assert (Current_Task = Environment_Task);
   begin
      API.Poll_Events;
   end Poll_Events;

   procedure Wait_For_Events is
      pragma Assert (Current_Task = Environment_Task);
   begin
      API.Wait_Events;
   end Wait_For_Events;

   procedure Wait_For_Events (Timeout : Seconds) is
      pragma Assert (Current_Task = Environment_Task);
   begin
      API.Wait_Events_Timeout (Timeout);
   end Wait_For_Events;

   procedure Post_Empty_Event renames API.Post_Empty_Event;

end Glfw.Input;
