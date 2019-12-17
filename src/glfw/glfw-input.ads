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

package Glfw.Input is
   pragma Preelaborate;

   type Button_State is (Released, Pressed);

   type Input_Toggle is (Sticky_Keys, Sticky_Mouse_Buttons, Raw_Mouse_Motion);

   procedure Poll_Events;
   --  Process events in the event queue
   --
   --  Task safety: Must only be called from the environment task.

   procedure Wait_For_Events;
   --  Wait until at least one event is available and then process
   --  all events in the event queue
   --
   --  Task safety: Must only be called from the environment task.

   procedure Wait_For_Events (Timeout : Seconds);
   --  Wait, for the specified duration, until at least one event is
   --  available and then process all events in the event queue
   --
   --  Task safety: Must only be called from the environment task.

   procedure Post_Empty_Event;
   --  Post an empty event to wake up the task calling procedure
   --  Wait_For_Events
   --
   --  Task safety: May be called from any task.

private

   for Button_State use (Released => 0, Pressed => 1);
   for Button_State'Size use Interfaces.C.int'Size;

   for Input_Toggle use (Sticky_Keys          => 16#33002#,
                         Sticky_Mouse_Buttons => 16#33003#,
                         Raw_Mouse_Motion     => 16#33005#);
   for Input_Toggle'Size use Interfaces.C.int'Size;

   --  Just so we can implement them with rename
   pragma Convention (C, Post_Empty_Event);

end Glfw.Input;
