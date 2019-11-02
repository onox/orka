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

package Glfw.Input.Mouse is
   pragma Preelaborate;

   type Button is new Interfaces.C.int range 0 .. 7;

   type Enter_Action is (Leaving, Entering);

   type Cursor_Mode is (Normal, Hidden, Disabled);

   Left_Button   : constant := 0;
   Right_Button  : constant := 1;
   Middle_Button : constant := 2;

   subtype Coordinate is Interfaces.C.double;
   subtype Scroll_Offset is Interfaces.C.double;

   function Raw_Motion_Supported return Boolean;
   --  Return True if the system supports raw mouse motion, False otherwise
   --
   --  Must only be called from the environment task.

private
   for Button'Size use Interfaces.C.int'Size;

   for Enter_Action use (Leaving  => 0,
                         Entering => 1);
   for Enter_Action'Size use C.int'Size;

   for Cursor_Mode use (Normal   => 16#34001#,
                        Hidden   => 16#34002#,
                        Disabled => 16#34003#);
   for Cursor_Mode'Size use Interfaces.C.int'Size;
end Glfw.Input.Mouse;
