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

   type Sticky_Toggle is (Sticky_Keys, Sticky_Mouse_Buttons);

   procedure Poll_Events;
   procedure Wait_For_Events;

private

   for Button_State use (Released => 0, Pressed => 1);
   for Button_State'Size use Interfaces.C.int'Size;

   for Sticky_Toggle use (Sticky_Keys          => 16#33002#,
                          Sticky_Mouse_Buttons => 16#33003#);
   for Sticky_Toggle'Size use Interfaces.C.int'Size;

   --  Just so we can implement them with rename
   pragma Convention (C, Poll_Events);
   pragma Convention (C, Wait_For_Events);

end Glfw.Input;
