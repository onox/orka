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

with Wayland;

with AWT.Registry;

package body AWT.Inputs is

   Global : AWT.Registry.Compositor renames AWT.Registry.Global;

   function Keyboard_Has_Focus return Boolean is
      use type Standard.Wayland.Unsigned_32;

      pragma Assert (Global.Seat.Keyboard_State.Focused xor Global.Seat.Keyboard_Enter_Serial = 0);
   begin
      return Global.Seat.Keyboard_State.Focused;
   end Keyboard_Has_Focus;

end AWT.Inputs;
