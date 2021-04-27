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

private with Orka.OS;

package AWT.Wayland.Windows.Cursors is
   pragma Preelaborate;

   type Animated_Cursor_Window is abstract limited new Wayland_Window with private;

private

   type Animated_Cursor_Window is abstract limited new Wayland_Window with record
      Start_Time, Next_Time : Duration := Orka.OS.Monotonic_Clock;
   end record;

   overriding
   function On_Change_Cursor
     (Object : in out Animated_Cursor_Window;
      Name   : AWT.Inputs.Cursors.Pointer_Cursor;
      Cursor : WC.Cursor'Class) return WC.Cursor_Image'Class;

   overriding
   procedure Update_Cursor (Object : in out Animated_Cursor_Window);

end AWT.Wayland.Windows.Cursors;
