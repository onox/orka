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

package body AWT.Wayland.Windows.Cursors is

   overriding
   function On_Change_Cursor
     (Object : in out Animated_Cursor_Window;
      Name   : AWT.Inputs.Cursors.Pointer_Cursor;
      Cursor : WC.Cursor'Class) return WC.Cursor_Image'Class
   is
      use all type AWT.Inputs.Cursors.Pointer_Cursor;

      Current_Time : constant Duration := Orka.OS.Monotonic_Clock;
   begin
      if Name /= Object.Cursor then
         Object.Start_Time := Current_Time;
      end if;

      declare
         Elapsed_Time : constant Duration := Current_Time - Object.Start_Time;
         Remaining    : Duration;

         Index : constant WC.Image_Index :=
           Cursor.Index_At_Elapsed_Time (Elapsed_Time, Remaining);
      begin
         Object.Next_Time := Current_Time + Remaining;
         --  TODO Use timerfd to make sure Global.Display.Check_For_Events
         --  in AWT.Registry.Process_Events returns at Object.Next_Time?
         return Result : WC.Cursor_Image'Class := Cursor.Image (Index);
      end;
   end On_Change_Cursor;

   overriding
   procedure Update_Cursor (Object : in out Animated_Cursor_Window) is
   begin
      if Orka.OS.Monotonic_Clock > Object.Next_Time then
         Object.Set_Pointer_Cursor (Object.Cursor);
      end if;
   end Update_Cursor;

end AWT.Wayland.Windows.Cursors;
