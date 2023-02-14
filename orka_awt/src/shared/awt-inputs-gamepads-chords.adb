--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2019 onox <denkpadje@gmail.com>
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

with Orka.OS;

package body AWT.Inputs.Gamepads.Chords is

   function Create_Chord
     (Buttons  : Button_Index_Array;
      Max_Time : Duration) return Chord
   is
      use type Orka.Time;
   begin
      return
        (Button_Count  => Buttons'Length,
         Buttons       => Buttons,
         Current_State => Idle,
         Max_Time      => Max_Time,
         Start_Press   => Orka.OS.Monotonic_Clock - Max_Time);
   end Create_Chord;

   function Detect_Activation
     (Object : in out Chord;
      State  : Gamepad_State) return Boolean
   is
      use type Orka.Time;

      Current_Time : constant Orka.Time := Orka.OS.Monotonic_Clock;

      Pressed_Buttons : Natural := 0;
   begin
      for Button of Object.Buttons loop
         if State.Buttons (Button) = Pressed then
            Pressed_Buttons := Pressed_Buttons + 1;
         end if;
      end loop;

      if Pressed_Buttons = 0 then
         Object.Current_State := Idle;
      end if;

      --  Record the time only for the first button that got pressed
      if Object.Current_State = Idle and Pressed_Buttons > 0 then
         Object.Start_Press   := Current_Time;
         Object.Current_State := Active;
      end if;

      --  If any (but not all) button, which was previously pressed, was released,
      --  then deactivate the chord and require that all buttons are released before
      --  the chord can be activated again
      if Object.Current_State = Active and then
        (for some Button of Object.Buttons => State.Released (Button))
      then
         Object.Current_State := Deactivated;
      end if;

      declare
         On_Time : constant Boolean := Current_Time - Object.Start_Press < Object.Max_Time;
      begin
         return Result : constant Boolean :=
           On_Time and Object.Current_State = Active and Pressed_Buttons = Object.Button_Count
         do
            if Result then
               Object.Current_State := Deactivated;
            end if;
         end return;
      end;
   end Detect_Activation;

end AWT.Inputs.Gamepads.Chords;
