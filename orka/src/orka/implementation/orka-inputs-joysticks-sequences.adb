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

package body Orka.Inputs.Joysticks.Sequences is

   function Create_Sequence
     (Buttons  : Button_Index_Array;
      Max_Time : Duration) return Sequence is
   begin
      return
        (Button_Count => Buttons'Length,
         Buttons      => Buttons,
         Index        => Buttons'First,
         Max_Time     => Max_Time,
         Start_Press  => Orka.OS.Monotonic_Clock - Max_Time);
   end Create_Sequence;

   function Detect_Activation
     (Object   : in out Sequence;
      Joystick : Joystick_Input'Class) return Boolean
   is
      Current_Time : constant Time := Orka.OS.Monotonic_Clock;

      On_Time : constant Boolean := Current_Time - Object.Start_Press < Object.Max_Time;

      Pressed_Buttons : constant Boolean_Button_States := Joystick.Just_Pressed;
      Expected_Button : Boolean_Button_States := (others => False);
   begin
      Expected_Button (Object.Buttons (Object.Index)) := True;
      declare
         Unexpected_Buttons : constant Boolean_Button_States :=
           Pressed_Buttons and not Expected_Button;
      begin
         --  If the user presses a wrong button, it might actually be
         --  the correct button of the start of the sequence. Therefore,
         --  do not stop, but just reset the sequence
         if (for some Button of Unexpected_Buttons => Button) then
            Object.Index := 1;
         end if;

         if Pressed_Buttons (Object.Buttons (Object.Index)) then
            if Object.Index = 1 then
               Object.Start_Press := Current_Time;
               Object.Index := Object.Index + 1;
            elsif On_Time then
               if Object.Index = Object.Button_Count then
                  Object.Index := 1;
                  return True;
               else
                  Object.Index := Object.Index + 1;
               end if;
            else
               Object.Index := 1;
            end if;
         end if;
      end;

      return False;
   end Detect_Activation;

end Orka.Inputs.Joysticks.Sequences;
