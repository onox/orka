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

package body Orka.Inputs.Joysticks.Chords is

   function Create_Chord
     (Buttons : Button_Index_Array;
      Frames  : Positive) return Chord is
   begin
      return
        (Frame_Count  => Frames,
         Button_Count => Buttons'Length,
         Buttons      => Buttons,
         States       => (others => (others => False)),
         Frame_Index  => 1);
   end Create_Chord;

   function Detect_Activation
     (Object   : in out Chord;
      Joystick : Joystick_Input'Class) return Boolean
   is
      Current_State : constant Joystick_State := Joystick.Current_State;
      Pressed_Buttons : Boolean_Button_States := (others => False);
   begin
      Object.States (Object.Frame_Index) := Joystick.Just_Pressed;

      for State of Object.States loop
         Pressed_Buttons := Pressed_Buttons or State;
      end loop;

      if Object.Frame_Index < Object.Frame_Count then
         Object.Frame_Index := Object.Frame_Index + 1;
      else
         Object.Frame_Index := 1;
      end if;

      return Result : constant Boolean :=
        (for all Index of Object.Buttons =>
          Current_State.Buttons (Index) = Pressed and Pressed_Buttons (Index))
      do
         if Result then
            Object.States := (others => (others => False));
         end if;
      end return;
   end Detect_Activation;

end Orka.Inputs.Joysticks.Chords;
