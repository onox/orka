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

package AWT.Inputs.Gamepads.Chords is
   pragma Preelaborate;

   subtype Chord_Button_Index is Positive range 1 .. 4;

   type Button_Index_Array is array (Chord_Button_Index range <>) of Gamepad_Button;

   type Chord (<>) is tagged private;

   function Create_Chord
     (Buttons  : Button_Index_Array;
      Max_Time : Duration) return Chord
   with Pre => Buttons'Length >= 2;
   --  Return a chord for the given button indices, which must be pressed
   --  at the same time within small duration
   --
   --  Because humans are not perfect, they may press some buttons of the
   --  chord a little bit later after the first one. Therefore, Max_Time should
   --  be a small duration, something between 30 and 80 ms.

   function Detect_Activation
     (Object : in out Chord;
      State  : Gamepad_State) return Boolean;
   --  Return True if all buttons of the chord are currently pressed and
   --  were pressed within the maximum duration before now, False otherwise

private

   type State_Array is array (Positive range <>) of Changed_Gamepad_Buttons;

   type State_Type is (Idle, Active, Deactivated);

   type Chord (Button_Count : Positive) is tagged record
      Buttons       : Button_Index_Array (1 .. Button_Count);
      Max_Time      : Duration;
      Current_State : State_Type := Idle;
      Start_Press   : Orka.Time;
   end record;

end AWT.Inputs.Gamepads.Chords;
