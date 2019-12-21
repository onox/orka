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

package Orka.Inputs.Joysticks.Chords is
   pragma Preelaborate;

   subtype Chord_Button_Index is Positive range 1 .. 4;

   type Button_Index_Array is array (Chord_Button_Index range <>) of Button_Index;

   type Chord (<>) is tagged private;

   function Create_Chord
     (Buttons : Button_Index_Array;
      Frames  : Positive) return Chord
   with Pre => Buttons'Length >= 2;
   --  Return a chord for the given button indices, which must be pressed
   --  at the same time in one or a few frames
   --
   --  Because humans are not perfect, they may press some buttons of the
   --  chord a few frames after the first one. Therefore, Frames should
   --  be a small number, something between 2 and 4.

   function Detect_Activation
     (Object   : in out Chord;
      Joystick : Joystick_Input'Class) return Boolean;
   --  Return True if all buttons of the chord are currently pressed and
   --  have been pressed in the current or last few frames, False otherwise

private

   type State_Array is array (Positive range <>) of Boolean_Button_States;

   type Chord (Frame_Count, Button_Count : Positive) is tagged record
      Buttons     : Button_Index_Array (1 .. Button_Count);
      States      : State_Array (1 .. Frame_Count);
      Frame_Index : Positive;
   end record;

end Orka.Inputs.Joysticks.Chords;
