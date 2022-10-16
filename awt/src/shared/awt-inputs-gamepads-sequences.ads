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

package AWT.Inputs.Gamepads.Sequences is

   subtype Sequence_Button_Index is Positive range 1 .. 16;

   type Button_Index_Array is array (Sequence_Button_Index range <>) of Gamepad_Button;

   type Sequence (<>) is tagged private;

   function Create_Sequence
     (Buttons  : Button_Index_Array;
      Max_Time : Duration) return Sequence
   with Pre => Buttons'Length >= 2;
   --  Return a sequence for the given button indices, which must be
   --  pressed one after another within a certain duration

   function Detect_Activation
     (Object : in out Sequence;
      State  : Gamepad_State) return Boolean;
   --  Return True if the buttons in the sequence have been pressed,
   --  False otherwise

private

   type Sequence (Button_Count : Positive) is tagged record
      Buttons     : Button_Index_Array (1 .. Button_Count);
      Index       : Sequence_Button_Index;
      Max_Time    : Duration;
      Start_Press : Orka.Time;
   end record;

end AWT.Inputs.Gamepads.Sequences;
