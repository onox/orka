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

package AWT.Inputs.Gamepads.Tapping is

   type Button_Tap_Detector is tagged private;

   function Create_Tap_Detector
     (Button    : Gamepad_Button;
      Max_Delta : Duration) return Button_Tap_Detector;
   --  Return a button tap detector for the given button

   function Detect_Activation
     (Object : in out Button_Tap_Detector;
      State  : Gamepad_State) return Boolean;
   --  Return True if the button is tapped rapidly, False otherwise

private

   type Button_Tap_Detector is tagged record
      Button     : Gamepad_Button;
      Max_Delta  : Duration;
      Last_Press : Orka.Time;
      Active     : Boolean;
   end record;

end AWT.Inputs.Gamepads.Tapping;
