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

private with Ada.Real_Time;

package Orka.Inputs.Joysticks.Tapping is

   type Button_Tap_Detector is tagged private;

   function Create_Tap_Detector
     (Button    : Button_Index;
      Max_Delta : Duration) return Button_Tap_Detector;
   --  Return a button tap detector for the given button index

   function Detect_Activation
     (Object   : in out Button_Tap_Detector;
      Joystick : Joystick_Input'Class) return Boolean;
   --  Return True if the button is tapped rapidly, False otherwise

private

   type Button_Tap_Detector is tagged record
      Button     : Button_Index;
      Max_Delta  : Ada.Real_Time.Time_Span;
      Last_Press : Ada.Real_Time.Time;
      Active     : Boolean;
   end record;

end Orka.Inputs.Joysticks.Tapping;
