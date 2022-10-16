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

package body AWT.Inputs.Gamepads.Tapping is

   function Create_Tap_Detector
     (Button    : Gamepad_Button;
      Max_Delta : Duration) return Button_Tap_Detector
   is
      use type Orka.Time;
   begin
      return
        (Button     => Button,
         Max_Delta  => Max_Delta,
         Last_Press => Orka.OS.Monotonic_Clock - Max_Delta,
         Active     => False);
   end Create_Tap_Detector;

   function Detect_Activation
     (Object : in out Button_Tap_Detector;
      State  : Gamepad_State) return Boolean
   is
      use type Orka.Time;

      Current_Time : constant Orka.Time := Orka.OS.Monotonic_Clock;

      On_Time : constant Boolean := Current_Time - Object.Last_Press < Object.Max_Delta;
   begin
      if State.Pressed (Object.Button) then
         Object.Last_Press := Current_Time;
         Object.Active := On_Time;
      end if;

      return On_Time and Object.Active;
   end Detect_Activation;

end AWT.Inputs.Gamepads.Tapping;
