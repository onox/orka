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

package body Orka.Inputs.Joysticks.Tapping is

   use Ada.Real_Time;

   function Create_Tap_Detector
     (Button    : Button_Index;
      Max_Delta : Duration) return Button_Tap_Detector
   is
      DT : constant Time_Span := To_Time_Span (Max_Delta);
   begin
      return
        (Button     => Button,
         Max_Delta  => DT,
         Last_Press => Clock - DT,
         Active     => False);
   end Create_Tap_Detector;

   function Detect_Activation
     (Object   : in out Button_Tap_Detector;
      Joystick : Joystick_Input'Class) return Boolean
   is
      Current_Time : constant Time := Clock;

      On_Time : constant Boolean := Current_Time - Object.Last_Press < Object.Max_Delta;
   begin
      if Joystick.Just_Pressed (Object.Button) then
         Object.Last_Press := Current_Time;
         Object.Active := On_Time;
      end if;

      return On_Time and Object.Active;
   end Detect_Activation;

end Orka.Inputs.Joysticks.Tapping;
