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

with Ada.Unchecked_Conversion;

package body Orka.Inputs.Joysticks is

   function Convert is new Ada.Unchecked_Conversion
     (Source => Joystick_Button_States, Target => Boolean_Button_States);

   function Just_Pressed (Object : Joystick_Input'Class) return Boolean_Button_States is
      Changed_Buttons : constant Boolean_Button_States :=
        Convert (Object.Current_State.Buttons) xor Convert (Object.Last_State.Buttons);
   begin
      return Changed_Buttons and Convert (Object.Current_State.Buttons);
   end Just_Pressed;

   function Just_Released (Object : Joystick_Input'Class) return Boolean_Button_States is
      Changed_Buttons : constant Boolean_Button_States :=
        Convert (Object.Current_State.Buttons) xor Convert (Object.Last_State.Buttons);
   begin
      return Changed_Buttons and not Convert (Object.Current_State.Buttons);
   end Just_Released;

end Orka.Inputs.Joysticks;
