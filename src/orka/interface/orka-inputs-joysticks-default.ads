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

package Orka.Inputs.Joysticks.Default is
   pragma Preelaborate;

   type Abstract_Joystick_Input is abstract new Joystick_Input with private;

   overriding
   function Current_State
     (Object : Abstract_Joystick_Input) return Inputs.Joysticks.Joystick_State;

   overriding
   function Last_State
     (Object : Abstract_Joystick_Input) return Inputs.Joysticks.Joystick_State;

   overriding
   procedure Update_State
     (Object  : in out Abstract_Joystick_Input;
      Process : access procedure (Value : in out Axis_Position;
                                  Index :        Positive));

   function State
     (Object : in out Abstract_Joystick_Input) return Joystick_State
   is abstract;

private

   type Abstract_Joystick_Input is abstract new Joystick_Input with record
      Current_State, Last_State : Joystick_State;
   end record;

end Orka.Inputs.Joysticks.Default;
