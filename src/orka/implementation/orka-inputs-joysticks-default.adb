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

package body Orka.Inputs.Joysticks.Default is

   overriding
   function Current_State
     (Object : Abstract_Joystick_Input) return Joystick_State is (Object.Current_State);

   overriding
   function Last_State
     (Object : Abstract_Joystick_Input) return Joystick_State is (Object.Last_State);

   overriding
   procedure Update_State
     (Object  : in out Abstract_Joystick_Input;
      Process : access procedure (Value : in out Axis_Position;
                                  Index :        Positive)) is
   begin
      Object.Last_State    := Object.Current_State;
      Object.Current_State := Abstract_Joystick_Input'Class (Object).State;

      if Process /= null then
         for Index in 1 .. Object.Current_State.Axis_Count loop
            Process (Object.Current_State.Axes (Index), Index);
         end loop;
      end if;
   end Update_State;

end Orka.Inputs.Joysticks.Default;
