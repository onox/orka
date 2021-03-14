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

package Orka.Inputs.Joysticks.Gamepads is
   pragma Preelaborate;

   type Button is
     (Right_Pad_Down,
      Right_Pad_Right,
      Right_Pad_Left,
      Right_Pad_Up,
      Left_Shoulder,
      Right_Shoulder,
      Center_Left,
      Center_Right,
      Center_Logo,
      Left_Stick,
      Right_Stick,
      Left_Pad_Up,
      Left_Pad_Right,
      Left_Pad_Down,
      Left_Pad_Left);

   type Axis is
     (Left_Stick_X,
      Left_Stick_Y,
      Right_Stick_X,
      Right_Stick_Y,
      Left_Trigger,
      Right_Trigger);

   function Value (Index : Button_Index) return Button;
   function Value (Index : Axis_Index) return Axis;

   function Index (Value : Button) return Button_Index;
   function Index (Value : Axis) return Axis_Index;

   procedure Normalize_Axes (Axes : in out Axis_Positions);

end Orka.Inputs.Joysticks.Gamepads;
