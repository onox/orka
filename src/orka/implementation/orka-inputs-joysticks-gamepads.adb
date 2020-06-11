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

package body Orka.Inputs.Joysticks.Gamepads is

   function Value (Index : Button_Index) return Button is
     (Button'Val (Index - Button_Index'First));

   function Value (Index : Axis_Index) return Axis is
     (Axis'Val (Index - Axis_Index'First));

   function Index (Value : Button) return Button_Index is
     (Button_Index'First + Button'Pos (Value));

   function Index (Value : Axis) return Axis_Index is
     (Axis_Index'First + Axis'Pos (Value));

   procedure Normalize_Axes (Axes : in out Axis_Positions) is
   begin
      --  Let vertical axis go from down to up
      Axes (2) := -1.0 * Axes (Index (Left_Stick_Y));
      Axes (4) := -1.0 * Axes (Index (Right_Stick_Y));

      --  Map -1 .. 1 to 0 .. 1 for triggers
      Axes (5) := (Axes (Index (Left_Trigger)) + 1.0) / 2.0;
      Axes (6) := (Axes (Index (Right_Trigger)) + 1.0) / 2.0;
   end Normalize_Axes;

end Orka.Inputs.Joysticks.Gamepads;
