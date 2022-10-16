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

package body AWT.Inputs.Gamepads.Filtering is

   function Low_Pass_Filter
     (Current, Last : Axis_Position;
      RC, DT        : Float_32) return Axis_Position
   is
      A : constant Axis_Position := Axis_Position (DT / (RC + DT));
   begin
      return A * Current + (1.0 - A) * Last;
   end Low_Pass_Filter;

   function Dead_Zone (Value, Threshold : Axis_Position) return Axis_Position is
      Result : Axis_Position;

      Scale : constant Axis_Position'Base := 1.0 / (1.0 - Threshold);
   begin
      Result := (if abs Value <= Threshold then 0.0 else Value);

      if Value >= 0.0 then
         Result := Axis_Position'Max (Result - Threshold, 0.0);
         Result := Axis_Position'Min (Result * Scale, 1.0);
      else
         Result := Axis_Position'Min (Result + Threshold, 0.0);
         Result := Axis_Position'Max (Result * Scale, -1.0);
      end if;

      return Result;
   end Dead_Zone;

   function Invert (Value : Axis_Position; Enable : Boolean) return Axis_Position is
     (if Enable then -1.0 * Value else Value);

end AWT.Inputs.Gamepads.Filtering;
