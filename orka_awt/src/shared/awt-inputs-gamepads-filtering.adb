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
      RC, DT        : Orka.Float_32) return Axis_Position
   is
      A : constant Axis_Position := Axis_Position (DT / (RC + DT));
   begin
      return A * Current + (Axis_Position'Last - A) * Last;
   end Low_Pass_Filter;

   function Dead_Zone (Value, Threshold : Axis_Position) return Axis_Position is
      Result : Orka.Float_32;

      V : constant Orka.Float_32 := Orka.Float_32 (Value);
      T : constant Orka.Float_32 := Orka.Float_32 (Threshold);

      Scale : constant Orka.Float_32 := 1.0 / (1.0 - T);
   begin
      Result := (if abs V <= T then 0.0 else V);

      if V >= 0.0 then
         Result := Orka.Float_32'Max (Result - T, 0.0);
         Result := Orka.Float_32'Min (Result * Scale, Orka.Float_32 (Axis_Position'Last));
      else
         Result := Orka.Float_32'Min (Result + T, 0.0);
         Result := Orka.Float_32'Max (Result * Scale, Orka.Float_32 (Axis_Position'First));
      end if;

      return Axis_Position (Result);
   end Dead_Zone;

   function Invert (Value : Axis_Position; Enable : Boolean) return Axis_Position is
     (if Enable then -1.0 * Value else Value);

end AWT.Inputs.Gamepads.Filtering;
