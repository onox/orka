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

with Ada.Numerics;

package Orka.Inputs.Joysticks.Filtering is
   pragma Preelaborate;

   function RC (Cutoff_Frequency : GL.Types.Single) return GL.Types.Single is
     (1.0 / (2.0 * Ada.Numerics.Pi * Cutoff_Frequency));
   --  Return the RC for a given cutoff frequency in Hertz
   --
   --           1
   --  fc = ---------
   --       2*Pi * RC

   function Low_Pass_Filter
     (Current, Last : Axis_Position;
      RC, DT        : GL.Types.Single) return Axis_Position;

   function Dead_Zone (Value, Threshold : Axis_Position) return Axis_Position
     with Pre => Threshold >= 0.0;

   function Invert (Value : Axis_Position; Enable : Boolean) return Axis_Position;

end Orka.Inputs.Joysticks.Filtering;
