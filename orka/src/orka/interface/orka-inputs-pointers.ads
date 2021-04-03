--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

with GL.Types;

package Orka.Inputs.Pointers is
   pragma Preelaborate;

   type Button is (Left, Right, Middle);

   type Button_State is (Released, Pressed);

   type Pointer_Buttons is array (Button) of Button_State;

   type Pointer_Mode is (Visible, Hidden, Locked);

   type Dimension is (X, Y);

   type Coordinate is array (Dimension) of GL.Types.Double;

   type Pointer_State is record
      Buttons  : Pointer_Buttons := (others => Released);
      Mode     : Pointer_Mode    := Visible;

      Position : Coordinate := (others => 0.0);
      Relative : Coordinate := (others => 0.0);
      Scroll   : Coordinate := (others => 0.0);
   end record;

   type Pointer_Input is limited interface;

   type Pointer_Input_Ptr is not null access Pointer_Input'Class;

   function State (Object : Pointer_Input) return Pointer_State is abstract;

   procedure Set_Mode (Object : in out Pointer_Input; Mode : Pointer_Mode) is abstract;

end Orka.Inputs.Pointers;
