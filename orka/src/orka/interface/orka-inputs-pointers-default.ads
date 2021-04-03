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

package Orka.Inputs.Pointers.Default is
   pragma Preelaborate;

   type Abstract_Pointer_Input is abstract new Pointer_Input with private;

   overriding
   function State (Object : Abstract_Pointer_Input) return Pointer_State;

   overriding
   procedure Set_Mode (Object : in out Abstract_Pointer_Input; Mode : Pointer_Mode);

   procedure Set_Position (Object : in out Abstract_Pointer_Input; X, Y : GL.Types.Double);

   procedure Set_Scroll_Offset (Object : in out Abstract_Pointer_Input; X, Y : GL.Types.Double);

   procedure Set_Button_State
     (Object  : in out Abstract_Pointer_Input;
      Subject : Pointers.Button;
      State   : Pointers.Button_State);

   procedure Set_Cursor_Mode
     (Object : in out Abstract_Pointer_Input;
      Mode   : Pointer_Mode) is abstract;

private

   type Abstract_Pointer_Input is abstract new Pointer_Input with record
      State : Pointer_State;
   end record;

end Orka.Inputs.Pointers.Default;
