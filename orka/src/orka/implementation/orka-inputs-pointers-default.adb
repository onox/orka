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

package body Orka.Inputs.Pointers.Default is

   overriding
   function State (Object : Abstract_Pointer_Input) return Pointer_State is
   begin
      return Object.State;
   end State;

   overriding
   procedure Set_Mode (Object : in out Abstract_Pointer_Input; Mode : Pointer_Mode) is
   begin
      if Object.State.Mode /= Mode then
         Object.State.Mode := Mode;
         Abstract_Pointer_Input'Class (Object).Set_Cursor_Mode (Mode);
      end if;
   end Set_Mode;

   procedure Set_Position (Object : in out Abstract_Pointer_Input; X, Y : GL.Types.Double) is
   begin
      Object.State.Relative :=
        (X - Object.State.Position (Dimension'First),
         Y - Object.State.Position (Dimension'Last));

      Object.State.Position := (X, Y);
   end Set_Position;

   procedure Set_Scroll_Offset (Object : in out Abstract_Pointer_Input; X, Y : GL.Types.Double) is
   begin
      Object.State.Scroll := (X, Y);
   end Set_Scroll_Offset;

   procedure Set_Button_State
     (Object  : in out Abstract_Pointer_Input;
      Subject : Button;
      State   : Button_State) is
   begin
      Object.State.Buttons (Subject) := State;
   end Set_Button_State;

end Orka.Inputs.Pointers.Default;
