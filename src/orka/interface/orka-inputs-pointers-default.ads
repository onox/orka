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
   function Position_X (Object : Abstract_Pointer_Input) return GL.Types.Single;

   overriding
   function Position_Y (Object : Abstract_Pointer_Input) return GL.Types.Single;

   overriding
   function Delta_X (Object : Abstract_Pointer_Input) return GL.Types.Single;

   overriding
   function Delta_Y (Object : Abstract_Pointer_Input) return GL.Types.Single;

   overriding
   function Scroll_X (Object : Abstract_Pointer_Input) return GL.Types.Single;

   overriding
   function Scroll_Y (Object : Abstract_Pointer_Input) return GL.Types.Single;

   overriding
   function Locked (Object : Abstract_Pointer_Input) return Boolean;

   overriding
   function Visible (Object : Abstract_Pointer_Input) return Boolean;

   overriding
   procedure Lock_Pointer (Object : in out Abstract_Pointer_Input; Locked : Boolean);

   overriding
   procedure Set_Visible (Object : in out Abstract_Pointer_Input; Visible : Boolean);

   overriding
   function Button_Pressed
     (Object  : Abstract_Pointer_Input;
      Subject : Pointers.Button) return Boolean;

   procedure Set_Position (Object : in out Abstract_Pointer_Input; X, Y : GL.Types.Double);

   procedure Set_Scroll_Offset (Object : in out Abstract_Pointer_Input; X, Y : GL.Types.Double);

   procedure Set_Button_State
     (Object  : in out Abstract_Pointer_Input;
      Subject : Pointers.Button;
      State   : Pointers.Button_State);

   type Cursor_Mode is (Normal, Hidden, Disabled);

   procedure Set_Cursor_Mode
     (Object : in out Abstract_Pointer_Input;
      Mode   : Cursor_Mode) is abstract;

private

   type Button_Array is array (Pointers.Button) of Boolean;

   type Abstract_Pointer_Input is abstract new Pointer_Input with record
      X, Y           : GL.Types.Double := 0.0;
      Prev_X, Prev_Y : GL.Types.Double := 0.0;
      Last_X, Last_Y : GL.Types.Double := 0.0;
      Prev_Offset_X, Prev_Offset_Y : GL.Types.Double := 0.0;
      Last_Offset_X, Last_Offset_Y : GL.Types.Double := 0.0;
      Buttons : Button_Array := (others => False);
      Locked  : Boolean := False;
      Visible : Boolean := True;
   end record;

end Orka.Inputs.Pointers.Default;
