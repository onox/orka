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
   function Position_X (Object : Abstract_Pointer_Input) return GL.Types.Single is
     (GL.Types.Single (Object.X));

   overriding
   function Position_Y (Object : Abstract_Pointer_Input) return GL.Types.Single is
     (GL.Types.Single (Object.Y));

   overriding
   function Delta_X (Object : Abstract_Pointer_Input) return GL.Types.Single is
      use type GL.Types.Double;
   begin
      return GL.Types.Single (Object.Last_X - Object.Prev_X);
   end Delta_X;

   overriding
   function Delta_Y (Object : Abstract_Pointer_Input) return GL.Types.Single is
      use type GL.Types.Double;
   begin
      return GL.Types.Single (Object.Last_Y - Object.Prev_Y);
   end Delta_Y;

   overriding
   function Scroll_X (Object : Abstract_Pointer_Input) return GL.Types.Single is
      use type GL.Types.Double;
   begin
      return GL.Types.Single (Object.Last_Offset_X - Object.Prev_Offset_X);
   end Scroll_X;

   overriding
   function Scroll_Y (Object : Abstract_Pointer_Input) return GL.Types.Single is
      use type GL.Types.Double;
   begin
      return GL.Types.Single (Object.Last_Offset_Y - Object.Prev_Offset_Y);
   end Scroll_Y;

   overriding
   function Locked (Object : Abstract_Pointer_Input) return Boolean is
     (Object.Locked);

   overriding
   function Visible (Object : Abstract_Pointer_Input) return Boolean is
     (Object.Visible);

   overriding
   procedure Lock_Pointer (Object : in out Abstract_Pointer_Input; Locked : Boolean) is
   begin
      if Object.Locked /= Locked then
         Object.Locked := Locked;
         if Locked then
            Abstract_Pointer_Input'Class (Object).Set_Cursor_Mode (Disabled);
         else
            Abstract_Pointer_Input'Class (Object).Set_Cursor_Mode
              ((if Object.Visible then Normal else Hidden));
         end if;
      end if;
   end Lock_Pointer;

   overriding
   procedure Set_Visible (Object : in out Abstract_Pointer_Input; Visible : Boolean) is
   begin
      if Object.Visible /= Visible then
         Object.Visible := Visible;
         Abstract_Pointer_Input'Class (Object).Set_Cursor_Mode
           ((if Visible then Normal else Hidden));
      end if;
   end Set_Visible;

   overriding
   function Button_Pressed
     (Object  : Abstract_Pointer_Input;
      Subject : Button) return Boolean is
   begin
      return Object.Buttons (Subject);
   end Button_Pressed;

   procedure Set_Position (Object : in out Abstract_Pointer_Input; X, Y : GL.Types.Double) is
   begin
      if not Object.Locked then
         Object.X := X;
         Object.Y := Y;
      end if;
      Object.Prev_X := Object.Last_X;
      Object.Prev_Y := Object.Last_Y;
      Object.Last_X := X;
      Object.Last_Y := Y;
   end Set_Position;

   procedure Set_Scroll_Offset (Object : in out Abstract_Pointer_Input; X, Y : GL.Types.Double) is
   begin
      Object.Prev_Offset_X := Object.Last_Offset_X;
      Object.Prev_Offset_Y := Object.Last_Offset_Y;
      Object.Last_Offset_X := X;
      Object.Last_Offset_Y := Y;
   end Set_Scroll_Offset;

   procedure Set_Button_State
     (Object  : in out Abstract_Pointer_Input;
      Subject : Button;
      State   : Button_State) is
   begin
      Object.Buttons (Subject) := State = Pressed;
   end Set_Button_State;

end Orka.Inputs.Pointers.Default;
