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

package body Orka.Inputs.GLFW is

   overriding
   function Position_X (Object : GLFW_Pointer_Input) return GL.Types.Single is
     (GL.Types.Single (Object.X));

   overriding
   function Position_Y (Object : GLFW_Pointer_Input) return GL.Types.Single is
     (GL.Types.Single (Object.Y));

   overriding
   function Delta_X (Object : GLFW_Pointer_Input) return GL.Types.Single is
      use type GL.Types.Double;
   begin
      return GL.Types.Single (Object.Last_X - Object.Prev_X);
   end Delta_X;

   overriding
   function Delta_Y (Object : GLFW_Pointer_Input) return GL.Types.Single is
      use type GL.Types.Double;
   begin
      return GL.Types.Single (Object.Last_Y - Object.Prev_Y);
   end Delta_Y;

   overriding
   function Scroll_X (Object : GLFW_Pointer_Input) return GL.Types.Single is
      use type GL.Types.Double;
   begin
      return GL.Types.Single (Object.Last_Offset_X - Object.Prev_Offset_X);
   end Scroll_X;

   overriding
   function Scroll_Y (Object : GLFW_Pointer_Input) return GL.Types.Single is
      use type GL.Types.Double;
   begin
      return GL.Types.Single (Object.Last_Offset_Y - Object.Prev_Offset_Y);
   end Scroll_Y;

   overriding
   function Locked (Object : GLFW_Pointer_Input) return Boolean is
     (Object.Locked);

   overriding
   function Visible (Object : GLFW_Pointer_Input) return Boolean is
     (Object.Visible);

   overriding
   procedure Lock_Pointer (Object : in out GLFW_Pointer_Input; Locked : Boolean) is
      use Standard.Glfw.Input.Mouse;
   begin
      if Object.Locked /= Locked then
         Object.Locked := Locked;
         if Locked then
            Object.Window.Set_Cursor_Mode (Disabled);
         else
            Object.Window.Set_Cursor_Mode ((if Object.Visible then Normal else Hidden));
         end if;
      end if;
   end Lock_Pointer;

   overriding
   procedure Set_Visible (Object : in out GLFW_Pointer_Input; Visible : Boolean) is
      use Standard.Glfw.Input.Mouse;
   begin
      if Object.Visible /= Visible then
         Object.Visible := Visible;
         Object.Window.Set_Cursor_Mode ((if Visible then Normal else Hidden));
      end if;
   end Set_Visible;

   overriding
   function Button_Pressed
     (Object  : GLFW_Pointer_Input;
      Subject : Button) return Boolean is
   begin
      return Object.Buttons (Subject);
   end Button_Pressed;

   procedure Set_Position (Object : in out GLFW_Pointer_Input; X, Y : GL.Types.Double) is
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

   procedure Set_Scroll_Offset (Object : in out GLFW_Pointer_Input; X, Y : GL.Types.Double) is
   begin
      Object.Prev_Offset_X := Object.Last_Offset_X;
      Object.Prev_Offset_Y := Object.Last_Offset_Y;
      Object.Last_Offset_X := X;
      Object.Last_Offset_Y := Y;
   end Set_Scroll_Offset;

   procedure Set_Button_State
     (Object  : in out GLFW_Pointer_Input;
      Subject : Standard.Glfw.Input.Mouse.Button;
      State   : Standard.Glfw.Input.Button_State)
   is
      use Standard.Glfw.Input;
   begin
      case Subject is
         when Mouse.Left_Button =>
            Object.Buttons (Left) := State = Pressed;
         when Mouse.Right_Button =>
            Object.Buttons (Right) := State = Pressed;
         when Mouse.Middle_Button =>
            Object.Buttons (Middle) := State = Pressed;
         when others =>
            raise Program_Error with "Invalid mouse button";
      end case;
   end Set_Button_State;

   procedure Set_Window
     (Object  : in out GLFW_Pointer_Input;
      Window  : Standard.Glfw.Windows.Window_Reference) is
   begin
      Object.Window := Window;
   end Set_Window;

   function Create_Pointer_Input return Pointer_Input_Ptr is
   begin
      return new GLFW_Pointer_Input'(Pointer_Input with others => <>);
   end Create_Pointer_Input;

end Orka.Inputs.GLFW;
