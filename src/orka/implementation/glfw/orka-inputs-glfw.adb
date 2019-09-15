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

package body Orka.Inputs.GLFW is

   overriding
   procedure Set_Cursor_Mode
     (Object  : in out GLFW_Pointer_Input;
      Mode    : Pointers.Default.Cursor_Mode)
   is
      package Mouse renames Standard.Glfw.Input.Mouse;

      use all type Pointers.Default.Cursor_Mode;
   begin
      case Mode is
         when Normal =>
            Object.Window.Set_Cursor_Mode (Mouse.Normal);
         when Hidden =>
            Object.Window.Set_Cursor_Mode (Mouse.Hidden);
         when Disabled =>
            Object.Window.Set_Cursor_Mode (Mouse.Disabled);
      end case;
   end Set_Cursor_Mode;

   procedure Set_Button_State
     (Object  : in out GLFW_Pointer_Input;
      Subject : Standard.Glfw.Input.Mouse.Button;
      State   : Standard.Glfw.Input.Button_State)
   is
      use Standard.Glfw.Input;
      use all type Inputs.Pointers.Button;
      use all type Inputs.Pointers.Button_State;

      Pointer_State : constant Pointers.Button_State
        := (case State is
              when Pressed  => Pressed,
              when Released => Released);
   begin
      case Subject is
         when Mouse.Left_Button =>
            Object.Set_Button_State (Left, Pointer_State);
         when Mouse.Right_Button =>
            Object.Set_Button_State (Right, Pointer_State);
         when Mouse.Middle_Button =>
            Object.Set_Button_State (Middle, Pointer_State);
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

   function Create_Pointer_Input return Inputs.Pointers.Pointer_Input_Ptr is
   begin
      return new GLFW_Pointer_Input'
        (Pointers.Default.Abstract_Pointer_Input with others => <>);
   end Create_Pointer_Input;

end Orka.Inputs.GLFW;
