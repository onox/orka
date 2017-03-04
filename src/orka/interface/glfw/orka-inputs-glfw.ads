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

with Glfw.Input.Mouse;
with Glfw.Windows;

package Orka.Inputs.GLFW is
   pragma Preelaborate;

   type GLFW_Pointer_Input is new Pointer_Input with private;

   overriding
   function Position_X (Object : GLFW_Pointer_Input) return GL.Types.Single;

   overriding
   function Position_Y (Object : GLFW_Pointer_Input) return GL.Types.Single;

   overriding
   function Delta_X (Object : GLFW_Pointer_Input) return GL.Types.Single;

   overriding
   function Delta_Y (Object : GLFW_Pointer_Input) return GL.Types.Single;

   overriding
   function Scroll_X (Object : GLFW_Pointer_Input) return GL.Types.Single;

   overriding
   function Scroll_Y (Object : GLFW_Pointer_Input) return GL.Types.Single;

   overriding
   function Locked (Object : GLFW_Pointer_Input) return Boolean;

   overriding
   function Visible (Object : GLFW_Pointer_Input) return Boolean;

   overriding
   procedure Lock_Pointer (Object : in out GLFW_Pointer_Input; Locked : Boolean);

   overriding
   procedure Set_Visible (Object : in out GLFW_Pointer_Input; Visible : Boolean);

   overriding
   function Button_Pressed
     (Object  : GLFW_Pointer_Input;
      Subject : Button) return Boolean;

   procedure Set_Position (Object : in out GLFW_Pointer_Input; X, Y : GL.Types.Double);

   procedure Set_Scroll_Offset (Object : in out GLFW_Pointer_Input; X, Y : GL.Types.Double);

   procedure Set_Button_State
     (Object  : in out GLFW_Pointer_Input;
      Subject : Standard.Glfw.Input.Mouse.Button;
      State   : Standard.Glfw.Input.Button_State);

   procedure Set_Window
     (Object  : in out GLFW_Pointer_Input;
      Window  : Standard.Glfw.Windows.Window_Reference);

   function Create_Pointer_Input return Pointer_Input_Ptr;

private

   type Button_Array is array (Button) of Boolean;

   type GLFW_Pointer_Input is new Pointer_Input with record
      X, Y           : GL.Types.Single := 0.0;
      Prev_X, Prev_Y : GL.Types.Double := 0.0;
      Last_X, Last_Y : GL.Types.Double := 0.0;
      Prev_Offset_X, Prev_Offset_Y : GL.Types.Double := 0.0;
      Last_Offset_X, Last_Offset_Y : GL.Types.Double := 0.0;
      Buttons : Button_Array := (others => False);
      Locked  : Boolean := False;
      Visible : Boolean := True;
      Window  : access Standard.Glfw.Windows.Window;
   end record;

end Orka.Inputs.GLFW;
