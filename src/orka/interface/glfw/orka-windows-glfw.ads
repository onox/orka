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

with Ada.Finalization;

private with GL.Types;

private with Glfw.Windows;
private with Glfw.Input.Keys;
private with Glfw.Input.Mouse;

package Orka.Windows.GLFW is

   type Active_GLFW is limited new Ada.Finalization.Limited_Controlled with private;

   function Initialize
     (Major, Minor : Natural;
      Debug : Boolean := False) return Active_GLFW'Class
   with Pre => Major > 3 or else (Major = 3 and Minor >= 2);

   function Create_Window
     (Width, Height : Positive;
      Samples : Natural := 0;
      Visible, Resizable : Boolean := True) return Window'Class;

   type GLFW_Window is limited new Window with private;

   overriding
   function Pointer_Input
     (Object : GLFW_Window) return Inputs.Pointers.Pointer_Input_Ptr;

   overriding
   function Width (Object : GLFW_Window) return Positive;

   overriding
   function Height (Object : GLFW_Window) return Positive;

   overriding
   procedure Set_Title (Object : in out GLFW_Window; Value : String);

   overriding
   procedure Close (Object : in out GLFW_Window);

   overriding
   function Should_Close (Object : in out GLFW_Window) return Boolean;

   overriding
   procedure Process_Input (Object : in out GLFW_Window);

   overriding
   procedure Swap_Buffers (Object : in out GLFW_Window);

   overriding
   procedure Enable_Vertical_Sync (Object : in out GLFW_Window; Enable : Boolean);

private

   type Active_GLFW is limited new Ada.Finalization.Limited_Controlled with record
      Debug     : Boolean := False;
      Finalized : Boolean;
   end record;

   overriding
   procedure Finalize (Object : in out Active_GLFW);

   type GLFW_Window is limited new Standard.Glfw.Windows.Window and Window with record
      Input     : Inputs.Pointers.Pointer_Input_Ptr;
      Finalized : Boolean;
      Position_X : GL.Types.Double := 0.0;
      Position_Y : GL.Types.Double := 0.0;
      Scroll_X   : GL.Types.Double := 0.0;
      Scroll_Y   : GL.Types.Double := 0.0;
      Width, Height : Positive;
   end record;

   overriding
   procedure Finalize (Object : in out GLFW_Window);

   overriding
   procedure Close_Requested (Object : not null access GLFW_Window);

   overriding
   procedure Key_Changed
     (Object   : not null access GLFW_Window;
      Key      : Standard.Glfw.Input.Keys.Key;
      Scancode : Standard.Glfw.Input.Keys.Scancode;
      Action   : Standard.Glfw.Input.Keys.Action;
      Mods     : Standard.Glfw.Input.Keys.Modifiers);

   overriding
   procedure Mouse_Position_Changed
     (Object : not null access GLFW_Window;
      X, Y   : Standard.Glfw.Input.Mouse.Coordinate);

   overriding
   procedure Mouse_Scrolled
     (Object : not null access GLFW_Window;
      X, Y   : Standard.Glfw.Input.Mouse.Scroll_Offset);

   overriding
   procedure Mouse_Button_Changed
     (Object  : not null access GLFW_Window;
      Button  : Standard.Glfw.Input.Mouse.Button;
      State   : Standard.Glfw.Input.Button_State;
      Mods    : Standard.Glfw.Input.Keys.Modifiers);

   overriding
   procedure Framebuffer_Size_Changed
     (Object : not null access GLFW_Window;
      Width, Height : Natural);

end Orka.Windows.GLFW;
