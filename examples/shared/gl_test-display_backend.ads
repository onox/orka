--  Copyright (c) 2013 Felix Krause <contact@flyx.org>
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

with Glfw.Windows;

private with Glfw.Input.Keys;
private with Glfw.Input.Mouse;

package GL_Test.Display_Backend is

   --  Only exposes most basic functionality, not intended for usage
   --  outside tests.

   procedure Open_Window (Width, Height : Natural; Visible : Boolean := True; Depth_Bits : Natural := 0);

   function Get_Window return Glfw.Windows.Window_Reference;

   procedure Swap_Buffers_And_Poll_Events;

   procedure Set_Window_Title (Value : String);

   procedure Close_Window;

   procedure Shutdown;

   procedure Init (Major, Minor : Natural)
     with Pre => Major > 3 or else (Major = 3 and Minor >= 2);

   procedure Set_Not_Resizable;

   function Get_Mouse_X return Float;

   function Get_Mouse_Y return Float;

   function Get_Zoom_Distance return Float;

   procedure Set_Zoom_Distance (Distance : Float);

   function Get_Effect (Maximum : Positive) return Integer;

private

   subtype Mouse_Zoom is Float range 2.0 .. 100.0;

   subtype Mouse_Pos is Float range -180.0 .. 180.0;

   type Effects is new Integer;

   type Test_Window is new Glfw.Windows.Window with record
      Zoom_Distance : Mouse_Zoom := 2.0;
      Moving_Camera : Boolean := False;

      Mouse_X, Mouse_Y   : Float := 0.0;
      Offset_X, Offset_Y : Mouse_Pos := 0.0;
      Prev_X, Prev_Y     : Float := 0.0;

      Effect : Effects := 0;
   end record;

   type Test_Window_Access is not null access Test_Window;

   overriding
   procedure Close_Requested (Object : not null access Test_Window);

   overriding
   procedure Key_Changed (Object   : not null access Test_Window;
                          Key      : Glfw.Input.Keys.Key;
                          Scancode : Glfw.Input.Keys.Scancode;
                          Action   : Glfw.Input.Keys.Action;
                          Mods     : Glfw.Input.Keys.Modifiers);

   overriding
   procedure Mouse_Position_Changed (Object : not null access Test_Window;
                                     X, Y   : Glfw.Input.Mouse.Coordinate);

   overriding
   procedure Mouse_Scrolled (Object : not null access Test_Window;
                             X, Y   : Glfw.Input.Mouse.Scroll_Offset);

   overriding
   procedure Mouse_Button_Changed (Object  : not null access Test_Window;
                                   Button  : Glfw.Input.Mouse.Button;
                                   State   : Glfw.Input.Button_State;
                                   Mods    : Glfw.Input.Keys.Modifiers);

end GL_Test.Display_Backend;
