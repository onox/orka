--  SPDX-License-Identifier: Apache-2.0
--
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

with System;

with Ada.Finalization;
with Ada.Strings.Unbounded;

with Glfw.Input.Mouse;
with Glfw.Input.Keys;
with Glfw.Monitors;

package Glfw.Windows is
   pragma Preelaborate;

   use type Interfaces.C.int;

   package SU renames Ada.Strings.Unbounded;

   type Window is limited new Ada.Finalization.Limited_Controlled with private;
   type Window_Reference is not null access all Window;

   Creation_Error : exception;

   package Callbacks is
      --  Avoid pollution of Glfw.Windows package with symbols

      type Kind is (Position, Size, Close, Refresh, Focus, Maximize,
                    Content_Scale, Iconify, Framebuffer_Size,
                    Mouse_Button, Mouse_Position, Mouse_Scroll, Mouse_Enter,
                    Key, Char, File_Drop);
   end Callbacks;

   subtype Coordinate is Interfaces.C.int;

   subtype Opacity is Interfaces.C.C_float range 0.0 .. 1.0;

   type Path_List is array (Positive range <>) of SU.Unbounded_String;

   --  Task safety: Unless indicated otherwise, subprograms in this package
   --  must only be called from the environment task.

   procedure Init (Object        : not null access Window;
                   Width, Height : Size;
                   Title         : String; -- interpreted as UTF-8
                   Monitor       : Monitors.Monitor := Monitors.No_Monitor;
                   Share_Resources_With : access Window'Class := null);
   --  Create a window and its corresponding OpenGL context
   --
   --  If the window cannot be created, a Creation_Error is raised.
   --
   --  The OpenGL context of Share_Resources_With must no be current on
   --  the rendering task.

   function Initialized (Object : not null access Window) return Boolean;

   procedure Destroy (Object : not null access Window);
   --  Destroy the window and its corresponding OpenGL context
   --
   --  The context must not be current on the rendering task.

   procedure Show (Object : not null access Window);
   procedure Hide (Object : not null access Window);

   procedure Focus (Object : not null access Window);
   procedure Maximize (Object : not null access Window);

   procedure Request_Attention (Object : not null access Window);

   function Get_Monitor (Object : not null access Window) return Monitors.Monitor;

   procedure Set_Monitor
     (Object  : not null access Window;
      Monitor : Monitors.Monitor := Monitors.No_Monitor);

   procedure Set_Title (Object : not null access Window; Value : String);

   procedure Get_OpenGL_Version (Object : not null access Window;
                                 Major, Minor, Revision : out Natural);

   function Key_State (Object : not null access Window; Key : Input.Keys.Key)
                       return Input.Button_State;

   function Mouse_Button_State (Object : not null access Window;
                                Button : Input.Mouse.Button)
                                return Input.Button_State;

   function Get_Input_Toggle (Object : not null access Window;
                              Kind   : Input.Input_Toggle) return Boolean;

   procedure Set_Input_Toggle (Object : not null access Window;
                               Kind   : Input.Input_Toggle;
                               Value  : Boolean);

   function Get_Cursor_Mode (Object : not null access Window)
     return Input.Mouse.Cursor_Mode;

   procedure Set_Cursor_Mode (Object : not null access Window;
                              Mode   : Input.Mouse.Cursor_Mode);

   procedure Get_Cursor_Pos (Object : not null access Window;
                             X, Y   : out Input.Mouse.Coordinate);
   procedure Set_Cursor_Pos (Object : not null access Window;
                             X, Y   : Input.Mouse.Coordinate);

   procedure Get_Position (Object : not null access Window;
                           X, Y : out Coordinate);
   procedure Set_Position (Object : not null access Window;
                           X, Y : Coordinate);

   procedure Set_Aspect_Ratio
     (Object : not null access Window;
      Numer, Denom : Size)
   with Pre => Numer > 0 and Denom > 0;

   procedure Set_Window_Size_Limits
     (Object : not null access Window;
      Min_Width, Min_Height, Max_Width, Max_Height : Size)
   with Pre => Max_Width >= Min_Width and Max_Height >= Min_Height;

   procedure Get_Size (Object : not null access Window;
                       Width, Height : out Size);
   procedure Set_Size (Object : not null access Window;
                       Width, Height : Size);

   procedure Get_Content_Scale
     (Object : not null access Window; X, Y : out Float);

   function Get_Opacity (Object : not null access Window) return Opacity;
   procedure Set_Opacity (Object : not null access Window; Value : Opacity);

   procedure Get_Frame_Size
     (Object : not null access Window;
      Left, Top, Right, Bottom : out Size);

   procedure Get_Framebuffer_Size (Object : not null access Window;
                                   Width, Height : out Size);

   function Focused   (Object : not null access Window) return Boolean;
   function Iconified (Object : not null access Window) return Boolean;
   function Resizable (Object : not null access Window) return Boolean;
   function Visible   (Object : not null access Window) return Boolean;
   function Decorated (Object : not null access Window) return Boolean;
   function Floating  (Object : not null access Window) return Boolean;
   function Maximized (Object : not null access Window) return Boolean;
   function Hovered   (Object : not null access Window) return Boolean;
   function Auto_Iconified  (Object : not null access Window) return Boolean;
   function Focused_On_Show (Object : not null access Window) return Boolean;
   function Transparent_Framebuffer (Object : not null access Window) return Boolean;

   procedure Set_Resizable (Object : not null access Window; Enable : Boolean);
   procedure Set_Decorated (Object : not null access Window; Enable : Boolean);
   procedure Set_Floating  (Object : not null access Window; Enable : Boolean);
   procedure Set_Auto_Iconify  (Object : not null access Window; Enable : Boolean);
   procedure Set_Focus_On_Show (Object : not null access Window; Enable : Boolean);

   function Should_Close (Object : not null access constant Window) return Boolean;
   --  Return the close flag of the window
   --
   --  Task safety: May be called from any task.

   procedure Set_Should_Close
     (Object : not null access Window;
      Value  : Boolean);
   --  Set the close flag to signal that the window should (not) be closed
   --
   --  Task safety: May be called from any task.

   -----------------------------------------------------------------------------
   --                                Event API                                --
   -----------------------------------------------------------------------------

   procedure Enable_Callback (Object : not null access Window;
                              Subject : Callbacks.Kind);

   procedure Disable_Callback (Object  : not null access Window;
                               Subject : Callbacks.Kind);

   procedure Position_Changed (Object : not null access Window;
                               X, Y : Integer) is null;
   procedure Size_Changed (Object : not null access Window;
                           Width, Height : Natural) is null;
   procedure Close_Requested (Object : not null access Window) is null;
   procedure Refresh (Object : not null access Window) is null;
   procedure Focus_Changed (Object : not null access Window;
                            Focused : Boolean) is null;
   procedure Maximize_Changed
     (Object    : not null access Window;
      Maximized : Boolean) is null;
   procedure Content_Scale_Changed
     (Object : not null access Window;
      X, Y   : Float) is null;
   procedure Iconification_Changed (Object : not null access Window;
                                    Iconified : Boolean) is null;
   procedure Framebuffer_Size_Changed (Object : not null access Window;
                                       Width, Height : Natural) is null;
   procedure Mouse_Button_Changed (Object  : not null access Window;
                                   Button  : Input.Mouse.Button;
                                   State   : Input.Button_State;
                                   Mods    : Input.Keys.Modifiers) is null;
   procedure Mouse_Position_Changed (Object : not null access Window;
                                     X, Y   : Input.Mouse.Coordinate) is null;
   procedure Mouse_Scrolled (Object : not null access Window;
                             X, Y   : Input.Mouse.Scroll_Offset) is null;
   procedure Mouse_Entered (Object : not null access Window;
                            Action : Input.Mouse.Enter_Action) is null;
   procedure Key_Changed (Object   : not null access Window;
                          Key      : Input.Keys.Key;
                          Scancode : Input.Keys.Scancode;
                          Action   : Input.Keys.Action;
                          Mods     : Input.Keys.Modifiers) is null;
   procedure Character_Entered (Object : not null access Window;
                                Char   : Wide_Wide_Character) is null;

   procedure Files_Dropped
     (Object : not null access Window;
      Paths  : Path_List) is null;

private

   type Windowed_Size is record
      X, Y          : Coordinate;
      Width, Height : Size;
   end record;

   type Window is limited new Ada.Finalization.Limited_Controlled with record
      Handle : System.Address := System.Null_Address;

      --  Used to restore position and size when moving from fullscreen
      --  to windowed
      Original_Size : Windowed_Size;
   end record;

   function Window_Ptr (Raw : System.Address) return not null access Window'Class;

end Glfw.Windows;
