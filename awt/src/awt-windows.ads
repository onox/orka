--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2021 onox <denkpadje@gmail.com>
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

with AWT.Inputs;
with AWT.Monitors;

package AWT.Windows with SPARK_Mode => On is
   pragma Preelaborate;

   type Size_Mode is (Default, Minimized, Maximized, Fullscreen);

   type Monitor_Presence is (Entered, Left);

   type Window_State is record
      Visible, Resizable, Decorated, Transparent : Boolean := False;
      Mode          : Size_Mode := Default;
      Width, Height : Natural   := 0;
      Margin        : Natural   := 0;
   end record;

   type Framebuffer_State is record
      Width, Height : Natural  := 0;
      Scale         : Positive := 1;
   end record;

   type Window is limited interface;

   procedure Create_Window
     (Object                        : aliased in out Window;
      ID, Title                     : String;
      Width, Height                 : Positive;
      Visible, Resizable, Decorated : Boolean := True;
      Transparent                   : Boolean := False) is abstract
   with Pre'Class => Is_Initialized;

   procedure Set_Application_ID (Object : in out Window; ID : String) is abstract;

   procedure Set_Title (Object : in out Window; Title : String) is abstract;

   procedure Set_Margin
     (Object : in out Window;
      Margin : Natural) is abstract;

   procedure Set_Size (Object : in out Window; Width, Height : Positive) is abstract;

   procedure Set_Size_Limits
     (Object : in out Window;
      Min_Width, Min_Height, Max_Width, Max_Height : Natural) is abstract
   with Pre'Class =>
     (Max_Width = 0 or Max_Width >= Min_Width) and (Max_Height = 0 or Max_Height >= Min_Height);

   procedure Set_Size_Mode (Object : in out Window; Mode : Size_Mode) is abstract;
   --  Request the window to be fullscreen, maximized, minimized, or none
   --  of these
   --
   --  A transparent window may not actually be transparent in fullscreen mode.

   procedure Set_Size_Mode
     (Object  : in out Window;
      Mode    : AWT.Windows.Size_Mode;
      Monitor : AWT.Monitors.Monitor'Class) is abstract
   with Pre'Class => Mode = Fullscreen;

   procedure Set_Framebuffer_Scale (Object : in out Window; Scale : Positive) is abstract;

   procedure Set_Raw_Pointer_Motion (Object : in out Window; Enable : Boolean) is abstract;

   procedure Set_Visible (Object : in out Window; Visible : Boolean) is abstract;

   procedure Set_Pointer_Cursor
     (Object : in out Window;
      Cursor : AWT.Inputs.Cursors.Pointer_Cursor) is abstract;

   procedure Set_Pointer_Mode
     (Object : in out Window;
      Mode   : AWT.Inputs.Pointer_Mode) is abstract;

   function Raw_Pointer_Motion (Object : Window) return Boolean is abstract;

   function State (Object : Window) return Window_State is abstract;

   function State (Object : Window) return Framebuffer_State is abstract;

   function State (Object : in out Window) return AWT.Inputs.Pointer_State is abstract;

   function State (Object : in out Window) return AWT.Inputs.Keyboard_State is abstract;

   procedure Close (Object : in out Window) is abstract;

   function Should_Close (Object : Window) return Boolean is abstract;
   --  Return True if the window should be closed, False otherwise
   --
   --  If True, then it is no longer needed to process any events and an event
   --  loop can be exited.

   procedure Swap_Buffers (Object : in out Window) is abstract;
   --  Swap the buffers of the default framebuffer
   --
   --  Must be called by the task on which the GL context to which the default
   --  framebuffer belongs is current

   procedure Set_Vertical_Sync (Object : in out Window; Enable : Boolean) is abstract;
   --  Enable or disable vertical sync
   --
   --  Has no effect on Wayland (because the compositor will always prevent tearing).
   --
   --  Must be called by the task on which the GL context to which the default
   --  framebuffer belongs is current

   ----------------------------------------------------------------------------
   --                                 Events                                 --
   ----------------------------------------------------------------------------

   procedure On_Configure (Object : in out Window; State : Window_State) is null;
   --  Invoked when the state of the window has changed

   function On_Close (Object : Window) return Boolean is abstract;
   --  Invoked when the user wants to close the window
   --
   --  Return True if the window should actually be closed, or return False
   --  and display a dialog window on the screen that asks the user for
   --  confirmation.

   procedure On_Move
     (Object   : in out Window;
      Monitor  : AWT.Monitors.Monitor'Class;
      Presence : Monitor_Presence) is null;
   --  Invoked when the window is moved to/from a monitor

   procedure On_Drag
     (Object : in out Window;
      X, Y   : AWT.Inputs.Fixed) is null;
   --  Invoked when the user drag something over the window
   --
   --  Call procedure Set_Action in AWT.Drag_And_Drop in response to this event.

   procedure On_Drop (Object : in out Window) is null;
   --  Invoked when the user drops something on the window
   --
   --  To perform the drag-and-drop, call Get in AWT.Drag_And_Drop
   --  and then, after the given callback has been executed, accept
   --  or reject the drag-and-drop by calling Finish.
   --
   --  Call procedure Get in AWT.Drag_And_Drop to receive data and
   --  call Finish to complete the data transfer.

end AWT.Windows;
