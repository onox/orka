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

with Glfw.Errors;
with Glfw.Windows.Context;
with Glfw.Windows.Hints;

with Orka.Inputs.GLFW;
with Orka.Logging;

package body Orka.Windows.GLFW is

   use all type Orka.Logging.Source;
   use all type Orka.Logging.Severity;
   use Orka.Logging;

   package Messages is new Orka.Logging.Messages (Window_System);

   procedure Print_Error (Code : Standard.Glfw.Errors.Kind; Description : String) is
   begin
      Messages.Log (Error, "GLFW " & Code'Image & ": " & Trim (Description));
   end Print_Error;

   function Initialize
     (Major, Minor : Natural;
      Debug : Boolean := False) return Orka.Contexts.Context'Class is
   begin
      --  Initialize GLFW
      Standard.Glfw.Errors.Set_Callback (Print_Error'Access);
      Standard.Glfw.Init;

      --  Initialize OpenGL context
      Standard.Glfw.Windows.Hints.Set_Minimum_OpenGL_Version (Major, Minor);
      Standard.Glfw.Windows.Hints.Set_Forward_Compat (True);
      Standard.Glfw.Windows.Hints.Set_Client_API (Standard.Glfw.Windows.Context.OpenGL);
      Standard.Glfw.Windows.Hints.Set_Profile (Standard.Glfw.Windows.Context.Core_Profile);
      Standard.Glfw.Windows.Hints.Set_Debug_Context (Debug);

      return Active_GLFW'(Orka.Contexts.Context with Debug => Debug);
   end Initialize;

   overriding
   procedure Shutdown (Object : in out Active_GLFW) is
   begin
      if Object.Debug then
         Messages.Log (Debug, "Shutting down GLFW");
      end if;
      Standard.Glfw.Shutdown;
   end Shutdown;

   overriding
   procedure Finalize (Object : in out GLFW_Window) is
   begin
      if not Object.Finalized then
         Messages.Log (Debug, "Closing GLFW window");
         Object.Destroy;
         Object.Finalized := True;
      end if;
   end Finalize;

   function Create_Window
     (Width, Height : Positive;
      Samples : Natural := 0;
      Visible, Resizable : Boolean := True) return Window'Class
   is
      package Windows renames Standard.Glfw.Windows;
   begin
      return Result : GLFW_Window := GLFW_Window'(Windows.Window
        with Input => Inputs.GLFW.Create_Pointer_Input, Finalized => False, others => <>)
      do
         declare
            Reference : constant Windows.Window_Reference
              := Windows.Window (Window'Class (Result))'Access;
         begin
            Windows.Hints.Set_Visible (Visible);
            Windows.Hints.Set_Resizable (Resizable);
            Windows.Hints.Set_Samples (Samples);

            Reference.Init (Standard.Glfw.Size (Width), Standard.Glfw.Size (Height), "");

            Inputs.GLFW.GLFW_Pointer_Input (Result.Input.all).Set_Window (Reference);

            declare
               Width, Height : Standard.Glfw.Size;
            begin
               Reference.Get_Framebuffer_Size (Width, Height);
               Result.Framebuffer_Size_Changed (Natural (Width), Natural (Height));

               Messages.Log (Debug, "Created GLFW window and GL context");
               Messages.Log (Debug, "  size:      " &
                 Trim (Width'Image) & " x " & Trim (Height'Image));
               Messages.Log (Debug, "  visible:   " & (if Visible then "yes" else "no"));
               Messages.Log (Debug, "  resizable: " & (if Resizable then "yes" else "no"));
            end;

            --  Callbacks
            Reference.Enable_Callback (Windows.Callbacks.Close);
            Reference.Enable_Callback (Windows.Callbacks.Mouse_Button);
            Reference.Enable_Callback (Windows.Callbacks.Mouse_Position);
            Reference.Enable_Callback (Windows.Callbacks.Mouse_Scroll);
            Reference.Enable_Callback (Windows.Callbacks.Key);
            Reference.Enable_Callback (Windows.Callbacks.Framebuffer_Size);

            Windows.Context.Make_Current (Reference);
         end;
      end return;
   end Create_Window;

   overriding
   function Pointer_Input
     (Object : GLFW_Window) return Inputs.Pointers.Pointer_Input_Ptr
   is (Object.Input);

   overriding
   function Width (Object : GLFW_Window) return Positive is
     (Object.Width);

   overriding
   function Height (Object : GLFW_Window) return Positive is
     (Object.Height);

   overriding
   procedure Set_Title (Object : in out GLFW_Window; Value : String) is
   begin
      Standard.Glfw.Windows.Window (Object)'Access.Set_Title (Value);
   end Set_Title;

   overriding
   procedure Close (Object : in out GLFW_Window) is
   begin
      Object.Set_Should_Close (True);
   end Close;

   overriding
   function Should_Close (Object : in out GLFW_Window) return Boolean is
   begin
      return Standard.Glfw.Windows.Window (Object)'Access.Should_Close;
   end Should_Close;

   overriding
   procedure Process_Input (Object : in out GLFW_Window) is
   begin
      Standard.Glfw.Input.Poll_Events;

      --  Update position of mouse
      Inputs.GLFW.GLFW_Pointer_Input (Object.Input.all).Set_Position
        (Object.Position_X, Object.Position_Y);

      --  Update scroll offset of mouse
      Inputs.GLFW.GLFW_Pointer_Input (Object.Input.all).Set_Scroll_Offset
        (Object.Scroll_X, Object.Scroll_Y);
   end Process_Input;

   overriding
   procedure Swap_Buffers (Object : in out GLFW_Window) is
   begin
      Standard.Glfw.Windows.Context.Swap_Buffers (Object'Access);
   end Swap_Buffers;

   overriding
   procedure Enable_Vertical_Sync (Object : in out GLFW_Window; Enable : Boolean) is
   begin
      Standard.Glfw.Windows.Context.Set_Swap_Interval (if Enable then 1 else 0);
   end Enable_Vertical_Sync;

   overriding
   procedure Close_Requested (Object : not null access GLFW_Window) is
   begin
      --  TODO Call Object.Set_Should_Close (False); if certain conditions are not met
      null;
   end Close_Requested;

   overriding
   procedure Key_Changed
     (Object   : not null access GLFW_Window;
      Key      : Standard.Glfw.Input.Keys.Key;
      Scancode : Standard.Glfw.Input.Keys.Scancode;
      Action   : Standard.Glfw.Input.Keys.Action;
      Mods     : Standard.Glfw.Input.Keys.Modifiers)
   is
      use Standard.Glfw.Input.Keys;
   begin
      if Key = Escape and Action = Press then
         Object.Set_Should_Close (True);
      end if;
      --  TODO Add Button_Input object
   end Key_Changed;

   overriding
   procedure Mouse_Position_Changed
     (Object : not null access GLFW_Window;
      X, Y   : Standard.Glfw.Input.Mouse.Coordinate) is
   begin
      Object.Position_X := GL.Types.Double (X);
      Object.Position_Y := GL.Types.Double (Y);
   end Mouse_Position_Changed;

   overriding
   procedure Mouse_Scrolled
     (Object : not null access GLFW_Window;
      X, Y   : Standard.Glfw.Input.Mouse.Scroll_Offset)
   is
      use type GL.Types.Double;
   begin
      Object.Scroll_X := Object.Scroll_X + GL.Types.Double (X);
      Object.Scroll_Y := Object.Scroll_Y + GL.Types.Double (Y);
   end Mouse_Scrolled;

   overriding
   procedure Mouse_Button_Changed
     (Object  : not null access GLFW_Window;
      Button  : Standard.Glfw.Input.Mouse.Button;
      State   : Standard.Glfw.Input.Button_State;
      Mods    : Standard.Glfw.Input.Keys.Modifiers) is
   begin
      Inputs.GLFW.GLFW_Pointer_Input (Object.Input.all).Set_Button_State (Button, State);
   end Mouse_Button_Changed;

   overriding
   procedure Framebuffer_Size_Changed
     (Object : not null access GLFW_Window;
      Width, Height : Natural) is
   begin
      Object.Width  := Width;
      Object.Height := Height;
   end Framebuffer_Size_Changed;

end Orka.Windows.GLFW;
