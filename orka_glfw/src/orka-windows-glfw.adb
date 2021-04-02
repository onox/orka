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

with GL.Context;
with GL.Viewports;

package body Orka.Windows.GLFW is

   use all type Orka.Logging.Source;
   use all type Orka.Logging.Severity;
   use Orka.Logging;

   package Messages is new Orka.Logging.Messages (Window_System);

   procedure Print_Error (Code : Standard.Glfw.Errors.Kind; Description : String) is
   begin
      Messages.Log (Error, "GLFW " & Code'Image & ": " & Trim (Description));
   end Print_Error;

   -----------------------------------------------------------------------------

   overriding
   procedure Finalize (Object : in out GLFW_Context) is
   begin
      if Object.Flags.Debug then
         Messages.Log (Debug, "Shutting down GLFW");
      end if;

      Standard.Glfw.Shutdown;
   end Finalize;

   overriding
   procedure Enable (Object : in out GLFW_Context; Subject : Contexts.Feature) is
   begin
      Contexts.Enable (Object.Features, Subject);
   end Enable;

   overriding
   function Enabled (Object : GLFW_Context; Subject : Contexts.Feature) return Boolean
     is (Contexts.Enabled (Object.Features, Subject));

   overriding
   function Is_Current (Object : GLFW_Context; Kind : Orka.Contexts.Task_Kind) return Boolean is
   begin
      raise GL.Feature_Not_Supported_Exception;
      return True;
   end Is_Current;

   overriding
   procedure Make_Current (Object : GLFW_Context) is
   begin
      raise GL.Feature_Not_Supported_Exception;
   end Make_Current;

   overriding
   procedure Make_Current
     (Object : GLFW_Context;
      Window : in out Orka.Windows.Window'Class)
   is
      Reference : constant Standard.Glfw.Windows.Window_Reference
        := Standard.Glfw.Windows.Window (Window)'Access;
   begin
      Standard.Glfw.Windows.Context.Make_Current (Reference);
   end Make_Current;

   overriding
   procedure Make_Not_Current (Object : GLFW_Context) is
   begin
      Standard.Glfw.Windows.Context.Make_Current (null);
      --  TODO Make sure Object is current on calling task
   end Make_Not_Current;

   overriding
   function Version (Object : GLFW_Context) return Contexts.Context_Version is
   begin
      return
        (Major => GL.Context.Major_Version,
         Minor => GL.Context.Minor_Version);
   end Version;

   overriding
   function Flags (Object : GLFW_Context) return Contexts.Context_Flags is
      Flags : constant GL.Context.Context_Flags := GL.Context.Flags;

      Result : Contexts.Context_Flags;
   begin
      pragma Assert (Flags.Forward_Compatible);

      Result.Debug    := Flags.Debug;
      Result.Robust   := Flags.Robust_Access;
      Result.No_Error := Flags.No_Error;

      return Result;
   end Flags;

   overriding
   function Create_Context
     (Version : Contexts.Context_Version;
      Flags   : Contexts.Context_Flags := (others => False)) return GLFW_Context
   is
      package Context_Hints renames Standard.Glfw.Windows.Context;
   begin
      --  Initialize GLFW
      Standard.Glfw.Errors.Set_Callback (Print_Error'Access);
      Standard.Glfw.Init;

      --  Initialize OpenGL context
      Standard.Glfw.Windows.Hints.Set_Minimum_OpenGL_Version (Version.Major, Version.Minor);
      Standard.Glfw.Windows.Hints.Set_Forward_Compat (True);
      Standard.Glfw.Windows.Hints.Set_Client_API (Context_Hints.OpenGL);
      Standard.Glfw.Windows.Hints.Set_Profile (Context_Hints.Core_Profile);
      Standard.Glfw.Windows.Hints.Set_Debug_Context (Flags.Debug);
      Standard.Glfw.Windows.Hints.Set_Robustness
        (if Flags.Robust then
           Context_Hints.Lose_Context_On_Reset
         else
           Context_Hints.No_Robustness);
--      Standard.Glfw.Windows.Hints.Set_No_Error_Context (Flags.No_Error);

      return (Ada.Finalization.Limited_Controlled with
        Version  => Version,
        Flags    => Flags,
        Features => <>);
   end Create_Context;

   -----------------------------------------------------------------------------

   overriding
   function Create_Window
     (Context            : Contexts.Surface_Context'Class;
      Width, Height      : Positive;
      Title              : String  := "";
      Samples            : Natural := 0;
      Visible, Resizable : Boolean := True;
      Transparent        : Boolean := False) return GLFW_Window
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
            Windows.Hints.Set_Transparent_Framebuffer (Transparent);

            Reference.Init (Standard.Glfw.Size (Width), Standard.Glfw.Size (Height), Title);

            Inputs.GLFW.GLFW_Pointer_Input (Result.Input.all).Set_Window (Reference);

            declare
               Width, Height : Standard.Glfw.Size;
            begin
               Reference.Get_Framebuffer_Size (Width, Height);
               Result.Framebuffer_Size_Changed (Natural (Width), Natural (Height));

               Messages.Log (Debug, "Created GLFW window and GL context");
               Messages.Log (Debug, "  size:        " &
                 Trim (Width'Image) & " × " & Trim (Height'Image));
               Messages.Log (Debug, "  visible:     " & (if Visible then "yes" else "no"));
               Messages.Log (Debug, "  resizable:   " & (if Resizable then "yes" else "no"));
               Messages.Log (Debug, "  transparent: " & (if Transparent then "yes" else "no"));
            end;

            --  Callbacks
            Reference.Enable_Callback (Windows.Callbacks.Close);
            Reference.Enable_Callback (Windows.Callbacks.Mouse_Button);
            Reference.Enable_Callback (Windows.Callbacks.Mouse_Position);
            Reference.Enable_Callback (Windows.Callbacks.Mouse_Scroll);
            Reference.Enable_Callback (Windows.Callbacks.Key);
            Reference.Enable_Callback (Windows.Callbacks.Framebuffer_Size);

            Standard.Glfw.Windows.Context.Make_Current (Reference);

            Messages.Log (Debug, "  context:");
            Messages.Log (Debug, "    flags:    " & Orka.Contexts.Image (Context.Flags));
            Messages.Log (Debug, "    version:  " & GL.Context.Version_String);
            Messages.Log (Debug, "    renderer: " & GL.Context.Renderer);

            GL.Viewports.Set_Clipping (GL.Viewports.Lower_Left, GL.Viewports.Zero_To_One);
            Result.Vertex_Array.Create;
         end;
      end return;
   end Create_Window;

   overriding
   procedure Finalize (Object : in out GLFW_Window) is
   begin
      if not Object.Finalized then
         Messages.Log (Debug, "Closing GLFW window");

         Object.Vertex_Array.Delete;

         Standard.Glfw.Windows.Context.Make_Current (null);
         --  FIXME Requires context to be current => ask render task to release context

         Object.Destroy;
         Object.Finalized := True;
      end if;
   end Finalize;

   overriding
   function Pointer_Input
     (Object : GLFW_Window) return Inputs.Pointers.Pointer_Input_Ptr
   is (Object.Input);

   overriding
   function Width (Object : GLFW_Window) return Positive is (Object.Width);

   overriding
   function Height (Object : GLFW_Window) return Positive is (Object.Height);

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
   function Should_Close (Object : GLFW_Window) return Boolean is
   begin
      return Standard.Glfw.Windows.Window (Object)'Access.Should_Close;
   end Should_Close;

   overriding
   procedure Process_Input (Object : in out GLFW_Window) is
      Prev_Position_X : constant GL.Types.Double := Object.Position_X;
      Prev_Position_Y : constant GL.Types.Double := Object.Position_Y;

      use type GL.Types.Double;
   begin
      Object.Scroll_X := 0.0;
      Object.Scroll_Y := 0.0;

      Standard.Glfw.Input.Poll_Events;

      --  Update position of mouse
      Inputs.GLFW.GLFW_Pointer_Input (Object.Input.all).Set_Position
        (Object.Position_X, Object.Position_Y);

      --  Keep track of Locked transitioned to True
      if Object.Input.Locked then
         Object.Got_Locked := Object.Got_Locked or not Object.Last_Locked;
      else
         Object.Got_Locked := False;
      end if;
      Object.Last_Locked := Object.Input.Locked;

      if Object.Got_Locked and then
        (Object.Position_X /= Prev_Position_X or Object.Position_Y /= Prev_Position_Y)
      then
         Object.Got_Locked := False;

         --  GLFW bug: Update position of mouse again to reset delta to 0.0
         Inputs.GLFW.GLFW_Pointer_Input (Object.Input.all).Set_Position
           (Object.Position_X, Object.Position_Y);
      end if;

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
   procedure Set_Vertical_Sync (Object : in out GLFW_Window; Enable : Boolean) is
   begin
      Standard.Glfw.Windows.Context.Set_Swap_Interval (if Enable then 1 else 0);
   end Set_Vertical_Sync;

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
      --  Accumulate the offset because the callback can be called
      --  multiple times
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
