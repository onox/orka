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

with Wayland.Enums.Client;
with Wayland.Enums.Presentation_Time;
with Wayland.Enums.Pointer_Constraints_Unstable_V1;
with Wayland.Enums.Xdg_Decoration_Unstable_V1;
with Wayland.Enums.Xdg_Shell;

with Wayland.EGL.AWT;

with AWT.Registry;

with Orka.OS;

package body AWT.Wayland.Windows is

   function "+" (Value : SU.Unbounded_String) return String renames SU.To_String;
   function "+" (Value : String) return SU.Unbounded_String renames SU.To_Unbounded_String;

   Global : AWT.Registry.Compositor renames AWT.Registry.Global;

   package Output_Data  renames AWT.Registry.Output_Data;

   ----------------------------------------------------------------------------

   procedure Locked_Pointer_Locked
     (Pointer : in out WP.Pointer_Constraints_Unstable_V1.Locked_Pointer_V1'Class)
   is
      Object : constant not null access Wayland_Window :=
        Locked_Pointer_With_Window (Pointer).Window;

      use all type AWT.Inputs.Dimension;
      use all type AWT.Inputs.Pointer_Mode;
   begin
      Pointer.Set_Cursor_Position_Hint
        (Standard.Wayland.Fixed (Object.Locked_Position (X)),
         Standard.Wayland.Fixed (Object.Locked_Position (Y)));
      Object.Set_Pointer_Mode (Locked);
   end Locked_Pointer_Locked;

   procedure Locked_Pointer_Unlocked
     (Pointer : in out WP.Pointer_Constraints_Unstable_V1.Locked_Pointer_V1'Class)
   is
      Object : constant not null access Wayland_Window :=
        Locked_Pointer_With_Window (Pointer).Window;
   begin
      Object.Set_Pointer_Mode (Object.Unlocked_Pointer_Mode);
   end Locked_Pointer_Unlocked;

   ----------------------------------------------------------------------------

   procedure Surface_Enter
     (Surface : in out WP.Client.Surface'Class;
      Output  : WP.Client.Output)
   is
      Window : constant not null access Wayland_Window :=
        Surface_With_Window (Surface).Window;

      Monitor : constant not null access AWT.Registry.Monitor_Device :=
        Output_Data.Get_Data (Output).Monitor;
   begin
      Wayland_Window'Class (Window.all).On_Move (Monitor.all, AWT.Windows.Entered);
   end Surface_Enter;

   procedure Surface_Leave
     (Surface : in out WP.Client.Surface'Class;
      Output  : WP.Client.Output)
   is
      Window : constant not null access Wayland_Window :=
        Surface_With_Window (Surface).Window;

      Monitor : constant not null access AWT.Registry.Monitor_Device :=
        Output_Data.Get_Data (Output).Monitor;
   begin
      Wayland_Window'Class (Window.all).On_Move (Monitor.all, AWT.Windows.Left);
   end Surface_Leave;

   ----------------------------------------------------------------------------

   package Locked_Pointer_Events is new PC.Locked_Pointer_V1_Events
     (Locked   => Locked_Pointer_Locked,
      Unlocked => Locked_Pointer_Unlocked);

   package Surface_Events is new WP.Client.Surface_Events
     (Enter => Surface_Enter,
      Leave => Surface_Leave);

   ----------------------------------------------------------------------------

   procedure Set_Opaque
     (Object        : in out Wayland_Window;
      X, Y          : Natural;
      Width, Height : Natural)
   is
      Region : WP.Client.Region;
   begin
      Global.Compositor.Create_Region (Region);

      if not Region.Has_Proxy then
         raise Internal_Error with "Wayland: Failed to get region";
      end if;

      Region.Add (0, 0, Width, Height);
      Object.Surface.Set_Opaque_Region (Region);
      Region.Destroy;
   end Set_Opaque;

   procedure Set_Pointer_Input
     (Object        : in out Wayland_Window;
      X, Y          : Natural;
      Width, Height : Natural)
   is
      Region : WP.Client.Region;
   begin
      Global.Compositor.Create_Region (Region);

      if not Region.Has_Proxy then
         raise Internal_Error with "Wayland: Failed to get region";
      end if;

      Region.Add (X, Y, Width, Height);
      Object.Surface.Set_Input_Region (Region);
      Region.Destroy;
   end Set_Pointer_Input;

   procedure Set_Window_Margin
     (Object        : in out Wayland_Window;
      Margin        : Natural;
      Width, Height : Positive)
   is
      X : Natural renames Margin;
      Y : Natural renames Margin;
   begin
      Object.XDG_Surface.Set_Window_Geometry (X, Y, Width, Height);
      Object.Set_Pointer_Input (X, Y, Width, Height);

      if not Object.Pending_State.Transparent then
         Object.Set_Opaque (X, Y, Width, Height);
      end if;

      Object.Pending_State.Width  := Width;
      Object.Pending_State.Height := Height;

      Object.Pending_State.Margin := Margin;
   end Set_Window_Margin;

   procedure Set_Idle_Inhibit (Object : in out Wayland_Window; Enable : Boolean) is
   begin
      if not Global.Inhibit_Manager.Has_Proxy then
         return;
      end if;

      if Enable and not Object.Idle_Inhibitor.Has_Proxy then
         if Global.Inhibit_Manager.Has_Proxy then
            Global.Inhibit_Manager.Create_Inhibitor (Object.Surface, Object.Idle_Inhibitor);
         else
            --  Cannot inhibit screensaver due to missing inhibit manager
            null;
         end if;
      elsif not Enable and Object.Idle_Inhibitor.Has_Proxy then
         Object.Idle_Inhibitor.Destroy;
      end if;
   end Set_Idle_Inhibit;

   procedure Do_Resize
     (Object        : in out Wayland_Window;
      Width, Height : Positive;
      Margin        : Natural)
   is
      Scale : Positive renames Object.Pending_Scale;
   begin
      Object.EGL_Window.Resize
        ((Width  => Scale * (Width + 2 * Margin),
          Height => Scale * (Height + 2 * Margin)));
      Object.Set_Window_Margin (Margin, Width, Height);

      Object.Current_State.Width  := Width;
      Object.Current_State.Height := Height;

      Object.Current_Scale := Scale;
   end Do_Resize;

   procedure XDG_Surface_Configure
     (Xdg_Surface : in out WP.Xdg_Shell.Xdg_Surface'Class;
      Serial      : Unsigned_32)
   is
      Object : constant not null access Wayland_Window :=
        Xdg_Surface_With_Window (Xdg_Surface).Window;

      State : AWT.Windows.Window_State renames Object.Pending_State;

      Size_Changed : constant Boolean :=
        Object.Current_State.Width /= State.Width or
          Object.Current_State.Height /= State.Height;
   begin
      Object.Current_State := State;

      if Object.Initial_Configure then
         Object.Initial_Configure := False;

         Object.Do_Resize (State.Width, State.Height, State.Margin);

         Xdg_Surface.Ack_Configure (Serial);

         Wayland_Window'Class (Object.all).On_Configure (State);

         Object.Frame_Handler.Set_Has_Buffer (True);
      else
         Object.Frame_Handler.Set_Size (State.Width, State.Height, State.Margin, Serial);
      end if;

   end XDG_Surface_Configure;

   procedure XDG_Toplevel_Configure
     (Xdg_Toplevel : in out WP.Xdg_Shell.Xdg_Toplevel'Class;
      Width        : Natural;
      Height       : Natural;
      States       : WP.Xdg_Shell.State_Array)
   is
      use all type WE.Xdg_Shell.Xdg_Toplevel_State;
      use all type AWT.Windows.Size_Mode;
      use all type AWT.Windows.Window_State;

      Object : constant not null access Wayland_Window :=
        Xdg_Toplevel_With_Window (Xdg_Toplevel).Window;

      Is_Maximized, Is_Fullscreen : Boolean := False;
   begin
      if States'Length > 0 then
         for State of States loop
            case State is
               when Maximized =>
                  Is_Maximized := True;
               when Fullscreen =>
                  Is_Fullscreen := True;
               when Resizing =>
                  null;
               when Activated =>
                  null;
               when Tiled_Left | Tiled_Right | Tiled_Top | Tiled_Bottom =>
                  null;
            end case;
         end loop;
      end if;

      Object.Pending_State.Width  := (if Width > 0 then Width else Object.Restore_Width);
      Object.Pending_State.Height := (if Height > 0 then Height else Object.Restore_Height);

      if Is_Fullscreen then
         Object.Pending_State.Mode := Fullscreen;
      elsif Is_Maximized then
         Object.Pending_State.Mode := Maximized;
      else
         Object.Pending_State.Mode := Default;
      end if;

      declare
         State : AWT.Windows.Window_State renames Object.Pending_State;

         State_Changed : constant Boolean :=
            Object.Current_State /= State;

         Size_Changed : constant Boolean :=
           Object.Current_State.Width /= State.Width or
             Object.Current_State.Height /= State.Height;

         Mode_Changed : constant Boolean :=
           Object.Current_State.Mode /= State.Mode;
      begin
         if Size_Changed then
            Object.Set_Window_Margin (State.Margin, State.Width, State.Height);
         end if;

         if Mode_Changed then
            Object.Set_Idle_Inhibit (State.Mode = Fullscreen);

            --  Temporarily remove any margin set by the user if the
            --  window is maximized or fullscreen
            if State.Mode in Maximized | Fullscreen then
               Object.Set_Window_Margin (0, State.Width, State.Height);
            elsif Object.Current_State.Mode in Maximized | Fullscreen then
               Object.Set_Window_Margin (Object.Restore_Margin, State.Width, State.Height);
            end if;
         end if;
      end;
   end XDG_Toplevel_Configure;

   procedure XDG_Toplevel_Close
     (Xdg_Toplevel : in out WP.Xdg_Shell.Xdg_Toplevel'Class)
   is
      Object : constant not null access Wayland_Window :=
        Xdg_Toplevel_With_Window (Xdg_Toplevel).Window;
   begin
      if Wayland_Window'Class (Object.all).On_Close then
         Object.Close;
      end if;
   end XDG_Toplevel_Close;

   procedure XDG_Toplevel_Decoration_Configure
     (Decoration : in out WP.Xdg_Decoration_Unstable_V1.Toplevel_Decoration_V1'Class;
      Mode       : WE.Xdg_Decoration_Unstable_V1.Toplevel_Decoration_V1_Mode)
   is
      use all type WE.Xdg_Decoration_Unstable_V1.Toplevel_Decoration_V1_Mode;

      Object : constant not null access Wayland_Window :=
        Toplevel_Decoration_With_Window (Decoration).Window;
   begin
      Object.Pending_State.Decorated := Mode = Server_Side;
   end XDG_Toplevel_Decoration_Configure;

   -----------------------------------------------------------------------------

   procedure Feedback_Synchronized_Output
     (Feedback : in out WP.Presentation_Time.Presentation_Feedback'Class;
      Output   : WP.Client.Output'Class)
   is
      Window : constant not null access Wayland_Window :=
        Feedback_With_Frame (Feedback).Data.Window;

      Index : constant Frame_Index := Feedback_With_Frame (Feedback).Data.Index;

      Monitor : constant not null access AWT.Registry.Monitor_Device :=
        Output_Data.Get_Data (Output).Monitor;
   begin
      Window.Frame_Handler.On_Frame_Output (Index, Monitor.Current_State.Refresh);
   end Feedback_Synchronized_Output;

   procedure Feedback_Presented
     (Feedback : in out WP.Presentation_Time.Presentation_Feedback'Class;
      Timestamp : Duration;
      Refresh   : Duration;
      Counter   : Standard.Wayland.Unsigned_64;
      Flags     : WE.Presentation_Time.Presentation_Feedback_Kind)
   is
      Window : constant not null access Wayland_Window :=
        Feedback_With_Frame (Feedback).Data.Window;

      Index : constant Frame_Index := Feedback_With_Frame (Feedback).Data.Index;
   begin
      Feedback.Destroy;
      Window.Frame_Handler.On_Frame_Presented (Index, Timestamp, Refresh);
   end Feedback_Presented;

   procedure Feedback_Discarded
     (Feedback : in out WP.Presentation_Time.Presentation_Feedback'Class)
   is
      Window : constant not null access Wayland_Window :=
        Feedback_With_Frame (Feedback).Data.Window;

      Index : constant Frame_Index := Feedback_With_Frame (Feedback).Data.Index;
   begin
      Feedback.Destroy;
      Window.Frame_Handler.On_Frame_Discarded (Index);
   end Feedback_Discarded;

   -----------------------------------------------------------------------------

   package XDG_Surface_Events is new WP.Xdg_Shell.Xdg_Surface_Events
     (Configure => XDG_Surface_Configure);

   package XDG_Toplevel_Events is new WP.Xdg_Shell.Xdg_Toplevel_Events
     (Configure => XDG_Toplevel_Configure,
      Close     => XDG_Toplevel_Close);

   package XDG_Toplevel_Decoration_Events is
     new WP.Xdg_Decoration_Unstable_V1.Toplevel_Decoration_V1_Events
       (Configure => XDG_Toplevel_Decoration_Configure);

   package Feedback_Events is new WP.Presentation_Time.Presentation_Feedback_Events
     (Synchronized_Output => Feedback_Synchronized_Output,
      Presented           => Feedback_Presented,
      Discarded           => Feedback_Discarded);

   -----------------------------------------------------------------------------

   overriding procedure Finalize (Object : in out Wayland_Window) is
   begin
      Object.Frame_Handler.Finalize;

      if Object.EGL_Window.Is_Initialized then
         Object.EGL_Window.Destroy;
      end if;

      if Object.Idle_Inhibitor.Has_Proxy then
         Object.Idle_Inhibitor.Destroy;
      end if;

      if Object.Locked_Pointer.Has_Proxy then
         Object.Locked_Pointer.Destroy;
      end if;

      if Object.Decoration.Has_Proxy then
         Object.Decoration.Destroy;
      end if;

      if Object.XDG_Toplevel.Has_Proxy then
         Object.XDG_Toplevel.Destroy;
      end if;

      if Object.XDG_Surface.Has_Proxy then
         Object.XDG_Surface.Destroy;
      end if;

      if Object.Surface.Has_Proxy then
         Object.Surface.Destroy;
      end if;

      if Object.Cursor_Surface.Has_Proxy then
         Object.Cursor_Surface.Destroy;
      end if;
   end Finalize;

   procedure Create_XDG_Window
     (Object               : in out Wayland_Window;
      ID, Title            : String;
      Width, Height        : Positive;
      Resizable, Decorated : Boolean)
   is
      use all type WE.Xdg_Decoration_Unstable_V1.Toplevel_Decoration_V1_Mode;
   begin
      Global.XDG_Shell.Get_Surface (Object.Surface, Object.XDG_Surface);
      if not Object.XDG_Surface.Has_Proxy then
         raise Internal_Error with "Wayland: Failed to get XDG surface";
      end if;

      Object.XDG_Surface.Get_Toplevel (Object.XDG_Toplevel);
      if not Object.XDG_Toplevel.Has_Proxy then
         raise Internal_Error with "Wayland: Failed to get XDG toplevel";
      end if;

      XDG_Surface_Events.Subscribe (Object.XDG_Surface);
      XDG_Toplevel_Events.Subscribe (Object.XDG_Toplevel);

      if Decorated and Global.XDG_Decoration_Manager.Has_Proxy then
         Global.XDG_Decoration_Manager.Get_Toplevel_Decoration
           (Object.XDG_Toplevel, Object.Decoration);

         if not Object.Decoration.Has_Proxy then
            raise Internal_Error with "Wayland: Failed to get XDG decoration";
         end if;

         XDG_Toplevel_Decoration_Events.Subscribe (Object.Decoration);

         Object.Decoration.Set_Mode (Server_Side);
      end if;

      Object.Pending_State.Width  := Width;
      Object.Pending_State.Height := Height;

      Object.Restore_Width  := Width;
      Object.Restore_Height := Height;

      if not Resizable then
         Object.Set_Size_Limits (Width, Height, Width, Height);
      end if;

      Object.Set_Pointer_Cursor (Object.Cursor);

      Object.Set_Application_ID (ID);
      Object.Set_Title (Title);

      -------------------------------------------------------------------------

      Object.Initial_Configure := True;

      Object.Surface.Commit;
      Global.Display.Roundtrip;

      declare
         Interval : constant Duration := 0.001;
         Timeout  : constant Duration := 2.0;

         Clock_Timeout : constant Duration := Orka.OS.Monotonic_Clock + Timeout;
      begin
         while Object.Initial_Configure and then AWT.Process_Events (Interval) loop
            if Orka.OS.Monotonic_Clock > Clock_Timeout then
               raise Internal_Error with "Wayland: Failed to receive configure event";
            end if;
         end loop;
      end;
   end Create_XDG_Window;

   overriding
   procedure Create_Window
     (Object                        : aliased in out Wayland_Window;
      ID, Title                     : String;
      Width, Height                 : Positive;
      Visible, Resizable, Decorated : Boolean := True;
      Transparent                   : Boolean := False)
   is
   begin
      if not Global.XDG_Shell.Has_Proxy then
         raise Internal_Error with "Wayland: No XDG shell available";
      end if;

      Global.Compositor.Create_Surface (Object.Cursor_Surface);
      if not Object.Cursor_Surface.Has_Proxy then
         raise Internal_Error with "Wayland: Failed to get cursor surface";
      end if;

      Global.Compositor.Create_Surface (Object.Surface);
      if not Object.Surface.Has_Proxy then
         raise Internal_Error with "Wayland: Failed to get surface";
      end if;

      Surface_Events.Subscribe (Object.Surface);

      -------------------------------------------------------------------------

      Object.EGL_Window.Create_Window (WP.Client.Surface (Object.Surface), Width, Height);

      Object.EGL_Surface := Standard.Wayland.EGL.AWT.Create_Surface
        (Object.EGL_Window, Object.EGL_Context.Display, Object.EGL_Config, Object.EGL_sRGB);

      pragma Assert (Object.EGL_Surface.Width = Width);
      pragma Assert (Object.EGL_Surface.Height = Height);

      -------------------------------------------------------------------------

      Object.Pending_State.Visible     := Visible;
      Object.Pending_State.Resizable   := Resizable;
      Object.Pending_State.Transparent := Transparent;

      Object.Create_XDG_Window (ID, Title, Width, Height, Resizable, Decorated);
   end Create_Window;

   overriding
   procedure Set_Size (Object : in out Wayland_Window; Width, Height : Positive) is
      State : AWT.Windows.Window_State renames Object.Current_State;
   begin
      Object.Frame_Handler.Set_Size (Width, Height, State.Margin, 0);
   end Set_Size;

   overriding
   procedure Set_Size_Limits
     (Object : in out Wayland_Window;
      Min_Width, Min_Height, Max_Width, Max_Height : Natural) is
   begin
      if Object.Pending_State.Resizable then
         Object.XDG_Toplevel.Set_Min_Size (Min_Width, Min_Height);
         Object.XDG_Toplevel.Set_Max_Size (Max_Width, Max_Height);
      end if;
   end Set_Size_Limits;

   overriding
   procedure Set_Margin
     (Object : in out Wayland_Window;
      Margin : Natural)
   is
      Width  : Positive renames Object.Pending_State.Width;
      Height : Positive renames Object.Pending_State.Height;
   begin
      Object.Restore_Margin := Margin;
      Object.Frame_Handler.Set_Size (Width, Height, Margin, 0);
   end Set_Margin;

   overriding
   procedure Set_Framebuffer_Scale (Object : in out Wayland_Window; Scale : Positive) is
   begin
      Object.Pending_Scale := Scale;
      Object.Surface.Set_Buffer_Scale (Scale);
   end Set_Framebuffer_Scale;

   overriding
   procedure Set_Size_Mode (Object : in out Wayland_Window; Mode : AWT.Windows.Size_Mode) is
      use all type AWT.Windows.Size_Mode;
   begin
      if Object.Pending_State.Mode = Default and Mode /= Default then
         Object.Restore_Width  := Object.Pending_State.Width;
         Object.Restore_Height := Object.Pending_State.Height;
      end if;

      case Mode is
         when Default =>
            Object.XDG_Toplevel.Set_Maximized (False);
            Object.XDG_Toplevel.Set_Fullscreen (False);
         when Minimized =>
            Object.XDG_Toplevel.Minimize;
         when Maximized =>
            Object.XDG_Toplevel.Set_Maximized (True);
            Object.XDG_Toplevel.Set_Fullscreen (False);
         when Fullscreen =>
            Object.XDG_Toplevel.Set_Fullscreen (True);
      end case;
   end Set_Size_Mode;

   overriding
   procedure Set_Size_Mode
     (Object  : in out Wayland_Window;
      Mode    : AWT.Windows.Size_Mode;
      Monitor : AWT.Monitors.Monitor'Class) is
   begin
      Object.XDG_Toplevel.Set_Fullscreen (True, AWT.Registry.Monitor_Device (Monitor).Output);
   end Set_Size_Mode;

   overriding
   procedure Set_Visible (Object : in out Wayland_Window; Visible : Boolean) is
      State : AWT.Windows.Window_State renames Object.Current_State;
   begin
      if State.Visible = Visible then
         return;
      end if;

      Object.Pending_State.Visible := Visible;

      if Visible then
         if Object.XDG_Toplevel.Has_Proxy then
            Object.XDG_Toplevel.Destroy;
         end if;

         if Object.XDG_Surface.Has_Proxy then
            Object.XDG_Surface.Destroy;
         end if;

         Object.Create_XDG_Window (+Object.Restore_ID, +Object.Restore_Title,
           Object.Restore_Width, Object.Restore_Height, State.Resizable, State.Decorated);
      else
         Object.Restore_Width  := Object.Pending_State.Width;
         Object.Restore_Height := Object.Pending_State.Height;

         Object.Frame_Handler.Set_Has_Buffer (False);
         State.Visible := Object.Pending_State.Visible;
      end if;
   end Set_Visible;

   overriding
   function State (Object : Wayland_Window) return AWT.Windows.Window_State is
   begin
      return Object.Current_State;
   end State;

   overriding
   function State (Object : Wayland_Window) return AWT.Windows.Framebuffer_State is
      State : AWT.Windows.Window_State renames Object.Current_State;
   begin
      return (Width  => State.Width + 2 * State.Margin,
              Height => State.Height + 2 * State.Margin,
              Scale  => Object.Current_Scale);
   end State;

   overriding
   procedure Set_Application_ID (Object : in out Wayland_Window; ID : String) is
   begin
      Object.Restore_ID := +ID;

      if Object.XDG_Toplevel.Has_Proxy and ID'Length > 0 then
         Object.XDG_Toplevel.Set_App_Id (ID);
      end if;
   end Set_Application_ID;

   overriding
   procedure Set_Title (Object : in out Wayland_Window; Title : String) is
   begin
      Object.Restore_Title := +Title;

      if Object.XDG_Toplevel.Has_Proxy then
         Object.XDG_Toplevel.Set_Title (Title);
      end if;
   end Set_Title;

   overriding
   procedure Close (Object : in out Wayland_Window) is
   begin
      Object.Should_Close := True;
   end Close;

   overriding
   function Should_Close (Object : Wayland_Window) return Boolean is
   begin
      return Object.Should_Close;
   end Should_Close;

   procedure Make_Current
     (Object  : in out Wayland_Window;
      Context : Standard.EGL.Objects.Contexts.Context)
   is
      use type Standard.EGL.Objects.Contexts.Context;
   begin
      if Context /= Object.EGL_Context then
         raise Constraint_Error with "Window does not belong to given context";
      end if;

      Object.EGL_Context.Make_Current (Object.EGL_Surface);
      Object.EGL_Context.Set_Swap_Interval (0);
   end Make_Current;

   overriding
   procedure Swap_Buffers (Object : in out Wayland_Window) is
      Time_To_Swap : Duration;
      Do_Swap      : Boolean;
   begin
      Object.Frame_Handler.Before_Swap_Buffers (Time_To_Swap, Do_Swap);
      Wayland_Window'Class (Object).Sleep_Until_Swap (Time_To_Swap);
      if Do_Swap then
         Object.EGL_Surface.Swap_Buffers;
      end if;
      Object.Frame_Handler.After_Swap_Buffers;
   end Swap_Buffers;

   overriding
   procedure Set_Vertical_Sync (Object : in out Wayland_Window; Enable : Boolean) is
   begin
      raise Program_Error with "Wayland always uses vsync to prevent tearing";
   end Set_Vertical_Sync;

   overriding
   function On_Close (Object : Wayland_Window) return Boolean is
   begin
      return True;
   end On_Close;

   function On_Change_Cursor
     (Object : in out Wayland_Window;
      Name   : AWT.Inputs.Cursors.Pointer_Cursor;
      Cursor : WC.Cursor'Class) return WC.Cursor_Image'Class is
   begin
      return Cursor.Image (WC.Image_Index'First);
   end On_Change_Cursor;

   procedure Set_Cursor (Object : in out Wayland_Window) is
      use all type AWT.Inputs.Dimension;
      use type Unsigned_32;
   begin
      if Global.Seat.Pointer_Enter_Serial /= 0 then
         Global.Seat.Pointer.Set_Cursor
           (Global.Seat.Pointer_Enter_Serial,
            Object.Cursor_Surface, Object.Cursor_Hotspot (X), Object.Cursor_Hotspot (Y));
      end if;
   end Set_Cursor;

   procedure Unset_Cursor is
      Null_Surface : WP.Client.Surface;
      use type Unsigned_32;
   begin
      if Global.Seat.Pointer_Enter_Serial /= 0 then
         Global.Seat.Pointer.Set_Cursor
           (Global.Seat.Pointer_Enter_Serial,
            Null_Surface, 0, 0);
      end if;
   end Unset_Cursor;

   procedure Restore_Cursor (Object : in out Wayland_Window) is
      use all type AWT.Inputs.Pointer_Mode;
   begin
      if Object.Pointer_State.Mode = Visible then
         Object.Set_Cursor;
      else
         Unset_Cursor;
      end if;
   end Restore_Cursor;

   overriding
   procedure Set_Pointer_Cursor
     (Object : in out Wayland_Window;
      Cursor : AWT.Inputs.Cursors.Pointer_Cursor)
   is
      use all type AWT.Inputs.Cursors.Pointer_Cursor;

      --  See XDG cursor spec [1] and CSS3 UI spec [2, 3] for a list of names
      --
      --  [1] https://www.freedesktop.org/wiki/Specifications/cursor-spec/
      --  [2] https://developer.mozilla.org/en-US/docs/Web/CSS/cursor
      --  [3] https://drafts.csswg.org/css-ui-3/#cursor
      Name : constant String :=
        (case Cursor is
           when Default => "default",

           --  Links and status
           when Context_Menu  => "context-menu",
           when Help          => "help",
           when Pointer       => "pointer",
           when Progress      => "progress",
           when Wait          => "wait",

           --  Selection
           when Cell          => "cell",
           when Crosshair     => "crosshair",
           when Text          => "text",
           when Vertical_Text => "vertical-text",

           --  Drag and drop
           when Alias       => "alias",
           when Copy        => "copy",
           when Move        => "move",      --  CSS3 UI
           when No_Drop     => "no-drop",
           when Not_Allowed => "not-allowed",
           when Grab        => "grab",      --  CSS3 UI
           when Grabbing    => "grabbing",  --  CSS3 UI

           --  Resizing and scrolling
           when All_Scroll  => "all-scroll",
           when Col_Resize  => "col-resize",
           when Row_Resize  => "row-resize",
           when N_Resize    => "n-resize",
           when E_Resize    => "e-resize",
           when S_Resize    => "s-resize",
           when W_Resize    => "w-resize",
           when NE_Resize   => "ne-resize",
           when NW_Resize   => "nw-resize",
           when SE_Resize   => "se-resize",
           when SW_Resize   => "sw-resize",
           when EW_Resize   => "ew-resize",
           when NS_Resize   => "ns-resize",
           when NESW_Resize => "nesw-resize",
           when NWSE_Resize => "nwse-resize",

           --  Zooming
           when Zoom_In  => "zoom-in",    --  CSS3 UI
           when Zoom_Out => "zoom-out");  --  CSS3 UI

      Cursor_Object : WC.Cursor'Class :=
        Global.Cursor_Theme.Get_Cursor (Name);

      Image : WC.Cursor_Image'Class :=
        Wayland_Window'Class (Object).On_Change_Cursor (Cursor, Cursor_Object);

      State : constant WC.Image_State := Image.State;
   begin
      Object.Cursor_Surface.Attach (Image.Get_Buffer, 0, 0);
      Object.Cursor_Surface.Damage (0, 0, State.Width, State.Height);
      Object.Cursor_Surface.Commit;

      Object.Cursor_Hotspot := (State.Hotspot_X, State.Hotspot_Y);

      Object.Cursor := Cursor;
      Object.Cursor_Images := Cursor_Object.Length;

      Object.Restore_Cursor;
   end Set_Pointer_Cursor;

   overriding
   procedure Set_Pointer_Mode
     (Object : in out Wayland_Window;
      Mode   : AWT.Inputs.Pointer_Mode)
   is
      use all type AWT.Inputs.Pointer_Mode;
   begin
      if Object.Pointer_State.Mode = Mode then
         return;
      end if;

      case Mode is
         when Visible | Hidden =>
            if Object.Locked_Pointer.Has_Proxy then
               Object.Unlocked_Pointer_Mode := Mode;
               Object.Locked_Pointer.Destroy;
            end if;
            Object.Pointer_State.Mode := Mode;
            Object.Restore_Cursor;
         when Locked =>
            if Object.Locked_Pointer.Has_Proxy then
               Object.Pointer_State.Mode := Locked;
            else
               Object.Unlocked_Pointer_Mode := Object.Pointer_State.Mode;

               declare
                  Region : WP.Client.Region;
               begin
                  Global.Pointer_Constraints.Lock_Pointer
                    (Object.Surface, Global.Seat.Pointer, Region,
                     WE.Pointer_Constraints_Unstable_V1.Oneshot,
                     Object.Locked_Pointer);
               end;

               if Object.Locked_Pointer.Has_Proxy then
                  --  It might happen that the current position gets updated
                  --  one or a few times between the request and the "locked" event. We
                  --  need to make sure the compositor warps the cursor back to the
                  --  position at the time of the request
                  Object.Locked_Position := Object.Pointer_State.Position;

                  Locked_Pointer_Events.Subscribe (Object.Locked_Pointer);
               end if;
            end if;

            Unset_Cursor;
      end case;
   end Set_Pointer_Mode;

   overriding
   function State (Object : Wayland_Window) return AWT.Inputs.Pointer_State is
   begin
      return Object.Pointer_State;
   end State;

   overriding
   function State (Object : Wayland_Window) return AWT.Inputs.Keyboard_State is
   begin
      return Object.Keyboard_State;
   end State;

   procedure Set_State
     (Object : in out Wayland_Window;
      State  : AWT.Inputs.Pointer_State)
   is
      Mode : constant AWT.Inputs.Pointer_Mode := Object.Pointer_State.Mode;

      use all type AWT.Inputs.Pointer_Mode;
   begin
      Object.Pointer_State := State;
      Object.Pointer_State.Mode := Mode;
      if Mode /= Locked then
         Object.Pointer_State.Relative := (others => 0.0);
      end if;
   end Set_State;

   procedure Set_State
     (Object : in out Wayland_Window;
      State  : AWT.Inputs.Keyboard_State) is
   begin
      Object.Keyboard_State := State;
   end Set_State;

   overriding
   function Raw_Pointer_Motion (Object : Wayland_Window) return Boolean is
   begin
      return Object.Raw_Pointer_Motion;
   end Raw_Pointer_Motion;

   overriding
   procedure Set_Raw_Pointer_Motion (Object : in out Wayland_Window; Enable : Boolean) is
   begin
      Object.Raw_Pointer_Motion := Enable;
   end Set_Raw_Pointer_Motion;

   procedure Update_Animated_Cursor (Window : not null access Wayland_Window) is
      use all type AWT.Inputs.Pointer_Mode;
   begin
      if Window.Cursor_Images > 1 and Window.Pointer_State.Mode = Visible then
         Wayland_Window'Class (Window.all).Update_Cursor;
      end if;
   end Update_Animated_Cursor;

   procedure Set_EGL_Data
     (Object  : in out Wayland_Window;
      Context : EGL.Objects.Contexts.Context;
      Config  : EGL.Objects.Configs.Config;
      sRGB    : Boolean) is
   begin
      Object.EGL_Context := Context;
      Object.EGL_Config  := Config;
      Object.EGL_sRGB    := sRGB;
   end Set_EGL_Data;

   procedure Ask_Feedback
     (Object   : in out Wayland_Window;
      Feedback : in out Feedback_With_Frame) is
   begin
      if Feedback.Has_Proxy then
         raise Internal_Error with
           "Wayland: Feedback object " & Feedback.Data.Index'Image & " still in use";
      end if;

      Global.Presentation.Feedback (Object.Surface, Feedback);
      if not Feedback.Has_Proxy then
         raise Internal_Error with "Wayland: Failed to get feedback";
      end if;

      Feedback_Events.Subscribe (Feedback);
   end Ask_Feedback;

   protected body Frame_Handler_With_Window is
      entry Before_Swap_Buffers (Time_To_Swap : out Duration; Do_Swap : out Boolean)
        when Pending < Frames'Length
      is
         Clock : constant Duration := Orka.OS.Monotonic_Clock;
      begin
         Swapping := True;

         Do_Swap := Has_Buffer;

         if Latest_Stop /= 0.0 then
            for I in 1 .. 6 loop
               declare
                  Next_Swap_Time : constant Duration :=
                    Latest_Stop - Max_In_Flight + I * Default_Refresh;
               begin
                  if Next_Swap_Time > Clock then
                     Time_To_Swap := Next_Swap_Time;
                     return;
                  end if;
               end;
            end loop;
         end if;

         Time_To_Swap := Clock;
      end Before_Swap_Buffers;

      procedure After_Swap_Buffers is
         Clock : constant Duration := Orka.OS.Monotonic_Clock;

         use type Unsigned_32;
      begin
         Swapping := False;

         if Pending < Frames'Length and Has_Buffer then
            for Data of Frames loop
               if not Data.Feedback.Has_Proxy then
                  Window.Ask_Feedback (Data.Feedback);
                  Data.Start := Clock;
                  Pending := Pending + 1;
                  exit;
               end if;
            end loop;
         end if;

         if Resize_Width > 0 and Resize_Height > 0 then
            Window.Do_Resize (Resize_Width, Resize_Height, Resize_Margin);

            if Resize_Serial /= 0 then
               Window.XDG_Surface.Ack_Configure (Resize_Serial);
            end if;

            Wayland_Window'Class (Window.all).On_Configure (Window.Current_State);

            Resize_Width  := 0;
            Resize_Height := 0;
            Resize_Serial := 0;
         end if;
      end After_Swap_Buffers;

      procedure Set_Size (Width, Height : Positive; Margin : Natural; Serial : Unsigned_32) is
      begin
         Resize_Width  := Width;
         Resize_Height := Height;
         Resize_Margin := Margin;
         Resize_Serial := Serial;
      end Set_Size;

      entry Set_Has_Buffer (Value : Boolean) when not Swapping is
      begin
         Has_Buffer := Value;

         if not Value then
            declare
               Buffer : WP.Client.Buffer;
            begin
               Window.Surface.Attach (Buffer, 0, 0);
               Window.Surface.Commit;
            end;
         end if;
      end Set_Has_Buffer;

      procedure On_Frame_Presented (Index : Frame_Index; Timestamp, Refresh : Duration) is
         Current_Frame : Frame renames Frames (Index);
      begin
         Pending := Pending - 1;

         Current_Frame.Stop := Timestamp;

         Max_In_Flight := Duration'Max (Max_In_Flight, Current_Frame.Stop - Current_Frame.Start);
         Latest_Stop   := Duration'Max (Latest_Stop, Timestamp);
      end On_Frame_Presented;

      procedure On_Frame_Discarded (Index : Frame_Index) is
      begin
         Pending := Pending - 1;
      end On_Frame_Discarded;

      procedure On_Frame_Output (Index : Frame_Index; Refresh : Duration) is
      begin
         Default_Refresh := Refresh;
      end On_Frame_Output;

      procedure Finalize is
      begin
         for Data of Frames loop
            if Data.Feedback.Has_Proxy then
               Data.Feedback.Destroy;
            end if;
         end loop;
      end Finalize;
   end Frame_Handler_With_Window;

   function Make_Frames (Window : not null access Wayland_Window) return Frame_Array is
   begin
      return Result : Frame_Array do
         for Index in Result'Range loop
            Result (Index).Index  := Index;
            Result (Index).Window := Window;
         end loop;
      end return;
   end Make_Frames;

end AWT.Wayland.Windows;
