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

with Ada.Unchecked_Conversion;

with Wayland.Enums.Client;
with Wayland.Enums.Presentation_Time;
with Wayland.Enums.Pointer_Constraints_Unstable_V1;
with Wayland.Enums.Xdg_Shell;

with Wayland.EGL.AWT;

with AWT.Registry;

package body AWT.Wayland.Windows is

   Global : AWT.Registry.Compositor renames AWT.Registry.Global;

   package Output_Data renames AWT.Registry.Output_Data;

   No_Buffer_Frame_Time : constant := 0.1;
   --  Frame time used if window is hidden. This is an arbitrary high
   --  frame time to keep a render task running at a low frequency.

   Default_Frame_Time : constant := 0.016_667;
   --  Frame time used if window is visible but has never been presented
   --  recently

   ----------------------------------------------------------------------------

   procedure Locked_Pointer_Locked
     (Pointer : in out WP.Pointer_Constraints_Unstable_V1.Locked_Pointer_V1'Class)
   is
      Object : constant not null access Wayland_Window :=
        Locked_Pointer_With_Window (Pointer).Window;
   begin
      Object.Window.Lock_Pointer;
   end Locked_Pointer_Locked;

   procedure Locked_Pointer_Unlocked
     (Pointer : in out WP.Pointer_Constraints_Unstable_V1.Locked_Pointer_V1'Class)
   is
      Object : constant not null access Wayland_Window :=
        Locked_Pointer_With_Window (Pointer).Window;
   begin
      Object.Window.Unlock_Pointer;
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

   procedure Frame_Done
     (Callback      : in out WP.Client.Callback'Class;
      Callback_Data : Unsigned_32)
   is
      Window : constant not null access Wayland_Window :=
        Callback_With_Window (Callback).Window;
   begin
      Callback.Destroy;
      Window.Frame_Handler.On_Frame_Done (Orka.OS.Monotonic_Clock);
   end Frame_Done;

   ----------------------------------------------------------------------------

   package Locked_Pointer_Events is new PC.Locked_Pointer_V1_Events
     (Locked   => Locked_Pointer_Locked,
      Unlocked => Locked_Pointer_Unlocked);

   package Surface_Events is new WP.Client.Surface_Events
     (Enter => Surface_Enter,
      Leave => Surface_Leave);

   package Frame_Callback_Events is new WP.Client.Callback_Events
     (Done => Frame_Done);

   ----------------------------------------------------------------------------

   procedure XDG_Surface_Configure
     (Xdg_Surface : in out WP.Xdg_Shell.Xdg_Surface'Class;
      Serial      : Unsigned_32)
   is
      Object : constant not null access Wayland_Window :=
        Xdg_Surface_With_Window (Xdg_Surface).Window;
   begin
      Object.Window.Configure_Surface (Serial);
   end XDG_Surface_Configure;

   procedure XDG_Toplevel_Configure
     (Xdg_Toplevel : in out WP.Xdg_Shell.Xdg_Toplevel'Class;
      Width        : Natural;
      Height       : Natural;
      States       : WP.Xdg_Shell.State_Array)
   is
      Object : constant not null access Wayland_Window :=
        Xdg_Toplevel_With_Window (Xdg_Toplevel).Window;
   begin
      Object.Window.Configure_Toplevel (Width, Height, States);
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
      Object : constant not null access Wayland_Window :=
        Toplevel_Decoration_With_Window (Decoration).Window;
   begin
      Object.Window.Configure_Decoration (Mode);
   end XDG_Toplevel_Decoration_Configure;

   -----------------------------------------------------------------------------

   procedure Feedback_Synchronized_Output
     (Feedback : in out WP.Presentation_Time.Presentation_Feedback'Class;
      Output   : WP.Client.Output'Class)
   is
      Window : constant not null access Wayland_Window :=
        Feedback_With_Frame (Feedback).Data.Window;

      Monitor : constant not null access AWT.Registry.Monitor_Device :=
        Output_Data.Get_Data (Output).Monitor;
   begin
      Window.Frame_Handler.On_Frame_Output (Monitor.Current_State.Refresh);
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
   begin
      Feedback.Destroy;
      Window.Frame_Handler.On_Frame_Presented (Timestamp, Refresh);
   end Feedback_Presented;

   procedure Feedback_Discarded
     (Feedback : in out WP.Presentation_Time.Presentation_Feedback'Class)
   is
      Window : constant not null access Wayland_Window :=
        Feedback_With_Frame (Feedback).Data.Window;
   begin
      Feedback.Destroy;
      Window.Frame_Handler.On_Frame_Discarded;
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

   protected body Window_Object is

      procedure Lock_Pointer is
         use all type AWT.Inputs.Dimension;
         use all type AWT.Inputs.Pointer_Mode;
      begin
         Locked_Pointer.Set_Cursor_Position_Hint
           (Standard.Wayland.Fixed (Locked_Position (X)),
            Standard.Wayland.Fixed (Locked_Position (Y)));
         Set_Pointer_Mode (Locked);
      end Lock_Pointer;

      procedure Unlock_Pointer is
      begin
         Set_Pointer_Mode (Unlocked_Pointer_Mode);
      end Unlock_Pointer;

      procedure Set_Opaque
        (X, Y          : Natural;
         Width, Height : Natural)
      is
         Region : WP.Client.Region;
      begin
         Global.Compositor.Create_Region (Region);

         if not Region.Has_Proxy then
            raise Internal_Error with "Wayland: Failed to get region";
         end if;

         Region.Add (0, 0, Width, Height);
         Surface.Set_Opaque_Region (Region);
         Region.Destroy;
      end Set_Opaque;

      procedure Set_Pointer_Input
        (X, Y          : Natural;
         Width, Height : Natural)
      is
         Region : WP.Client.Region;
      begin
         Global.Compositor.Create_Region (Region);

         if not Region.Has_Proxy then
            raise Internal_Error with "Wayland: Failed to get region";
         end if;

         Region.Add (X, Y, Width, Height);
         Surface.Set_Input_Region (Region);
         Region.Destroy;
      end Set_Pointer_Input;

      procedure Set_Window_Margin
        (Margin        : Natural;
         Width, Height : Positive)
      is
         X : Natural renames Margin;
         Y : Natural renames Margin;
      begin
         XDG_Surface.Set_Window_Geometry (X, Y, Width, Height);
         Set_Pointer_Input (X, Y, Width, Height);

         if not Pending_State.Transparent then
            Set_Opaque (X, Y, Width, Height);
         end if;

         Pending_State.Width  := Width;
         Pending_State.Height := Height;

         Pending_State.Margin := Margin;
      end Set_Window_Margin;

      procedure Set_Idle_Inhibit (Enable : Boolean) is
      begin
         if not Global.Inhibit_Manager.Has_Proxy then
            return;
         end if;

         if Enable and not Idle_Inhibitor.Has_Proxy then
            if Global.Inhibit_Manager.Has_Proxy then
               Global.Inhibit_Manager.Create_Inhibitor (Surface, Idle_Inhibitor);
            else
               --  Cannot inhibit screensaver due to missing inhibit manager
               null;
            end if;
         elsif not Enable and Idle_Inhibitor.Has_Proxy then
            Idle_Inhibitor.Destroy;
         end if;
      end Set_Idle_Inhibit;

      procedure Acknowledge_Configure (Serial : Unsigned_32) is
         use type Unsigned_32;
      begin
         if Serial /= 0 then
            XDG_Surface.Ack_Configure (Serial);
         end if;
         Wayland_Window'Class (Window.all).On_Configure (Current_State);
      end Acknowledge_Configure;

      procedure Do_Resize
        (Width, Height : Positive;
         Margin        : Natural;
         Serial        : Unsigned_32)
      is
         Scale : Positive renames Pending_Scale;
      begin
         EGL_Window.Resize
           ((Width  => Scale * (Width + 2 * Margin),
             Height => Scale * (Height + 2 * Margin)));
         Set_Window_Margin (Margin, Width, Height);

         Current_State.Width  := Width;
         Current_State.Height := Height;

         Current_Scale := Scale;

         Acknowledge_Configure (Serial);
      end Do_Resize;

      procedure Detach_Buffer is
         Buffer : WP.Client.Buffer;
      begin
         Surface.Attach (Buffer, 0, 0);
         Surface.Commit;
      end Detach_Buffer;

      function Is_Configuring return Boolean is (Initial_Configure);

      procedure Configure_Surface (Serial : Unsigned_32) is
         State : AWT.Windows.Window_State renames Pending_State;

         Size_Changed : constant Boolean :=
           Current_State.Width /= State.Width or
             Current_State.Height /= State.Height;
      begin
         Current_State := State;

         if Initial_Configure then
            Initial_Configure := False;

            Do_Resize (State.Width, State.Height, State.Margin, Serial);

            Window.Frame_Handler.Set_Has_Buffer (True);
         else
            Window.Frame_Handler.Set_Size (State.Width, State.Height, State.Margin, Serial);
         end if;
      end Configure_Surface;

      procedure Configure_Toplevel
        (Width  : Natural;
         Height : Natural;
         States : WP.Xdg_Shell.State_Array)
      is
         use all type WE.Xdg_Shell.Xdg_Toplevel_State;
         use all type AWT.Windows.Size_Mode;
         use all type AWT.Windows.Window_State;

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

         Pending_State.Width  := (if Width > 0 then Width else Restore_Width);
         Pending_State.Height := (if Height > 0 then Height else Restore_Height);

         if Is_Fullscreen then
            Pending_State.Mode := Fullscreen;
         elsif Is_Maximized then
            Pending_State.Mode := Maximized;
         else
            Pending_State.Mode := Default;
         end if;

         declare
            State : AWT.Windows.Window_State renames Pending_State;

            State_Changed : constant Boolean :=
               Current_State /= State;

            Size_Changed : constant Boolean :=
              Current_State.Width /= State.Width or
                Current_State.Height /= State.Height;

            Mode_Changed : constant Boolean :=
              Current_State.Mode /= State.Mode;
         begin
            if Size_Changed then
               Set_Window_Margin (State.Margin, State.Width, State.Height);
            end if;

            if Mode_Changed then
               Set_Idle_Inhibit (State.Mode = Fullscreen);

               --  Temporarily remove any margin set by the user if the
               --  window is maximized or fullscreen
               if State.Mode in Maximized | Fullscreen then
                  Set_Window_Margin (0, State.Width, State.Height);
               elsif Current_State.Mode in Maximized | Fullscreen then
                  Set_Window_Margin (Restore_Margin, State.Width, State.Height);
               end if;
            end if;
         end;
      end Configure_Toplevel;

      procedure Configure_Decoration
        (Mode : WE.Xdg_Decoration_Unstable_V1.Toplevel_Decoration_V1_Mode)
      is
         use all type WE.Xdg_Decoration_Unstable_V1.Toplevel_Decoration_V1_Mode;
      begin
         Pending_State.Decorated := Mode = Server_Side;
      end Configure_Decoration;

      procedure Finalize is
      begin
         Window.Frame_Handler.Finalize;

         if EGL_Window.Is_Initialized then
            EGL_Window.Destroy;
         end if;

         if Idle_Inhibitor.Has_Proxy then
            Idle_Inhibitor.Destroy;
         end if;

         if Locked_Pointer.Has_Proxy then
            Locked_Pointer.Destroy;
         end if;

         if Decoration.Has_Proxy then
            Decoration.Destroy;
         end if;

         if XDG_Toplevel.Has_Proxy then
            XDG_Toplevel.Destroy;
         end if;

         if XDG_Surface.Has_Proxy then
            XDG_Surface.Destroy;
         end if;

         if Surface.Has_Proxy then
            Surface.Destroy;
         end if;

         if Cursor_Surface.Has_Proxy then
            Cursor_Surface.Destroy;
         end if;
      end Finalize;

      procedure Create_XDG_Window
        (ID, Title            : String;
         Width, Height        : Positive;
         Resizable, Decorated : Boolean)
      is
         use all type WE.Xdg_Decoration_Unstable_V1.Toplevel_Decoration_V1_Mode;
      begin
         Global.XDG_Shell.Get_Surface (Surface, XDG_Surface);
         if not XDG_Surface.Has_Proxy then
            raise Internal_Error with "Wayland: Failed to get XDG surface";
         end if;

         XDG_Surface.Get_Toplevel (XDG_Toplevel);
         if not XDG_Toplevel.Has_Proxy then
            raise Internal_Error with "Wayland: Failed to get XDG toplevel";
         end if;

         XDG_Surface_Events.Subscribe (XDG_Surface);
         XDG_Toplevel_Events.Subscribe (XDG_Toplevel);

         if Decorated and Global.XDG_Decoration_Manager.Has_Proxy then
            Global.XDG_Decoration_Manager.Get_Toplevel_Decoration
              (XDG_Toplevel, Decoration);

            if not Decoration.Has_Proxy then
               raise Internal_Error with "Wayland: Failed to get XDG decoration";
            end if;

            XDG_Toplevel_Decoration_Events.Subscribe (Decoration);

            Decoration.Set_Mode (Server_Side);
         end if;

         Pending_State.Width  := Width;
         Pending_State.Height := Height;

         Restore_Width  := Width;
         Restore_Height := Height;

         if not Resizable then
            Set_Size_Limits (Width, Height, Width, Height);
         end if;

         Set_Pointer_Cursor (Cursor_Name);

         Set_Application_ID (ID);
         Set_Title (Title);

         -------------------------------------------------------------------------

         Initial_Configure := True;

         Surface.Commit;
      end Create_XDG_Window;

      procedure Create_Window
        (ID, Title                     : String;
         Width, Height                 : Positive;
         Visible, Resizable, Decorated : Boolean := True;
         Transparent                   : Boolean := False) is
      begin
         if not Global.XDG_Shell.Has_Proxy then
            raise Internal_Error with "Wayland: No XDG shell available";
         end if;

         Global.Compositor.Create_Surface (Cursor_Surface);
         if not Cursor_Surface.Has_Proxy then
            raise Internal_Error with "Wayland: Failed to get cursor surface";
         end if;

         Global.Compositor.Create_Surface (Surface);
         if not Surface.Has_Proxy then
            raise Internal_Error with "Wayland: Failed to get surface";
         end if;

         Surface_Events.Subscribe (Surface);

         -------------------------------------------------------------------------

         EGL_Window.Create_Window (WP.Client.Surface (Surface), Width, Height);

         EGL_Surface := Standard.Wayland.EGL.AWT.Create_Surface
           (EGL_Window, EGL_Context.Display, EGL_Config, EGL_sRGB);

         pragma Assert (EGL_Surface.Width = Width);
         pragma Assert (EGL_Surface.Height = Height);

         -------------------------------------------------------------------------

         Pending_State.Visible     := Visible;
         Pending_State.Resizable   := Resizable;
         Pending_State.Transparent := Transparent;

         Create_XDG_Window (ID, Title, Width, Height, Resizable, Decorated);
      end Create_Window;

      procedure Set_Size (Width, Height : Positive) is
         State : AWT.Windows.Window_State renames Current_State;
      begin
         Window.Frame_Handler.Set_Size (Width, Height, State.Margin, 0);
      end Set_Size;

      procedure Set_Size_Limits
        (Min_Width, Min_Height, Max_Width, Max_Height : Natural) is
      begin
         if Pending_State.Resizable then
            XDG_Toplevel.Set_Min_Size (Min_Width, Min_Height);
            XDG_Toplevel.Set_Max_Size (Max_Width, Max_Height);
         end if;
      end Set_Size_Limits;

      procedure Set_Margin
        (Margin : Natural)
      is
         Width  : Positive renames Pending_State.Width;
         Height : Positive renames Pending_State.Height;
      begin
         Restore_Margin := Margin;
         Window.Frame_Handler.Set_Size (Width, Height, Margin, 0);
      end Set_Margin;

      procedure Set_Framebuffer_Scale (Scale : Positive) is
      begin
         Pending_Scale := Scale;
         Surface.Set_Buffer_Scale (Scale);
      end Set_Framebuffer_Scale;

      procedure Set_Size_Mode (Mode : AWT.Windows.Size_Mode) is
         use all type AWT.Windows.Size_Mode;
      begin
         if Pending_State.Mode = Default and Mode /= Default then
            Restore_Width  := Pending_State.Width;
            Restore_Height := Pending_State.Height;
         end if;

         case Mode is
            when Default =>
               XDG_Toplevel.Set_Maximized (False);
               XDG_Toplevel.Set_Fullscreen (False);
            when Minimized =>
               XDG_Toplevel.Minimize;
            when Maximized =>
               XDG_Toplevel.Set_Maximized (True);
               XDG_Toplevel.Set_Fullscreen (False);
            when Fullscreen =>
               XDG_Toplevel.Set_Fullscreen (True);
         end case;
      end Set_Size_Mode;

      procedure Set_Size_Mode
        (Mode    : AWT.Windows.Size_Mode;
         Monitor : AWT.Monitors.Monitor'Class) is
      begin
         XDG_Toplevel.Set_Fullscreen (True, AWT.Registry.Monitor_Device (Monitor).Output);
      end Set_Size_Mode;

      procedure Set_Visible (Visible : Boolean) is
         State : AWT.Windows.Window_State renames Current_State;
      begin
         if State.Visible = Visible then
            return;
         end if;

         Pending_State.Visible := Visible;

         if Visible then
            if XDG_Toplevel.Has_Proxy then
               XDG_Toplevel.Destroy;
            end if;

            if XDG_Surface.Has_Proxy then
               XDG_Surface.Destroy;
            end if;

            Create_XDG_Window (+Restore_ID, +Restore_Title,
              Restore_Width, Restore_Height, State.Resizable, State.Decorated);
         else
            Restore_Width  := Pending_State.Width;
            Restore_Height := Pending_State.Height;

            Window.Frame_Handler.Set_Has_Buffer (False);
            State.Visible := Pending_State.Visible;
         end if;
      end Set_Visible;

      procedure Set_Application_ID (ID : String) is
      begin
         Restore_ID := +ID;

         if XDG_Toplevel.Has_Proxy and ID'Length > 0 then
            XDG_Toplevel.Set_App_Id (ID);
         end if;
      end Set_Application_ID;

      procedure Set_Title (Title : String) is
      begin
         Restore_Title := +Title;

         if XDG_Toplevel.Has_Proxy then
            XDG_Toplevel.Set_Title (Title);
         end if;
      end Set_Title;

      procedure Make_Current (Context : Standard.EGL.Objects.Contexts.Context) is
         use type Standard.EGL.Objects.Contexts.Context;
      begin
         if Context /= EGL_Context then
            raise Constraint_Error with "Window does not belong to given context";
         end if;

         EGL_Context.Make_Current (EGL_Surface);
         EGL_Context.Set_Swap_Interval (0);
      end Make_Current;

      procedure On_Change_Cursor
        (Name   : AWT.Inputs.Cursors.Pointer_Cursor;
         Cursor : WC.Cursor'Class;
         Index  : out WC.Image_Index)
      is
         use all type AWT.Inputs.Cursors.Pointer_Cursor;

         Current_Time : constant Duration := Orka.OS.Monotonic_Clock;
      begin
         if Name /= Cursor_Name then
            Start_Time := Current_Time;
         end if;

         declare
            Elapsed_Time : constant Duration := Current_Time - Start_Time;
            Remaining    : Duration;
         begin
            Index := Cursor.Index_At_Elapsed_Time (Elapsed_Time, Remaining);
            Next_Time := Current_Time + Remaining;
         end;
      end On_Change_Cursor;

      procedure Set_Cursor is
         use all type AWT.Inputs.Dimension;
         use type Unsigned_32;
      begin
         if Global.Seat.Pointer_Enter_Serial /= 0 then
            Global.Seat.Pointer.Set_Cursor
              (Global.Seat.Pointer_Enter_Serial,
               Cursor_Surface, Cursor_Hotspot (X), Cursor_Hotspot (Y));
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

      procedure Restore_Cursor is
         use all type AWT.Inputs.Pointer_Mode;
      begin
         if Pointer_State.Mode = Visible then
            Set_Cursor;
         else
            Unset_Cursor;
         end if;
      end Restore_Cursor;

      procedure Set_Pointer_Cursor (Cursor : AWT.Inputs.Cursors.Pointer_Cursor) is
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

         Index  : WC.Image_Index;
      begin
         On_Change_Cursor (Cursor, Cursor_Object, Index);

         declare
            Image : WC.Cursor_Image'Class := Cursor_Object.Image (Index);

            State : constant WC.Image_State := Image.State;
         begin
            Cursor_Surface.Attach (Image.Get_Buffer, 0, 0);
            Cursor_Surface.Damage (0, 0, State.Width, State.Height);
            Cursor_Surface.Commit;

            Cursor_Hotspot := (State.Hotspot_X, State.Hotspot_Y);
         end;

         Cursor_Name   := Cursor;
         Cursor_Images := Cursor_Object.Length;

         Restore_Cursor;
      end Set_Pointer_Cursor;

      procedure Set_Pointer_Mode (Mode : AWT.Inputs.Pointer_Mode) is
         use all type AWT.Inputs.Pointer_Mode;
      begin
         if Pointer_State.Mode = Mode then
            return;
         end if;

         case Mode is
            when Visible | Hidden =>
               if Locked_Pointer.Has_Proxy then
                  Unlocked_Pointer_Mode := Mode;
                  Locked_Pointer.Destroy;
               end if;
               Pointer_State.Mode := Mode;
               Restore_Cursor;
            when Locked =>
               if Locked_Pointer.Has_Proxy then
                  Pointer_State.Mode := Locked;
               else
                  Unlocked_Pointer_Mode := Pointer_State.Mode;

                  declare
                     Region : WP.Client.Region;
                  begin
                     Global.Pointer_Constraints.Lock_Pointer
                       (Surface, Global.Seat.Pointer, Region,
                        WE.Pointer_Constraints_Unstable_V1.Oneshot,
                        Locked_Pointer);
                  end;

                  if Locked_Pointer.Has_Proxy then
                     --  It might happen that the current position gets updated
                     --  one or a few times between the request and the "locked" event. We
                     --  need to make sure the compositor warps the cursor back to the
                     --  position at the time of the request
                     Locked_Position := Pointer_State.Position;

                     Locked_Pointer_Events.Subscribe (Locked_Pointer);
                  end if;
               end if;

               Unset_Cursor;
         end case;
      end Set_Pointer_Mode;

      function State return AWT.Windows.Window_State is
      begin
         return Current_State;
      end State;

      function State return AWT.Windows.Framebuffer_State is
         State : AWT.Windows.Window_State renames Current_State;
      begin
         return (Width  => State.Width + 2 * State.Margin,
                 Height => State.Height + 2 * State.Margin,
                 Scale  => Current_Scale);
      end State;

      procedure State (Result : out AWT.Inputs.Pointer_State) is
      begin
         Result := Pointer_State;
         Reset_Input := True;
      end State;

      procedure State (Result : out AWT.Inputs.Keyboard_State) is
      begin
         Result := Keyboard_State;
         Reset_Input := True;
      end State;

      procedure Reset_Input_State is
      begin
         if Reset_Input then
            --  Reset some state after the state has been read by
            --  the application. New state will be accumulated in procedure Set_State

            Pointer_State.Relative := (others => 0.0);
            Pointer_State.Scroll   := (others => 0.0);

            Pointer_State.Pressed  := (others => False);
            Pointer_State.Released := (others => False);

            Keyboard_State.Pressed  := (others => False);
            Keyboard_State.Released := (others => False);

            Reset_Input := False;
         end if;
      end Reset_Input_State;

      procedure Set_State (State : AWT.Inputs.Pointer_State) is
         Mode : constant AWT.Inputs.Pointer_Mode := Pointer_State.Mode;

         use all type AWT.Inputs.Pointer_Mode;

         function Convert is new Ada.Unchecked_Conversion
           (Source => AWT.Inputs.Pointer_Buttons,
            Target => AWT.Inputs.Changed_Buttons);

         use type AWT.Inputs.Changed_Buttons;

         Old_State : constant AWT.Inputs.Pointer_State := Pointer_State;

         Old_State_Buttons : constant AWT.Inputs.Changed_Buttons := Convert (Old_State.Buttons);
         New_State_Buttons : constant AWT.Inputs.Changed_Buttons := Convert (State.Buttons);

         Changed : constant AWT.Inputs.Changed_Buttons := Old_State_Buttons xor New_State_Buttons;
      begin
         Pointer_State := State;
         Pointer_State.Pressed  := Old_State.Pressed  or (Changed and     New_State_Buttons);
         Pointer_State.Released := Old_State.Released or (Changed and not New_State_Buttons);
         Pointer_State.Mode := Mode;
         if Mode /= Locked then
            Pointer_State.Relative := (others => 0.0);
         end if;
      end Set_State;

      procedure Set_State (State : AWT.Inputs.Keyboard_State) is
         function Convert is new Ada.Unchecked_Conversion
           (Source => AWT.Inputs.Keyboard_Keys,
            Target => AWT.Inputs.Changed_Keys);

         use type AWT.Inputs.Changed_Keys;

         Old_State : constant AWT.Inputs.Keyboard_State := Keyboard_State;

         Old_State_Buttons : constant AWT.Inputs.Changed_Keys := Convert (Old_State.Buttons);
         New_State_Buttons : constant AWT.Inputs.Changed_Keys := Convert (State.Buttons);

         Changed : constant AWT.Inputs.Changed_Keys := Old_State_Buttons xor New_State_Buttons;
      begin
         Keyboard_State := State;
         Keyboard_State.Pressed  := Old_State.Pressed  or (Changed and     New_State_Buttons);
         Keyboard_State.Released := Old_State.Released or (Changed and not New_State_Buttons);
      end Set_State;

      function Raw_Pointer_Motion return Boolean is
      begin
         return Raw_Pointer_Motion_Flag;
      end Raw_Pointer_Motion;

      procedure Set_Raw_Pointer_Motion (Enable : Boolean) is
      begin
         Raw_Pointer_Motion_Flag := Enable;
      end Set_Raw_Pointer_Motion;

      procedure Update_Animated_Cursor is
         use all type AWT.Inputs.Pointer_Mode;
      begin
         if Cursor_Images > 1 and Pointer_State.Mode = Visible then
            if Orka.OS.Monotonic_Clock > Next_Time then
               Set_Pointer_Cursor (Cursor_Name);
            end if;
         end if;
      end Update_Animated_Cursor;

      procedure Set_EGL_Data
        (Context : EGL.Objects.Contexts.Context;
         Config  : EGL.Objects.Configs.Config;
         sRGB    : Boolean) is
      begin
         EGL_Context := Context;
         EGL_Config  := Config;
         EGL_sRGB    := sRGB;
      end Set_EGL_Data;

      procedure Swap_Buffers is
      begin
         EGL_Surface.Swap_Buffers;
      end Swap_Buffers;

      procedure Ask_Feedback
        (Feedback : in out Feedback_With_Frame) is
      begin
         if Feedback.Has_Proxy then
            raise Internal_Error with
              "Wayland: Feedback object still in use";
         end if;

         Global.Presentation.Feedback (Surface, Feedback);
         if not Feedback.Has_Proxy then
            raise Internal_Error with "Wayland: Failed to get feedback";
         end if;

         Feedback_Events.Subscribe (Feedback);
      end Ask_Feedback;

      procedure Ask_Frame is
      begin
         if Frame.Has_Proxy then
            Surface.Frame (Frame);

            if not Frame.Has_Proxy then
               raise Internal_Error with "Wayland: Failed to get frame callback";
            end if;

            Frame_Callback_Events.Subscribe (Frame);
         end if;
      end Ask_Frame;

   end Window_Object;

   ----------------------------------------------------------------------------

   procedure Await_Configure (Object : in out Wayland_Window) is
   begin
      Global.Display.Roundtrip;

      declare
         Interval : constant Duration := 0.001;
         Timeout  : constant Duration := 2.0;

         Clock_Timeout : constant Duration := Orka.OS.Monotonic_Clock + Timeout;
      begin
         while Object.Window.Is_Configuring loop
            AWT.Process_Events (Interval);

            if Orka.OS.Monotonic_Clock > Clock_Timeout then
               raise Internal_Error with "Wayland: Failed to receive configure event";
            end if;
         end loop;
      end;
   end Await_Configure;

   overriding
   procedure Create_Window
     (Object                        : aliased in out Wayland_Window;
      ID, Title                     : String;
      Width, Height                 : Positive;
      Visible, Resizable, Decorated : Boolean := True;
      Transparent                   : Boolean := False) is
   begin
      Object.Window.Create_Window
        (ID, Title, Width, Height, Visible, Resizable, Decorated, Transparent);

      Object.Await_Configure;
   end Create_Window;

   overriding
   procedure Finalize (Object : in out Wayland_Window) is
   begin
      Object.Window.Finalize;
   end Finalize;

   overriding
   procedure Set_Application_ID (Object : in out Wayland_Window; ID : String) is
   begin
      Object.Window.Set_Application_ID (ID);
   end Set_Application_ID;

   overriding
   procedure Set_Title (Object : in out Wayland_Window; Title : String) is
   begin
      Object.Window.Set_Title (Title);
   end Set_Title;

   overriding
   procedure Set_Size (Object : in out Wayland_Window; Width, Height : Positive) is
   begin
      Object.Window.Set_Size (Width, Height);
   end Set_Size;

   overriding
   procedure Set_Size_Limits
     (Object : in out Wayland_Window;
      Min_Width, Min_Height, Max_Width, Max_Height : Natural) is
   begin
      Object.Window.Set_Size_Limits (Min_Width, Min_Height, Max_Width, Max_Height);
   end Set_Size_Limits;

   overriding
   procedure Set_Size_Mode (Object : in out Wayland_Window; Mode : AWT.Windows.Size_Mode) is
   begin
      Object.Window.Set_Size_Mode (Mode);
   end Set_Size_Mode;

   overriding
   procedure Set_Size_Mode
     (Object  : in out Wayland_Window;
      Mode    : AWT.Windows.Size_Mode;
      Monitor : AWT.Monitors.Monitor'Class) is
   begin
      Object.Window.Set_Size_Mode (Mode, Monitor);
   end Set_Size_Mode;

   overriding
   procedure Set_Framebuffer_Scale (Object : in out Wayland_Window; Scale : Positive) is
   begin
      Object.Window.Set_Framebuffer_Scale (Scale);
   end Set_Framebuffer_Scale;

   overriding
   procedure Set_Raw_Pointer_Motion (Object : in out Wayland_Window; Enable : Boolean) is
   begin
      Object.Window.Set_Raw_Pointer_Motion (Enable);
   end Set_Raw_Pointer_Motion;

   overriding
   procedure Set_Margin
     (Object : in out Wayland_Window;
      Margin : Natural) is
   begin
      Object.Window.Set_Margin (Margin);
   end Set_Margin;

   overriding
   procedure Set_Visible (Object : in out Wayland_Window; Visible : Boolean) is
   begin
      Object.Window.Set_Visible (Visible);

      if Visible then
         Object.Await_Configure;
      end if;
   end Set_Visible;

   overriding
   procedure Set_Pointer_Cursor
     (Object : in out Wayland_Window;
      Cursor : AWT.Inputs.Cursors.Pointer_Cursor) is
   begin
      Object.Window.Set_Pointer_Cursor (Cursor);
   end Set_Pointer_Cursor;

   overriding
   procedure Set_Pointer_Mode
     (Object : in out Wayland_Window;
      Mode   : AWT.Inputs.Pointer_Mode) is
   begin
      Object.Window.Set_Pointer_Mode (Mode);
   end Set_Pointer_Mode;

   overriding
   function Raw_Pointer_Motion (Object : Wayland_Window) return Boolean is
   begin
      return Object.Window.Raw_Pointer_Motion;
   end Raw_Pointer_Motion;

   overriding
   function State (Object : Wayland_Window) return AWT.Windows.Window_State is
   begin
      return Object.Window.State;
   end State;

   overriding
   function State (Object : Wayland_Window) return AWT.Windows.Framebuffer_State is
   begin
      return Object.Window.State;
   end State;

   overriding
   function State (Object : in out Wayland_Window) return AWT.Inputs.Pointer_State is
   begin
      return Result : AWT.Inputs.Pointer_State do
         Object.Window.State (Result);
      end return;
   end State;

   overriding
   function State (Object : in out Wayland_Window) return AWT.Inputs.Keyboard_State is
   begin
      return Result : AWT.Inputs.Keyboard_State do
         Object.Window.State (Result);
      end return;
   end State;

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

   ----------------------------------------------------------------------------

   procedure Set_State
     (Object : in out Wayland_Window;
      State  : AWT.Inputs.Pointer_State) is
   begin
      Object.Window.Set_State (State);
   end Set_State;

   procedure Set_State
     (Object : in out Wayland_Window;
      State  : AWT.Inputs.Keyboard_State) is
   begin
      Object.Window.Set_State (State);
   end Set_State;

   procedure Restore_Cursor (Object : in out Wayland_Window) is
   begin
      Object.Window.Restore_Cursor;
   end Restore_Cursor;

   procedure Reset_Input_State (Object : in out Wayland_Window) is
   begin
      Object.Window.Reset_Input_State;
   end Reset_Input_State;

   procedure Update_Animated_Cursor (Object : in out Wayland_Window) is
   begin
      Object.Window.Update_Animated_Cursor;
   end Update_Animated_Cursor;

   procedure Set_EGL_Data
     (Object  : in out Wayland_Window;
      Context : EGL.Objects.Contexts.Context;
      Config  : EGL.Objects.Configs.Config;
      sRGB    : Boolean) is
   begin
      Object.Window.Set_EGL_Data (Context, Config, sRGB);
   end Set_EGL_Data;

   procedure Make_Current
     (Object  : in out Wayland_Window;
      Context : Standard.EGL.Objects.Contexts.Context) is
   begin
      Object.Window.Make_Current (Context);
   end Make_Current;

   overriding
   procedure Swap_Buffers (Object : in out Wayland_Window) is
      Time_To_Swap : Duration;
      Do_Swap      : Boolean;
   begin
      Object.Frame_Handler.Before_Swap_Buffers (Time_To_Swap, Do_Swap);

      delay Time_To_Swap - Orka.OS.Monotonic_Clock;

      if Do_Swap then
         Object.Window.Swap_Buffers;
      end if;

      Object.Frame_Handler.After_Swap_Buffers (Do_Swap);
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

   ----------------------------------------------------------------------------

   function Ceiling (Left, Right : Duration) return Duration is
     (Integer (Duration'(Left / Right) + 0.5) * Right);

   protected body Frame_Handler_With_Window is
      procedure Before_Swap_Buffers (Time_To_Swap : out Duration; Do_Swap : out Boolean) is
         Clock : constant Duration := Orka.OS.Monotonic_Clock;

         Too_Many_Frames : constant Boolean := Pending >= Frames'Length;
      begin
         Do_Swap := Has_Buffer and not Too_Many_Frames;

         if Do_Swap then
            if not Global.Presentation.Has_Proxy then
               Window.Window.Ask_Frame;
            end if;

            if Latest_Stop > 0.0 and Default_Refresh > 0.0 then
               Time_To_Swap := Latest_Stop + Ceiling (Clock - Latest_Stop, Default_Refresh);
            else
               Time_To_Swap := Clock + Default_Frame_Time;
            end if;
         elsif Has_Buffer then
            Time_To_Swap := Clock + Default_Frame_Time;
         else
            Time_To_Swap := Clock + No_Buffer_Frame_Time;
         end if;
      end Before_Swap_Buffers;

      procedure After_Swap_Buffers (Did_Swap : Boolean) is
      begin
         if Did_Swap then
            if Global.Presentation.Has_Proxy and Pending < Frames'Length then
               for Data of Frames loop
                  if not Data.Feedback.Has_Proxy then
                     Window.Window.Ask_Feedback (Data.Feedback);
                     Pending := Pending + 1;
                     exit;
                  end if;
               end loop;
            end if;

            if Resize_Width > 0 and Resize_Height > 0 then
               Window.Window.Do_Resize (Resize_Width, Resize_Height, Resize_Margin, Resize_Serial);

               Resize_Width  := 0;
               Resize_Height := 0;
               Resize_Serial := 0;
            end if;
         end if;

         if not Has_Buffer then
            Window.Window.Detach_Buffer;
         end if;
      end After_Swap_Buffers;

      procedure Set_Size (Width, Height : Positive; Margin : Natural; Serial : Unsigned_32) is
      begin
         Resize_Width  := Width;
         Resize_Height := Height;
         Resize_Margin := Margin;
         Resize_Serial := Serial;
      end Set_Size;

      procedure Set_Has_Buffer (Value : Boolean) is
      begin
         Has_Buffer := Value;
      end Set_Has_Buffer;

      procedure On_Frame_Presented (Timestamp, Refresh : Duration) is
      begin
         Latest_Stop     := Duration'Max (Latest_Stop, Timestamp);
         Default_Refresh := Refresh;

         Pending := Pending - 1;
      end On_Frame_Presented;

      procedure On_Frame_Discarded is
      begin
         Pending := Pending - 1;
      end On_Frame_Discarded;

      procedure On_Frame_Output (Refresh : Duration) is
      begin
         Default_Refresh := Refresh;
      end On_Frame_Output;

      procedure On_Frame_Done (Timestamp : Duration) is
      begin
         Latest_Stop     := Duration'Max (Latest_Stop, Timestamp);
         Default_Refresh := Default_Frame_Time;
      end On_Frame_Done;

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
         for Frame of Result loop
            Frame.Window := Window;
         end loop;
      end return;
   end Make_Frames;

end AWT.Wayland.Windows;
