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

with Ada.Containers.Vectors;
with Ada.Unchecked_Conversion;

with Wayland.Enums.Pointer_Constraints_Unstable_V1;

with AWT.Gamepads;
with AWT.OS;
with AWT.Windows;

with Orka.OS;

package body AWT.Registry is

   use Standard.Wayland;

   overriding
   function Is_Connected (Object : Monitor_Device) return Boolean is
     (Object.Output.Has_Proxy);

   overriding
   function ID (Object : Monitor_Device) return Natural is
     (Natural (Object.Output.Get_ID));

   overriding
   function State (Object : Monitor_Device) return AWT.Monitors.Monitor_State is
     (Object.Current_State);

   ----------------------------------------------------------------------------

   procedure Output_Geometry
     (Output          : in out WP.Client.Output'Class;
      X, Y            : Integer;
      Physical_Width  : Integer;
      Physical_Height : Integer;
      Subpixel        : WE.Client.Output_Subpixel;
      Make            : String;
      Model           : String;
      Transform       : WE.Client.Output_Transform)
   is
      Monitor : constant not null access Monitor_Device :=
        Output_With_Monitor (Output).Monitor;
   begin
      Monitor.Pending_State.X := X;
      Monitor.Pending_State.Y := Y;

      Monitor.Physical_Width  := Physical_Width;
      Monitor.Physical_Height := Physical_Height;

      Monitor.Pending_State.Name := +(Make & " " & Model);
      Monitor.Subpixel  := Subpixel;
      Monitor.Transform := Transform;
   end Output_Geometry;

   procedure Output_Mode
     (Output  : in out WP.Client.Output'Class;
      Flags   : WE.Client.Output_Mode;
      Width   : Integer;
      Height  : Integer;
      Refresh : Integer)
   is
      Monitor : constant not null access Monitor_Device :=
        Output_With_Monitor (Output).Monitor;

      use all type WE.Client.Output_Mode;
   begin
      if Flags.Current then
         Monitor.Pending_State.Width   := Width;
         Monitor.Pending_State.Height  := Height;
         Monitor.Pending_State.Refresh :=
           (if Refresh > 0 then 1.0e3 / Duration (Refresh) else 0.0);
      end if;
   end Output_Mode;

   procedure Output_Done (Output  : in out WP.Client.Output'Class) is
      Monitor : constant not null access Monitor_Device :=
        Output_With_Monitor (Output).Monitor;

      use type AWT.Monitors.Monitor_Event_Listener_Ptr;
   begin
      Monitor.Current_State := Monitor.Pending_State;

      if not Monitor.Initialized then
         Monitor.Initialized := True;

         if Monitor_Listener /= null then
            Monitor_Listener.On_Connect
              (Monitor => AWT.Monitors.Monitor'Class (Monitor.all)'Access);
         end if;
      end if;
   end Output_Done;

   procedure Output_Scale
     (Output : in out WP.Client.Output'Class;
      Factor : Integer)
   is
      Monitor : constant not null access Monitor_Device :=
        Output_With_Monitor (Output).Monitor;
   begin
      Monitor.Pending_State.Scale := Factor;
   end Output_Scale;

   ----------------------------------------------------------------------------

   procedure Presentation_Clock
     (Presentation : in out WP.Presentation_Time.Presentation'Class;
      Id           : Unsigned_32) is
   begin
      if Id /= 1 then
         raise Constraint_Error with "Unknown presentation clock " & Id'Image;
      end if;
   end Presentation_Clock;

   ----------------------------------------------------------------------------

   procedure Pointer_Enter
     (Pointer   : in out WP.Client.Pointer'Class;
      Serial    : Unsigned_32;
      Surface   : WP.Client.Surface;
      Surface_X : Fixed;
      Surface_Y : Fixed)
   is
      Seat : constant not null access Seat_Devices :=
        Pointer_With_Seat (Pointer).Seat;

      Window : constant not null access AWT.Wayland.Windows.Wayland_Window :=
        Surface_Data.Get_Data (Surface).Window;
   begin
      Seat.Window := Window;

      Seat.Pointer_State   := (Focused => True, others => <>);
      Seat.Scroll_Discrete := False;
      Seat.Scrolling       := (others => False);

      Seat.Pointer_Enter_Serial := Serial;

      Window.Restore_Cursor;
   end Pointer_Enter;

   procedure Pointer_Leave
     (Pointer : in out WP.Client.Pointer'Class;
      Serial  : Unsigned_32;
      Surface : WP.Client.Surface)
   is
      Seat : constant not null access Seat_Devices :=
        Pointer_With_Seat (Pointer).Seat;

      Window : constant not null access AWT.Wayland.Windows.Wayland_Window :=
        Surface_Data.Get_Data (Surface).Window;
   begin
      if Seat.Window /= Window then
         raise Program_Error;
      end if;

      Seat.Pointer_State.Focused   := False;
      Seat.Pointer_State.Scrolling := False;

      Seat.Pointer_Enter_Serial := 0;
   end Pointer_Leave;

   procedure Pointer_Motion
     (Pointer : in out WP.Client.Pointer'Class;
      Time      : Unsigned_32;
      Surface_X : Fixed;
      Surface_Y : Fixed)
   is
      Seat : constant not null access Seat_Devices :=
        Pointer_With_Seat (Pointer).Seat;

      use all type AWT.Inputs.Dimension;
   begin
      Seat.Pointer_State.Position :=
        (X => AWT.Inputs.Fixed (Surface_X),
         Y => AWT.Inputs.Fixed (Surface_Y));
   end Pointer_Motion;

   procedure Pointer_Button
     (Pointer : in out WP.Client.Pointer'Class;
      Serial  : Unsigned_32;
      Time    : Unsigned_32;
      Button  : Unsigned_32;
      State   : WE.Client.Pointer_Button_State)
   is
      Seat : constant not null access Seat_Devices :=
        Pointer_With_Seat (Pointer).Seat;

      use all type WE.Client.Pointer_Button_State;
      use all type AWT.Inputs.Button_State;
   begin
      Seat.Pointer_State.Buttons (OS.Code_To_Button (Button)) :=
        (case State is
           when Released => Released,
           when Pressed  => Pressed);
   end Pointer_Button;

   procedure Pointer_Frame
     (Pointer : in out WP.Client.Pointer'Class)
   is
      Seat : constant not null access Seat_Devices :=
        Pointer_With_Seat (Pointer).Seat;
   begin
      Seat.Window.Set_State (Seat.Pointer_State);

      Seat.Scroll_Discrete := False;
      Seat.Pointer_State.Scroll   := (others => 0.0);
      Seat.Pointer_State.Relative := (others => 0.0);

      if not Seat.Pointer_State.Focused then
         Seat.Window := null;
      end if;
   end Pointer_Frame;

   procedure Pointer_Axis_Source
     (Pointer     : in out WP.Client.Pointer'Class;
      Axis_Source : WE.Client.Pointer_Axis_Source) is null;

   procedure Pointer_Axis_Stop
     (Pointer : in out WP.Client.Pointer'Class;
      Time    : Unsigned_32;
      Axis    : WE.Client.Pointer_Axis)
   is
      Seat : constant not null access Seat_Devices :=
        Pointer_With_Seat (Pointer).Seat;

      use all type WE.Client.Pointer_Axis;
      use all type AWT.Inputs.Dimension;

      Dimension : constant AWT.Inputs.Dimension :=
        (case Axis is
           when Horizontal_Scroll => X,
           when Vertical_Scroll   => Y);
   begin
      Seat.Scrolling (Dimension)   := False;
      Seat.Pointer_State.Scrolling := Seat.Scrolling (X) or Seat.Scrolling (Y);
   end Pointer_Axis_Stop;

   procedure Pointer_Axis
     (Pointer : in out WP.Client.Pointer'Class;
      Time    : Unsigned_32;
      Axis    : WE.Client.Pointer_Axis;
      Value   : Fixed)
   is
      Seat : constant not null access Seat_Devices :=
        Pointer_With_Seat (Pointer).Seat;

      use all type WE.Client.Pointer_Axis;
      use all type AWT.Inputs.Dimension;
      use type AWT.Inputs.Fixed;

      Dimension : constant AWT.Inputs.Dimension :=
        (case Axis is
           when Horizontal_Scroll => X,
           when Vertical_Scroll   => Y);

      Scroll : AWT.Inputs.Fixed renames Seat.Pointer_State.Scroll (Dimension);
   begin
      if not Seat.Scroll_Discrete then
         Scroll := Scroll + AWT.Inputs.Fixed (Value);
      end if;
      Seat.Pointer_State.Scrolling := True;
      Seat.Scrolling (Dimension)   := True;
   end Pointer_Axis;

   procedure Pointer_Axis_Discrete
     (Pointer  : in out WP.Client.Pointer'Class;
      Axis     : WE.Client.Pointer_Axis;
      Discrete : Integer)
   is
      Seat : constant not null access Seat_Devices :=
        Pointer_With_Seat (Pointer).Seat;

      use all type WE.Client.Pointer_Axis;
      use all type AWT.Inputs.Dimension;
      use type AWT.Inputs.Fixed;

      Dimension : constant AWT.Inputs.Dimension :=
        (case Axis is
           when Horizontal_Scroll => X,
           when Vertical_Scroll   => Y);

      Scroll : AWT.Inputs.Fixed renames Seat.Pointer_State.Scroll (Dimension);
   begin
      Scroll := Scroll + AWT.Inputs.Fixed (Discrete);
      Seat.Scroll_Discrete := True;
   end Pointer_Axis_Discrete;

   procedure Relative_Pointer_Relative_Motion
     (Pointer    : in out WP.Relative_Pointer_Unstable_V1.Relative_Pointer_V1'Class;
      Timestamp  : Duration;
      Dx         : Fixed;
      Dy         : Fixed;
      Dx_Unaccel : Fixed;
      Dy_Unaccel : Fixed)
   is
      Seat : constant not null access Seat_Devices :=
        Relative_Pointer_With_Seat (Pointer).Seat;

      use all type AWT.Inputs.Dimension;
      use type AWT.Inputs.Fixed;

      Relative_X : AWT.Inputs.Fixed renames Seat.Pointer_State.Relative (X);
      Relative_Y : AWT.Inputs.Fixed renames Seat.Pointer_State.Relative (Y);

      Raw_Motion : constant Boolean := Seat.Window.Raw_Pointer_Motion;
   begin
      Relative_X := Relative_X + AWT.Inputs.Fixed (if Raw_Motion then Dx_Unaccel else Dx);
      Relative_Y := Relative_Y + AWT.Inputs.Fixed (if Raw_Motion then Dy_Unaccel else Dy);
   end Relative_Pointer_Relative_Motion;

   ----------------------------------------------------------------------------

   procedure Keyboard_Keymap
     (Keyboard : in out WP.Client.Keyboard'Class;
      Format   : WE.Client.Keyboard_Keymap_Format;
      FD       : Standard.Wayland.File_Descriptor;
      Size     : Unsigned_32)
   is
      File : AWT.OS.File (FD);

--      use all type WE.Client.Keyboard_Keymap_Format;
   begin
      --  TODO Use libxkbcommon if Format = Xkb_V1
      File.Close;
   end Keyboard_Keymap;

   procedure Keyboard_Enter
     (Keyboard : in out WP.Client.Keyboard'Class;
      Serial   : Unsigned_32;
      Surface  : WP.Client.Surface;
      Keys     : Standard.Wayland.Unsigned_32_Array)
   is
      Seat : constant not null access Seat_Devices :=
        Keyboard_With_Seat (Keyboard).Seat;

      Window : constant not null access AWT.Wayland.Windows.Wayland_Window :=
        Surface_Data.Get_Data (Surface).Window;

      use all type AWT.Inputs.Button_State;
   begin
      Seat.Keyboard_Window := Window;
      Seat.Keyboard_Enter_Serial := Serial;

      Seat.Keyboard_State.Focused := True;
      Seat.Keyboard_State.Buttons := (others => Released);
      for Key of Keys loop
         Seat.Keyboard_State.Buttons (OS.Code_To_Button (Key)) := Pressed;
      end loop;

      Seat.Keyboard_Window.Set_State (Seat.Keyboard_State);
   end Keyboard_Enter;

   procedure Keyboard_Leave
     (Keyboard : in out WP.Client.Keyboard'Class;
      Serial   : Unsigned_32;
      Surface  : WP.Client.Surface)
   is
      Seat : constant not null access Seat_Devices :=
        Keyboard_With_Seat (Keyboard).Seat;

      Window : constant not null access AWT.Wayland.Windows.Wayland_Window :=
        Surface_Data.Get_Data (Surface).Window;

      use all type AWT.Inputs.Button_State;
   begin
      if Seat.Keyboard_Window /= Window then
         raise Program_Error;
      end if;

      Seat.Keyboard_State.Buttons := (others => Released);
      Seat.Keyboard_State.Focused := False;

      Seat.Keyboard_Window.Set_State (Seat.Keyboard_State);

      Seat.Keyboard_Enter_Serial := 0;
      Seat.Keyboard_Window := null;
   end Keyboard_Leave;

   procedure Keyboard_Key
     (Keyboard : in out WP.Client.Keyboard'Class;
      Serial   : Unsigned_32;
      Time     : Unsigned_32;
      Key      : Unsigned_32;
      State    : WE.Client.Keyboard_Key_State)
   is
      Seat : constant not null access Seat_Devices :=
        Keyboard_With_Seat (Keyboard).Seat;

      use all type WE.Client.Keyboard_Key_State;
      use all type AWT.Inputs.Button_State;

      Button : AWT.Inputs.Keyboard_Button renames OS.Code_To_Button (Key);
   begin
      Seat.Keyboard_State.Buttons (Button) :=
        (case State is
           when Pressed  => Pressed,
           when Released => Released);

      if State = Pressed then
         Seat.Keyboard_State.Last_Pressed := Button;
      end if;

      Seat.Keyboard_Window.Set_State (Seat.Keyboard_State);
   end Keyboard_Key;

   procedure Keyboard_Modifiers
     (Keyboard       : in out WP.Client.Keyboard'Class;
      Serial         : Unsigned_32;
      Mods_Depressed : Unsigned_32;
      Mods_Latched   : Unsigned_32;
      Mods_Locked    : Unsigned_32;
      Group          : Unsigned_32)
   is
      function Convert is new Ada.Unchecked_Conversion
        (Unsigned_32, AWT.Inputs.Keyboard_Modifiers);

      Seat : constant not null access Seat_Devices :=
        Keyboard_With_Seat (Keyboard).Seat;

      Modifiers : AWT.Inputs.Keyboard_Modifiers renames
        Convert (Mods_Depressed or Mods_Latched or Mods_Locked);
   begin
      Seat.Keyboard_State.Modifiers := Modifiers;

      --  Check needed for Weston compositor
      if Seat.Keyboard_Window /= null then
         Seat.Keyboard_Window.Set_State (Seat.Keyboard_State);
      end if;
   end Keyboard_Modifiers;

   procedure Keyboard_Repeat_Info
     (Keyboard : in out WP.Client.Keyboard'Class;
      Rate     : Integer;
      Delay_V  : Integer)
   is
      Seat : constant not null access Seat_Devices :=
        Keyboard_With_Seat (Keyboard).Seat;
   begin
      Seat.Keyboard_State.Repeat_Rate  := Natural (Rate);
      Seat.Keyboard_State.Repeat_Delay := Duration (Delay_V) / 1e3;
   end Keyboard_Repeat_Info;

   ----------------------------------------------------------------------------

   package Output_Events is new WP.Client.Output_Events
     (Geometry => Output_Geometry,
      Mode     => Output_Mode,
      Done     => Output_Done,
      Scale    => Output_Scale);

   package Presentation_Events is new WP.Presentation_Time.Presentation_Events
     (Clock => Presentation_Clock);

   package Pointer_Events is new WP.Client.Pointer_Events
     (Pointer_Enter           => Pointer_Enter,
      Pointer_Leave           => Pointer_Leave,
      Pointer_Motion          => Pointer_Motion,
      Pointer_Button          => Pointer_Button,
      Pointer_Scroll          => Pointer_Axis,
      Pointer_Frame           => Pointer_Frame,
      Pointer_Scroll_Source   => Pointer_Axis_Source,
      Pointer_Scroll_Stop     => Pointer_Axis_Stop,
      Pointer_Scroll_Discrete => Pointer_Axis_Discrete);

   package Relative_Pointer_Events is new RP.Relative_Pointer_V1_Events
     (Relative_Motion => Relative_Pointer_Relative_Motion);

   package Keyboard_Events is new WP.Client.Keyboard_Events
     (Keymap      => Keyboard_Keymap,
      Enter       => Keyboard_Enter,
      Leave       => Keyboard_Leave,
      Key         => Keyboard_Key,
      Modifiers   => Keyboard_Modifiers,
      Repeat_Info => Keyboard_Repeat_Info);

   ----------------------------------------------------------------------------

   procedure Get_Seat_Capabilities
     (Element      : in out AWT.Registry.Seat_Devices;
      Capabilities : WE.Client.Seat_Capability) is
   begin
      if Capabilities.Pointer and not Element.Pointer.Has_Proxy then
         Element.Seat.Get_Pointer (Element.Pointer);

         if not Element.Pointer.Has_Proxy then
            raise Internal_Error with "Wayland: Failed to get pointer";
         end if;

         Pointer_Events.Subscribe (Element.Pointer);

         if not Global.Relative_Pointer_Manager.Has_Proxy then
            raise Internal_Error with "Wayland: Expected to have relative pointer manager";
         end if;

         Global.Relative_Pointer_Manager.Get_Relative_Pointer
           (Element.Pointer, Element.Relative_Pointer);

         if not Element.Relative_Pointer.Has_Proxy then
            raise Internal_Error with "Wayland: Failed to get relative pointer";
         end if;

         Relative_Pointer_Events.Subscribe (Element.Relative_Pointer);
      end if;

      if Capabilities.Keyboard and not Element.Keyboard.Has_Proxy then
         Element.Seat.Get_Keyboard (Element.Keyboard);

         if not Element.Keyboard.Has_Proxy then
            raise Internal_Error with "Wayland: Failed to get keyboard";
         end if;

         Keyboard_Events.Subscribe (Element.Keyboard);
      end if;

      if Capabilities.Touch and not Element.Touch.Has_Proxy then
         Element.Seat.Get_Touch (Element.Touch);
      end if;
   end Get_Seat_Capabilities;

   procedure Release_Seat_Capabilities
     (Element      : in out Seat_Devices;
      Capabilities : WE.Client.Seat_Capability) is
   begin
      if not Capabilities.Pointer and Element.Pointer.Has_Proxy then
         if Element.Relative_Pointer.Has_Proxy then
            Element.Relative_Pointer.Destroy;
         end if;

         Element.Pointer.Release;
      end if;

      if not Capabilities.Keyboard and Element.Keyboard.Has_Proxy then
         Element.Keyboard.Release;
      end if;

      if not Capabilities.Touch and Element.Touch.Has_Proxy then
         Element.Touch.Release;
      end if;
   end Release_Seat_Capabilities;

   procedure Seat_Capabilities
     (Seat         : in out WP.Client.Seat'Class;
      Capabilities : WE.Client.Seat_Capability)
   is
      Object : constant not null access Seat_Devices :=
        Seat_With_Seat (Seat).Seat;
   begin
      Object.Capabilities := Capabilities;
      Release_Seat_Capabilities (Object.all, Capabilities);
      Get_Seat_Capabilities (Object.all, Capabilities);
   end Seat_Capabilities;

   package Seat_Events is new WP.Client.Seat_Events
     (Seat_Capabilities => Seat_Capabilities);

   ----------------------------------------------------------------------------

   --  Clipboard
   procedure Data_Offer_Offer
     (Data_Offer  : in out WP.Client.Data_Offer'Class;
      Mime_Type   : String) is
   begin
      if Mime_Type = UTF_8_Mime_Type then
         Global.Seat.Clipboard_Mime_Type_Valid := True;
      elsif Mime_Type = URIs_Mime_Type then
         Global.Seat.Drag_Drop_Mime_Type_Valid := True;
      end if;
   end Data_Offer_Offer;

   --  Drag and drop
   procedure Data_Offer_Source_Actions
     (Data_Offer : in out WP.Client.Data_Offer'Class;
      Actions    : WE.Client.Data_Device_Manager_Dnd_Action) is
   begin
      Global.Seat.Supported_Drag_Drop_Actions :=
        (Copy => Actions.Copy,
         Move => Actions.Move,
         Ask  => Actions.Ask);
   end Data_Offer_Source_Actions;

   --  Drag and drop
   procedure Data_Offer_Action
     (Data_Offer : in out WP.Client.Data_Offer'Class;
      Actions    : WE.Client.Data_Device_Manager_Dnd_Action)
   is
      use all type AWT.Inputs.Action_Kind;
   begin
      if Actions.Copy then
         Global.Seat.Valid_Drag_Drop_Action := Copy;
      elsif Actions.Move then
         Global.Seat.Valid_Drag_Drop_Action := Move;
      elsif Actions.Ask then
         Global.Seat.Valid_Drag_Drop_Action := Ask;
      else
         Global.Seat.Valid_Drag_Drop_Action := None;
      end if;
   end Data_Offer_Action;

   package Data_Offer_Events is new WP.Client.Data_Offer_Events
     (Offer          => Data_Offer_Offer,
      Source_Actions => Data_Offer_Source_Actions,
      Action         => Data_Offer_Action);

   --  Clipboard
   procedure Data_Device_Data_Offer
     (Data_Device : in out WP.Client.Data_Device'Class;
      Data_Offer  : in out WP.Client.Data_Offer)
   is
      use all type AWT.Inputs.Action_Kind;
   begin
      if Global.Seat.Data_Offer.Has_Proxy then
         Global.Seat.Data_Offer.Destroy;
      end if;

      Global.Seat.Clipboard_Mime_Type_Valid := False;
      Global.Seat.Drag_Drop_Mime_Type_Valid := False;

      Global.Seat.Supported_Drag_Drop_Actions := (others => False);
      Global.Seat.Valid_Drag_Drop_Action      := None;

      Data_Offer.Move (To => Global.Seat.Data_Offer);
      Data_Offer_Events.Subscribe (Global.Seat.Data_Offer);
   end Data_Device_Data_Offer;

   --  Drag and drop
   procedure Data_Device_Enter
     (Data_Device : in out WP.Client.Data_Device'Class;
      Serial      : Unsigned_32;
      Surface     : WP.Client.Surface;
      X, Y        : Fixed;
      Data_Offer  : in out WP.Client.Data_Offer)
   is
      Window : constant not null access AWT.Wayland.Windows.Wayland_Window :=
        Surface_Data.Get_Data (Surface).Window;

      use WP.Client;
   begin
      Global.Seat.Drag_Drop_Window := Window;

      pragma Assert (Global.Seat.Data_Offer = Data_Offer);

      if Global.Seat.Drag_Drop_Mime_Type_Valid then
         Data_Offer.Do_Accept (Serial, URIs_Mime_Type);
      end if;

      AWT.Windows.Window'Class (Global.Seat.Drag_Drop_Window.all).On_Drag
        (AWT.Inputs.Fixed (X), AWT.Inputs.Fixed (Y));
   end Data_Device_Enter;

   --  Drag and drop
   procedure Data_Device_Motion
     (Data_Device : in out WP.Client.Data_Device'Class;
      Time        : Unsigned_32;
      X, Y        : Fixed) is
   begin
      if Global.Seat.Drag_Drop_Mime_Type_Valid then
         AWT.Windows.Window'Class (Global.Seat.Drag_Drop_Window.all).On_Drag
           (AWT.Inputs.Fixed (X), AWT.Inputs.Fixed (Y));
      end if;
   end Data_Device_Motion;

   --  Drag and drop
   procedure Data_Device_Leave
     (Data_Device : in out WP.Client.Data_Device'Class)
   is
      use all type AWT.Inputs.Action_Kind;
   begin
      if Global.Seat.Drag_Drop_Window = null then
         raise Program_Error;
      end if;

      Global.Seat.Drag_Drop_Window := null;
   end Data_Device_Leave;

   --  Drag and drop
   procedure Data_Device_Drop
     (Data_Device : in out WP.Client.Data_Device'Class) is
   begin
      if Global.Seat.Drag_Drop_Mime_Type_Valid then
         AWT.Windows.Window'Class (Global.Seat.Drag_Drop_Window.all).On_Drop;
      end if;
   end Data_Device_Drop;

   package Data_Device_Events is new WP.Client.Data_Device_Events
     (Data_Offer => Data_Device_Data_Offer,
      Enter      => Data_Device_Enter,
      Motion     => Data_Device_Motion,
      Leave      => Data_Device_Leave,
      Drop       => Data_Device_Drop);

   procedure Initialize_Clipboard_And_Drag_And_Drop is
   begin
      if Global.Seat.Seat.Has_Proxy and Global.Data_Device_Manager.Has_Proxy then
         Global.Data_Device_Manager.Get_Data_Device (Global.Seat.Seat, Global.Seat.Data_Device);

         if not Global.Seat.Data_Device.Has_Proxy then
            raise AWT.Initialization_Error with "Wayland: Failed to get data device";
         end if;

         Data_Device_Events.Subscribe (Global.Seat.Data_Device);
      end if;
   end Initialize_Clipboard_And_Drag_And_Drop;

   ----------------------------------------------------------------------------

   procedure Base_Ping
     (Xdg_Wm_Base : in out WP.Xdg_Shell.Xdg_Wm_Base'Class;
      Serial      : Unsigned_32) is
   begin
      Xdg_Wm_Base.Pong (Serial);
   end Base_Ping;

   package Base_Events is new WP.Xdg_Shell.Xdg_Wm_Base_Events
     (Ping => Base_Ping);

   type ID_Pair is record
      Global_ID, Object_ID : Unsigned_32;
   end record;

   package ID_Pair_Vectors is new Ada.Containers.Vectors (Positive, ID_Pair);

   Binded_Identifiers : ID_Pair_Vectors.Vector;

   procedure Add_Pair (Pair : ID_Pair)
     with Pre => not Binded_Identifiers.Contains (Pair);

   procedure Add_Pair (Pair : ID_Pair) is
   begin
      Binded_Identifiers.Append (Pair);
   end Add_Pair;

   procedure Remove_First_Pair
     (Global_ID : Unsigned_32;
      Result : out ID_Pair;
      Found  : out Boolean)
   is
      Result_Cursor : ID_Pair_Vectors.Cursor := ID_Pair_Vectors.No_Element;

      procedure Is_Match (Position : ID_Pair_Vectors.Cursor) is
      begin
         if Global_ID = Binded_Identifiers (Position).Global_ID then
            Result_Cursor := Position;
         end if;
      end Is_Match;

      use type ID_Pair_Vectors.Cursor;
   begin
      Binded_Identifiers.Iterate (Is_Match'Access);

      Found := Result_Cursor /= ID_Pair_Vectors.No_Element;

      if Found then
         Result := Binded_Identifiers (Result_Cursor);
         Binded_Identifiers.Delete (Result_Cursor);
      end if;
   end Remove_First_Pair;

   procedure Global_Registry_Handler
     (Registry : in out WP.Client.Registry'Class;
      Id       : Unsigned_32;
      Name     : String;
      Version  : Unsigned_32)
   is
      use type AWT.Wayland.On_Available_Interface;
   begin
      if Name = WP.Client.Compositor_Interface.Name then
         Global.Compositor.Bind (Registry, Id, Unsigned_32'Min (Version, 4));

         if not Global.Compositor.Has_Proxy then
            raise Initialization_Error with "Wayland: Failed to get compositor";
         end if;
         Add_Pair ((Id, Global.Compositor.Get_ID));
      elsif Name = WP.Client.Shm_Interface.Name then
         Global.Shm.Bind (Registry, Id, Unsigned_32'Min (Version, 1));

         if not Global.Shm.Has_Proxy then
            raise AWT.Initialization_Error with "Wayland: Failed to get shm";
         end if;
         Add_Pair ((Id, Global.Shm.Get_ID));
      elsif Name = WP.Client.Seat_Interface.Name then
         if not Global.Seat.Seat.Has_Proxy then
            Global.Seat.Seat.Bind (Registry, Id, Unsigned_32'Min (Version, 6));

            if not Global.Seat.Seat.Has_Proxy then
               raise Initialization_Error with "Wayland: Failed to get seat";
            end if;
            Add_Pair ((Id, Global.Seat.Seat.Get_ID));

            Seat_Events.Subscribe (Global.Seat.Seat);
         end if;
      elsif Name = WP.Client.Output_Interface.Name then
         for Monitor of Global.Monitors loop
            if not Monitor.Output.Has_Proxy then
               Monitor.Output.Bind (Registry, Id, Unsigned_32'Min (Version, 3));

               if not Monitor.Output.Has_Proxy then
                  raise Initialization_Error with "Wayland: Failed to get output";
               end if;
               Add_Pair ((Id, Monitor.Output.Get_ID));

               Output_Events.Subscribe (Monitor.Output);

               return;
            end if;
         end loop;
      elsif Name = WP.Client.Data_Device_Manager_Interface.Name then
         Global.Data_Device_Manager.Bind (Registry, Id, Unsigned_32'Min (Version, 3));

         if not Global.Data_Device_Manager.Has_Proxy then
            raise AWT.Initialization_Error with "Wayland: Failed to get data device manager";
         end if;
         Add_Pair ((Id, Global.Data_Device_Manager.Get_ID));
      elsif Name = WP.Presentation_Time.Presentation_Interface.Name then
         Global.Presentation.Bind (Registry, Id, Unsigned_32'Min (Version, 1));

         if not Global.Presentation.Has_Proxy then
            raise Initialization_Error with "Wayland: Failed to get presentation";
         end if;
         Add_Pair ((Id, Global.Presentation.Get_ID));

         Presentation_Events.Subscribe (Global.Presentation);
      elsif Name = WP.Xdg_Shell.Xdg_Wm_Base_Interface.Name then
         Global.XDG_Shell.Bind (Registry, Id, Unsigned_32'Min (Version, 1));

         if not Global.XDG_Shell.Has_Proxy then
            raise Initialization_Error with "Wayland: Failed to get XDG shell";
         end if;
         Add_Pair ((Id, Global.XDG_Shell.Get_ID));

         Base_Events.Subscribe (Global.XDG_Shell);
      elsif Name = XD.Decoration_Manager_V1_Interface.Name then
         Global.XDG_Decoration_Manager.Bind (Registry, Id, Unsigned_32'Min (Version, 1));

         if not Global.XDG_Decoration_Manager.Has_Proxy then
            raise Initialization_Error with "Wayland: Failed to get XDG decoration manager";
         end if;
         Add_Pair ((Id, Global.XDG_Decoration_Manager.Get_ID));
      elsif Name = II.Idle_Inhibit_Manager_V1_Interface.Name then
         Global.Inhibit_Manager.Bind (Registry, Id, Unsigned_32'Min (Version, 1));

         if not Global.Inhibit_Manager.Has_Proxy then
            raise Initialization_Error with "Wayland: Failed to get idle inhibit manager";
         end if;
         Add_Pair ((Id, Global.Inhibit_Manager.Get_ID));
      elsif Name = RP.Relative_Pointer_Manager_V1_Interface.Name then
         Global.Relative_Pointer_Manager.Bind (Registry, Id, Unsigned_32'Min (Version, 1));

         if not Global.Relative_Pointer_Manager.Has_Proxy then
            raise Initialization_Error with "Wayland: Failed to get relative pointer manager";
         end if;
         Add_Pair ((Id, Global.Relative_Pointer_Manager.Get_ID));
      elsif Name = PC.Pointer_Constraints_V1_Interface.Name then
         Global.Pointer_Constraints.Bind (Registry, Id, Unsigned_32'Min (Version, 1));

         if not Global.Pointer_Constraints.Has_Proxy then
            raise Initialization_Error with "Wayland: Failed to get pointer constraints";
         end if;
         Add_Pair ((Id, Global.Pointer_Constraints.Get_ID));
      elsif Registry_Callback /= null then
         Registry_Callback (Registry, Id, Name, Version);

         if not Registry.Has_Proxy then
            raise Program_Error;
         end if;
      end if;
   end Global_Registry_Handler;

   procedure Remove_Object (ID : Unsigned_32) is
      use type AWT.Monitors.Monitor_Event_Listener_Ptr;
   begin
      if Global.Seat.Seat.Has_Proxy and then Global.Seat.Seat.Get_ID = ID then
         raise Program_Error with "Unexpectedly removed used seat";
      end if;

      for Monitor of Global.Monitors loop
         if Monitor.Output.Has_Proxy and then Monitor.Output.Get_ID = ID then
            if Monitor_Listener /= null then
               Monitor_Listener.On_Disconnect (Monitor => Monitor'Access);
            end if;
            Monitor.Output.Release;
            Monitor.Initialized := False;
         end if;
      end loop;
   end Remove_Object;

   procedure Global_Registry_Remover
     (Registry : in out WP.Client.Registry'Class;
      Id       : Unsigned_32)
   is
      Result : ID_Pair;
      Found  : Boolean := True;
   begin
      while Found loop
         Remove_First_Pair (Id, Result, Found);

         if Found then
            Remove_Object (Result.Object_ID);
         end if;
      end loop;
   end Global_Registry_Remover;

   package Registry_Events is new WP.client.Registry_Events
     (Global_Object_Added   => Global_Registry_Handler,
      Global_Object_Removed => Global_Registry_Remover);

   procedure Initialize is
   begin
      WP.Idle_Inhibit_Unstable_V1.Initialize;
      WP.Pointer_Constraints_Unstable_V1.Initialize;
      WP.Presentation_Time.Initialize;
      WP.Relative_Pointer_Unstable_V1.Initialize;
      WP.Xdg_Decoration_Unstable_V1.Initialize;
      WP.Xdg_Shell.Initialize;

      Global.Display.Connect;

      if not Global.Display.Is_Connected then
         raise Initialization_Error with "Wayland: No connection to compositor";
      end if;

      Global.Display.Get_Registry (Global.Registry);

      if not Global.Registry.Has_Proxy then
         raise Initialization_Error with "Wayland: Failed to get registry";
      end if;

      Registry_Events.Subscribe (Global.Registry);

      Global.Display.Dispatch;
      Global.Display.Roundtrip;

      Initialize_Clipboard_And_Drag_And_Drop;

      Global.Cursor_Theme.Load_Theme ("", 16, Global.Shm);

      Global.Initialized := True;
   exception
      when others =>
         Global.Finalize;
         raise;
   end Initialize;

   overriding procedure Finalize (Object : in out Compositor) is
   begin
      if Global.Cursor_Theme.Is_Initialized then
         Global.Cursor_Theme.Destroy;
      end if;

      if Global.XDG_Decoration_Manager.Has_Proxy then
         Global.XDG_Decoration_Manager.Destroy;
      end if;

      if Global.XDG_Shell.Has_Proxy then
         Global.XDG_Shell.Destroy;
      end if;

      if Global.Inhibit_Manager.Has_Proxy then
         Global.Inhibit_Manager.Destroy;
      end if;

      if Global.Presentation.Has_Proxy then
         Global.Presentation.Destroy;
      end if;

      if Global.Seat.Data_Offer.Has_Proxy then
         Global.Seat.Data_Offer.Destroy;
      end if;

      if Global.Seat.Data_Device.Has_Proxy then
         Global.Seat.Data_Device.Release;
      end if;

      Release_Seat_Capabilities (Global.Seat, (others => False));

      if Global.Seat.Seat.Has_Proxy then
         Global.Seat.Seat.Release;
      end if;

      if Global.Relative_Pointer_Manager.Has_Proxy then
         Global.Relative_Pointer_Manager.Destroy;
      end if;

      if Global.Pointer_Constraints.Has_Proxy then
         Global.Pointer_Constraints.Destroy;
      end if;

      for Monitor of Global.Monitors loop
         if Monitor.Output.Has_Proxy then
            Monitor.Output.Release;
         end if;
      end loop;

      if Global.Data_Source.Has_Proxy then
         Global.Data_Source.Destroy;
      end if;

      if Global.Data_Device_Manager.Has_Proxy then
         Global.Data_Device_Manager.Destroy;
      end if;

      if Global.Shm.Has_Proxy then
         Global.Shm.Destroy;
      end if;

      if Global.Compositor.Has_Proxy then
         Global.Compositor.Destroy;
      end if;

      if Global.Registry.Has_Proxy then
         Global.Registry.Destroy;
      end if;

      if Global.Display.Is_Connected then
         Global.Display.Disconnect;
      end if;

      Global.Initialized := False;
   end Finalize;

   function Is_Initialized return Boolean is (Global.Initialized);

   function Dispatch_Events
     (Timeout     : Duration;
      Descriptors : WP.Client.File_Descriptor_Array;
      Events      : out WP.Client.Events_Status_Array) return Standard.Wayland.Optional_Result
   is
      use all type WP.Client.Events_Status;
   begin
      if Global.Seat.Window /= null then
         AWT.Wayland.Windows.Update_Animated_Cursor (Global.Seat.Window);
      end if;

      while Global.Display.Prepare_Read = Error loop
         if not Global.Display.Dispatch_Pending.Is_Success then
            raise Internal_Error with "Wayland: Failed dispatching pending events";
         end if;
      end loop;

      if not Global.Display.Flush.Is_Success then
         Global.Display.Cancel_Read;
         raise Internal_Error with "Wayland: Failed to flush";
      end if;

      declare
         use type WP.Client.Events_Status_Array;

         Events_Statuses : WP.Client.Events_Status_Array :=
           No_Events & (Descriptors'Range => No_Events);
      begin
         if Timeout > 0.0 then
            Events_Statuses := Global.Display.Check_For_Events (Timeout, Descriptors);
         end if;

         Events := Events_Statuses (Events_Statuses'First + 1 .. Events_Statuses'Last);

         case Events_Statuses (Events_Statuses'First) is
            when Has_Events =>
               if Global.Display.Read_Events = Error then
                  raise Internal_Error with "Wayland: Failed reading events";
               end if;

               if not Global.Display.Dispatch_Pending.Is_Success then
                  raise Internal_Error with "Wayland: Failed dispatching pending events";
               end if;

               return (Is_Success => True, Count => 1);
            when No_Events =>
               Global.Display.Cancel_Read;

               return (Is_Success => True, Count => 0);
            when Error =>
               Global.Display.Cancel_Read;
               raise Internal_Error with "Wayland: Failed dispatching pending events";
         end case;
      end;
   end Dispatch_Events;

   procedure Process_Events (Timeout : Duration) is
      Result : Standard.Wayland.Optional_Result;

      use all type WP.Client.Events_Status;

      Count : constant Natural := (if Gamepad_Notify_Callback /= null then 1 else 0);

      Events      : WP.Client.Events_Status_Array (1 .. Count);
      Descriptors : WP.Client.File_Descriptor_Array (1 .. Count);

      Clock_Timeout : constant Duration := Orka.OS.Monotonic_Clock + Timeout;
   begin
      if Global.Seat.Window /= null then
         AWT.Wayland.Windows.Update_Animated_Cursor (Global.Seat.Window);

         --  If the application has called function State of a window, then
         --  reset some of its pointer and keyboard state just before updating it
         Global.Seat.Window.Reset_Input_State;
      end if;

      if Count = 1 then
         Descriptors (1) := Gamepad_Notify_FD;
      end if;

      loop
         Result := Dispatch_Events (Clock_Timeout - Orka.OS.Monotonic_Clock, Descriptors, Events);

         if Gamepad_Notify_Callback /= null and then Events (Events'First) = Has_Events then
            Gamepad_Notify_Callback.all;
         end if;

         if not Result.Is_Success then
            raise Internal_Error with "Wayland: Failed dispatching events";
         end if;

         exit when Result.Count = 0;
      end loop;
   end Process_Events;

   function Monitors return AWT.Monitors.Monitor_Array is
      Count : Natural := 0;

      First_Monitor : constant AWT.Monitors.Monitor_Ptr :=
        Global.Monitors (Global.Monitors'First)'Access;
   begin
      for Monitor of Global.Monitors loop
         if Monitor.Output.Has_Proxy then
            Count := Count + 1;
         end if;
      end loop;

      return Result : AWT.Monitors.Monitor_Array (1 .. Count) := (others => First_Monitor) do
         declare
            Index : Positive := Result'First;
         begin
            for Monitor of Global.Monitors loop
               if Monitor.Output.Has_Proxy then
                  Result (Index) := Monitor'Access;
                  Index := Index + 1;
               end if;
            end loop;
            pragma Assert (Index - 1 = Count);
         end;
      end return;
   end Monitors;

end AWT.Registry;
