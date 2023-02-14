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

with Ada.Finalization;

with Wayland.Cursor;
with Wayland.Enums.Client;
with Wayland.Protocols.Client;
with Wayland.Protocols.Idle_Inhibit_Unstable_V1;
with Wayland.Protocols.Pointer_Constraints_Unstable_V1;
with Wayland.Protocols.Presentation_Time;
with Wayland.Protocols.Relative_Pointer_Unstable_V1;
with Wayland.Protocols.Xdg_Decoration_Unstable_V1;
with Wayland.Protocols.Xdg_Shell;

with AWT.Inputs;
with AWT.Monitors;
with AWT.Wayland.Windows;

private package AWT.Registry is
   pragma Preelaborate;
   pragma Elaborate_Body;

   UTF_8_Mime_Type : constant String := "text/plain;charset=utf-8";
   URIs_Mime_Type  : constant String := "text/uri-list";

   subtype Unsigned_32 is Standard.Wayland.Unsigned_32;

   package WC renames Standard.Wayland.Cursor;
   package WE renames Standard.Wayland.Enums;
   package WP renames Standard.Wayland.Protocols;

   package II renames WP.Idle_Inhibit_Unstable_V1;
   package RP renames WP.Relative_Pointer_Unstable_V1;
   package XD renames WP.Xdg_Decoration_Unstable_V1;
   package PC renames WP.Pointer_Constraints_Unstable_V1;

   type Seat_Devices;

   type Seat_With_Seat (Seat : not null access Seat_Devices)
     is new WP.Client.Seat with null record;

   type Keyboard_With_Seat (Seat : not null access Seat_Devices)
     is new WP.Client.Keyboard with null record;

   type Pointer_With_Seat (Seat : not null access Seat_Devices)
     is new WP.Client.Pointer with null record;

   type Touch_With_Seat (Seat : not null access Seat_Devices)
     is new WP.Client.Touch with null record;

   type Relative_Pointer_With_Seat (Seat : not null access Seat_Devices)
     is new RP.Relative_Pointer_V1 with null record;

   type Pointer_Scrolling is array (AWT.Inputs.Dimension) of Boolean;

   type Seat_Devices is tagged limited record
      Window           : access AWT.Wayland.Windows.Wayland_Window;
      Keyboard_Window  : access AWT.Wayland.Windows.Wayland_Window;
      Drag_Drop_Window : access AWT.Wayland.Windows.Wayland_Window;

      Seat         : Seat_With_Seat (Seat_Devices'Access);
      Capabilities : WE.Client.Seat_Capability := (others => False);

      Keyboard : Keyboard_With_Seat (Seat_Devices'Access);
      Pointer  : Pointer_With_Seat (Seat_Devices'Access);
      Touch    : Touch_With_Seat (Seat_Devices'Access);

      Data_Device     : WP.Client.Data_Device;
      Data_Offer      : WP.Client.Data_Offer;

      Clipboard_Mime_Type_Valid : Boolean := False;
      Drag_Drop_Mime_Type_Valid : Boolean := False;

      Supported_Drag_Drop_Actions : AWT.Inputs.Actions     := (others => False);
      Valid_Drag_Drop_Action      : AWT.Inputs.Action_Kind := AWT.Inputs.None;

      Relative_Pointer : Relative_Pointer_With_Seat (Seat_Devices'Access);

      Pointer_State   : AWT.Inputs.Pointer_State;
      Scroll_Discrete : Boolean := False;
      Scrolling       : Pointer_Scrolling;

      Pointer_Enter_Serial : Unsigned_32;
      --  Needed when setting a cursor

      Keyboard_Enter_Serial : Unsigned_32;
      --  Needed for clipboard and drag and drop

      Keyboard_State : AWT.Inputs.Keyboard_State;
   end record;

   type Monitor_Device;

   type Output_With_Monitor (Monitor : not null access Monitor_Device)
     is new WP.Client.Output with null record;

   type Monitor_Device is limited new AWT.Monitors.Monitor with record
      Output : Output_With_Monitor (Monitor_Device'Access);

      Pending_State, Current_State : AWT.Monitors.Monitor_State;

      --  Geometry
      Physical_Width  : Natural;
      Physical_Height : Natural;
      Subpixel        : WE.Client.Output_Subpixel;
      Transform       : WE.Client.Output_Transform;

      Initialized : Boolean := True;
   end record;

   overriding
   function Is_Connected (Object : Monitor_Device) return Boolean;

   overriding
   function ID (Object : Monitor_Device) return Natural;

   overriding
   function State (Object : Monitor_Device) return AWT.Monitors.Monitor_State;

   type Monitor_Array is array (Positive range <>) of aliased Monitor_Device;

   type Compositor (Maximum_Monitors : Positive) is
     limited new Ada.Finalization.Limited_Controlled with
   record
      Display    : aliased WP.Client.Display;

      Registry   : WP.Client.Registry;
      Compositor : WP.Client.Compositor;
      Shm        : WP.Client.Shm;

      Data_Device_Manager : WP.Client.Data_Device_Manager;
      Data_Source         : WP.Client.Data_Source;

      Seat     : Seat_Devices;
      Monitors : Monitor_Array (1 .. Maximum_Monitors);

      Inhibit_Manager          : II.Idle_Inhibit_Manager_V1;
      Presentation             : WP.Presentation_Time.Presentation;
      Pointer_Constraints      : PC.Pointer_Constraints_V1;
      Relative_Pointer_Manager : RP.Relative_Pointer_Manager_V1;
      XDG_Shell                : WP.Xdg_Shell.Xdg_Wm_Base;
      XDG_Decoration_Manager   : XD.Decoration_Manager_V1;

      Cursor_Theme : WC.Cursor_Theme;

      Initialized : Boolean := False;
   end record;
   pragma Preelaborable_Initialization (Compositor);

   overriding procedure Finalize (Object : in out Compositor);

   package Surface_Data is new WP.Client.Surface_User_Data
     (AWT.Wayland.Windows.Surface_With_Window);

   package Output_Data is new WP.Client.Output_User_Data (Output_With_Monitor);

   Global : Compositor (Maximum_Monitors => 16);

   Registry_Callback : AWT.Wayland.On_Available_Interface;

   Monitor_Listener : AWT.Monitors.Monitor_Event_Listener_Ptr;

   procedure Initialize;

   function Is_Initialized return Boolean;

   procedure Process_Events (Timeout : Duration);

   function Monitors return AWT.Monitors.Monitor_Array;

   ----------------------------------------------------------------------------

   use type Standard.Wayland.File_Descriptor;

   Gamepad_Notify_FD       : Standard.Wayland.File_Descriptor := -1;
   Gamepad_Notify_Callback : access procedure := null;

end AWT.Registry;
