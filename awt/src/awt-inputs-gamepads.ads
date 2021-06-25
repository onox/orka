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

private with Ada.Finalization;
private with Ada.Strings.Unbounded;

private with AWT.Gamepads;

package AWT.Inputs.Gamepads with SPARK_Mode => On is
   pragma Preelaborate;

   type Connection_Kind is (Disconnected, Wired, Wireless);

   type GUID_String is new String (1 .. 32)
     with Dynamic_Predicate => (for all C of GUID_String => C in '0' .. '9' | 'a' .. 'f');

   type Normalized is new Float range 0.0 .. 1.0;

   ----------------------------------------------------------------------------

   type Gamepad_Button is
     (Direction_Up,
      Direction_Right,
      Direction_Down,
      Direction_Left,
      Action_Down,
      Action_Right,
      Action_Left,
      Action_Up,
      Shoulder_Left,
      Shoulder_Right,
      Thumb_Left,
      Thumb_Right,
      Center_Left,
      Center_Right,
      Center_Logo);

   type Gamepad_Axis is
     (Stick_Left_X,
      Stick_Left_Y,
      Stick_Right_X,
      Stick_Right_Y);

   type Gamepad_Trigger is
     (Trigger_Left,
      Trigger_Right);

   type Gamepad_Buttons is array (Gamepad_Button) of Button_State
     with Component_Size => 1;

   type Changed_Gamepad_Buttons is array (Gamepad_Button) of Boolean
     with Component_Size => 1;

   type Axis_Position is delta 2.0 ** (-15) range -1.0 .. 1.0 - 2.0 ** (-15)
     with Small => 2.0 ** (-15),
          Size  => 16;

   type Trigger_Position is delta 2.0 ** (-8) range 0.0 .. 1.0 - 2.0 ** (-8)
     with Small => 2.0 ** (-8),
          Size  => 8;

   type Gamepad_Axes is array (Gamepad_Axis) of Axis_Position;

   type Gamepad_Triggers is array (Gamepad_Trigger) of Trigger_Position;

   type Gamepad_State is record
      Buttons   : Gamepad_Buttons         := (others => Released);
      Pressed   : Changed_Gamepad_Buttons := (others => False);
      Released  : Changed_Gamepad_Buttons := (others => False);

      Axes     : Gamepad_Axes     := (others => 0.0);
      Triggers : Gamepad_Triggers := (others => 0.0);
   end record;

   ----------------------------------------------------------------------------

   type Battery_Capacity is range 0 .. 100;

   type Battery_Status is (Discharging, Charging, Not_Charging);

   type Battery_State (Is_Present : Boolean := False) is record
      case Is_Present is
         when True =>
            Capacity : Battery_Capacity;
            Status   : Battery_Status;
         when False =>
            null;
      end case;
   end record;

   ----------------------------------------------------------------------------

   type Color_Kind is (Red, Green, Blue);

   type RGB_Color is array (Color_Kind) of Normalized;

   type LED_State (Is_Present : Boolean := False) is record
      case Is_Present is
         when True =>
            Brightness : Normalized;
            Color      : RGB_Color;
         when False =>
            null;
      end case;
   end record;

   ----------------------------------------------------------------------------

   type Sensor_Axis_Value is delta 2.0 ** (-16) range -(2.0 ** 15) .. +(2.0 ** 15 - 2.0 ** (-16))
     with Small => 2.0 ** (-16),
          Size  => 32;

   type Sensor_Axis is (X, Y, Z, Rx, Ry, Rz);

   type Sensor_Axes is array (Sensor_Axis) of Sensor_Axis_Value;

   type Motion_State (Is_Present : Boolean := False)  is record
      case Is_Present is
         when True =>
            Axes : Sensor_Axes := (others => 0.0);
         when False =>
            null;
      end case;
   end record;

   ----------------------------------------------------------------------------

   type Gamepad is tagged limited private;

   function Name (Object : Gamepad) return String;

   function Serial_Number (Object : Gamepad) return String;

   function GUID (Object : Gamepad) return GUID_String;

   function Connection (Object : Gamepad) return Connection_Kind;

   function State (Object : in out Gamepad) return Gamepad_State;

   function State (Object : Gamepad) return Motion_State;

   function State (Object : Gamepad) return Battery_State;

   function State (Object : Gamepad) return LED_State;

   procedure Set_LED
     (Object     : in out Gamepad;
      Brightness : Normalized;
      Color      : RGB_Color);

   type Effect is private;

   function Rumble_Effect
     (Length, Offset : Duration;
      Strong, Weak   : Normalized) return Effect;

   function Periodic_Effect
     (Length, Offset : Duration;
      Magnitude      : Normalized;
      Attack, Fade   : Duration) return Effect;

   procedure Play_Effect (Object : in out Gamepad; Subject : Effect);

   procedure Cancel_Effect (Object : in out Gamepad; Subject : Effect);

   function Effects (Object : Gamepad) return Natural;

   procedure Log_Information (Gamepad : AWT.Inputs.Gamepads.Gamepad'Class);

   ----------------------------------------------------------------------------

   procedure Set_Mappings (Text : String);

   procedure Initialize;

   procedure Poll;

   type Gamepad_Ptr is not null access all Gamepad;

   type Gamepad_Array is array (Positive range <>) of Gamepad_Ptr;

   function Gamepads return Gamepad_Array;

   ----------------------------------------------------------------------------

   type Gamepad_Event_Listener is abstract tagged limited private;

   procedure On_Connect
     (Object  : Gamepad_Event_Listener;
      Gamepad : Gamepad_Ptr) is abstract;

   procedure On_Disconnect
     (Object  : Gamepad_Event_Listener;
      Gamepad : Gamepad_Ptr) is abstract;

private

   type Axis_Value is delta 2.0 ** (-16)
     range -(2.0 ** 47) ..
           +(2.0 ** 47 - 2.0 ** (-16));

   type Output_Kind is (Button, Axis, Trigger, None);

   type Output_Mapping (Kind : Output_Kind := None) is record
      case Kind is
         when Axis =>
            Axis    : AWT.Inputs.Gamepads.Gamepad_Axis;
            Scale   : Axis_Value := 2.0;
            Offset  : Axis_Value := 1.0;
         when Trigger =>
            Trigger : AWT.Inputs.Gamepads.Gamepad_Trigger;
         when Button =>
            Button  : AWT.Inputs.Gamepads.Gamepad_Button;
         when None =>
            null;
      end case;
   end record;

   type Side_Type is (Negative_Half, Positive_Half, Full_Range);

   subtype Hat_Side is Side_Type range Negative_Half .. Positive_Half;

   type Axis_Modifier is record
      Offset, Scale, Resolution : Axis_Value;
   end record;

   type Input_Mapping is record
      Modifier : Axis_Modifier;

      Side   : Side_Type := Full_Range;
      Invert : Boolean   := False;
   end record;

   type Mapping (Kind : Output_Kind := None) is record
      Input  : Input_Mapping;
      Output : Output_Mapping (Kind);
   end record;

   type Axis_Mappings is array (AWT.Gamepads.Input_Axis) of Mapping;
   type Key_Mappings  is array (AWT.Gamepads.Input_Button) of Mapping;
   type Hat_Mappings  is array (AWT.Gamepads.Input_Hat, Hat_Side) of Mapping;

   type Sensor_Modifiers is array (AWT.Gamepads.Sensor_Axis) of Axis_Modifier;

   type Gamepad is limited new AWT.Gamepads.Abstract_Gamepad with record
      Name, ID    : SU.Unbounded_String;
      GUID        : GUID_String;

      Gamepad : Gamepad_State;
      Sensor  : Motion_State;

      Axes : Axis_Mappings;
      Keys : Key_Mappings;
      Hats : Hat_Mappings;

      Sensors : Sensor_Modifiers;

      Has_Rumble  : Boolean := False;
      Initialized : Boolean := False;
      Max_Effects : Natural := 0;
   end record;

   ----------------------------------------------------------------------------

   type Effect is new AWT.Gamepads.Abstract_Effect;

   ----------------------------------------------------------------------------

   type Gamepad_Event_Listener is
     abstract limited new Ada.Finalization.Limited_Controlled with null record;

   overriding procedure Initialize (Object : in out Gamepad_Event_Listener);
   overriding procedure Finalize   (Object : in out Gamepad_Event_Listener);

end AWT.Inputs.Gamepads;
