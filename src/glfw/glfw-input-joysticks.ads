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

private with Glfw.Enums;

package Glfw.Input.Joysticks is
   pragma Preelaborate;

   use type Interfaces.C.C_float;

   type Axis_Position is new Interfaces.C.C_float range -1.0 .. 1.0;
   type Axis_Positions is array (Positive range <>) of aliased Axis_Position;

   type Joystick_Button_State is new Button_State;
   type Joystick_Button_States is array (Positive range <>) of
     aliased Joystick_Button_State;

   subtype Gamepad_Button_Index is Positive range 1 .. 15;
   subtype Gamepad_Axis_Index   is Positive range 1 .. 6;

   type Joystick_Gamepad_State is record
      Buttons : Joystick_Button_States (Gamepad_Button_Index);
      Axes    : Axis_Positions (Gamepad_Axis_Index);
   end record
     with Convention => C;

   type Joystick_Hat_State is
     (Centered, Up, Right, Right_Up, Down, Right_Down, Left, Left_Up, Left_Down);

   type Joystick_Hat_States is array (Positive range <>) of
     aliased Joystick_Hat_State;

   type Connect_State is (Connected, Disconnected);

   -----------------------------------------------------------------------------

   type Joystick_Index is range 1 .. 16;
   --  GLFW supports up to 16 joysticks; they are indexed from 1 to 16

   type Joystick is tagged private;
   --  A Joystick object will link to the first joystick by default

   function Get_Joystick (Index : Joystick_Index) return Joystick;

   function Index (Source : Joystick) return Joystick_Index;
   
   function Present (Source : Joystick) return Boolean;

   function Is_Gamepad (Source : Joystick) return Boolean;

   function Gamepad_Name (Source : Joystick) return String;

   function Gamepad_State (Source : Joystick) return Joystick_Gamepad_State;

   function Joystick_Name (Source : Joystick) return String;

   function Joystick_GUID (Source : Joystick) return String;

   procedure Update_Gamepad_Mappings (Mappings : String);
   
   function Positions (Source : Joystick) return Axis_Positions;
   function Button_States (Source : Joystick) return Joystick_Button_States;
   function Hat_States (Source : Joystick) return Joystick_Hat_States;

   type Joystick_Callback is access procedure
     (Source : Joystick; State : Connect_State);

   procedure Set_Callback (Callback : Joystick_Callback);
   --  Enable or disable a callback to receive joystick (dis)connection
   --  events
   --
   --  Task safety: Must only be called from the environment task.

private

   type Joystick is tagged record
      Raw_Index : Enums.Joystick_ID := Enums.Joystick_1;
   end record;
   
   for Joystick_Button_State'Size use Interfaces.C.char'Size;

   for Joystick_Hat_State use
     (Centered   => 0,
      Up         => 1,
      Right      => 2,
      Right_Up   => 3,
      Down       => 4,
      Right_Down => 6,
      Left       => 8,
      Left_Up    => 9,
      Left_Down  => 12);
   for Joystick_Hat_State'Size use Interfaces.C.char'Size;

   for Connect_State use (Connected    => 16#40001#,
                          Disconnected => 16#40002#);
   for Connect_State'Size use Interfaces.C.int'Size;
end Glfw.Input.Joysticks;
