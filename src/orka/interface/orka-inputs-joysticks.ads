--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2019 onox <denkpadje@gmail.com>
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

with GL.Types;

package Orka.Inputs.Joysticks is
   pragma Preelaborate;

   use type GL.Types.Single;

   type Axis_Position is new GL.Types.Single range -1.0 .. 1.0;

   type Button_State is (Released, Pressed);

   type Hat_State is
     (Centered, Up, Right, Right_Up, Down, Right_Down, Left, Left_Up, Left_Down);

   type Button_States  is array (Positive range <>) of Button_State with Pack;
   type Axis_Positions is array (Positive range <>) of aliased Axis_Position;
   type Hat_States     is array (Positive range <>) of aliased Hat_State;

   subtype Button_Index is Positive range 1 .. 64;
   subtype Axis_Index   is Positive range 1 .. 8;
   subtype Hat_Index    is Positive range 1 .. 8;

   type Joystick_State (Button_Count, Axis_Count, Hat_Count : Natural := 0) is record
      Buttons : Button_States  (Button_Index) := (others => Released);
      Axes    : Axis_Positions (Axis_Index)   := (others => 0.0);
      Hats    : Hat_States     (Hat_Index)    := (others => Centered);
   end record
     with Dynamic_Predicate =>
       (Button_Count <= Buttons'Length or else
         raise Constraint_Error with
           "Joystick has too many buttons:" & Button_Count'Image) and then
       (Axis_Count <= Axes'Length or else
         raise Constraint_Error with
           "Joystick has too many axes:" & Axis_Count'Image) and then
       (Hat_Count <= Hats'Length or else
         raise Constraint_Error with
           "Joystick has too many hats:" & Hat_Count'Image);

   -----------------------------------------------------------------------------

   type Joystick_Input is limited interface;

   type Joystick_Input_Access is access Joystick_Input'Class;

   subtype Joystick_Input_Ptr is not null Joystick_Input_Access;

   function Is_Present (Object : Joystick_Input) return Boolean is abstract;
   function Is_Gamepad (Object : Joystick_Input) return Boolean is abstract;

   function Name (Object : Joystick_Input) return String is abstract;
   function GUID (Object : Joystick_Input) return String is abstract;

   procedure Update_State
     (Object  : in out Joystick_Input;
      Process : access procedure (Value : in out Axis_Position;
                                  Index :        Positive)) is abstract;

   function Current_State (Object : Joystick_Input) return Joystick_State is abstract;
   function Last_State    (Object : Joystick_Input) return Joystick_State is abstract;

   subtype Joystick_Button_States is Button_States (Button_Index);

   type Boolean_Button_States is array (Joystick_Button_States'Range) of Boolean with Pack;

   function Just_Pressed  (Object : Joystick_Input'Class) return Boolean_Button_States;
   function Just_Released (Object : Joystick_Input'Class) return Boolean_Button_States;

   Disconnected_Error : exception;

end Orka.Inputs.Joysticks;
