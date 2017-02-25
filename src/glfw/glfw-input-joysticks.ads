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
   
   -- GLFW supports up to 16 joysticks; they are indexed from 1 to 16.
   type Joystick_Index is range 1 .. 16;
   
   -- A Joystick object will link to the first joystick by default.
   type Joystick is tagged private;
   
   type Axis_Position is new Interfaces.C.C_float range -1.0 .. 1.0;
   type Axis_Positions is array (Positive range <>) of aliased Axis_Position;
   
   type Joystick_Button_State is new Button_State;
   type Joystick_Button_States is array (Positive range <>) of
     aliased Joystick_Button_State;
   
   function Index (Source : Joystick) return Joystick_Index;
   procedure Set_Index (Target : in out Joystick; Value : Joystick_Index);
   
   function Present (Source : Joystick) return Boolean;
   
   function Positions (Source : Joystick) return Axis_Positions;
   function Button_States (Source : Joystick) return Joystick_Button_States;
   
private
   type Joystick is tagged record
      Raw_Index : Enums.Joystick_ID := Enums.Joystick_1;
   end record;
   
   for Joystick_Button_State'Size use Interfaces.C.char'Size;
end Glfw.Input.Joysticks;
