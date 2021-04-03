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

package body Orka.Inputs.Joysticks.Default is

   overriding
   function Current_State
     (Object : Abstract_Joystick_Input) return Joystick_State is (Object.Current_State);

   overriding
   function Last_State
     (Object : Abstract_Joystick_Input) return Joystick_State is (Object.Last_State);

   overriding
   procedure Update_State
     (Object  : in out Abstract_Joystick_Input;
      Process : access procedure (Value : in out Axis_Position;
                                  Index :        Positive)) is
   begin
      Object.Last_State    := Object.Current_State;
      Object.Current_State := Abstract_Joystick_Input'Class (Object).State;

      if Process /= null then
         for Index in 1 .. Object.Current_State.Axis_Count loop
            Process (Object.Current_State.Axes (Index), Index);
         end loop;
      end if;
   end Update_State;

   -----------------------------------------------------------------------------

   type Pointer_Joystick_Input is new Abstract_Joystick_Input with record
      Pointer : Inputs.Pointers.Pointer_Input_Ptr;
      Axes    : Orka.Transforms.Doubles.Vectors.Vector4 := (others => 0.0);
      Scales  : Orka.Transforms.Doubles.Vectors.Vector4;
   end record;

   overriding
   function Is_Present (Object : Pointer_Joystick_Input) return Boolean is (True);

   overriding
   function Name (Object : Pointer_Joystick_Input) return String is ("Pointer");

   overriding
   function GUID (Object : Pointer_Joystick_Input) return String is ("pointer");

   overriding
   function Is_Gamepad (Object : Pointer_Joystick_Input) return Boolean is (False);

   overriding
   function State (Object : in out Pointer_Joystick_Input) return Joystick_State is
      use type GL.Types.Double;
      use all type Inputs.Pointers.Dimension;
      use all type Inputs.Pointers.Pointer_Mode;

      function To_Button (Value : Inputs.Pointers.Button_State) return Button_State is
        (case Value is
           when Inputs.Pointers.Pressed  => Pressed,
           when Inputs.Pointers.Released => Released);

      function Clamp (Value, Minimum, Maximum : GL.Types.Double) return GL.Types.Double is
        (GL.Types.Double'Max (Minimum, GL.Types.Double'Min (Value, Maximum)));

      Limit_X : constant GL.Types.Double := 1.0 / Object.Scales (X);
      Limit_Y : constant GL.Types.Double := 1.0 / Object.Scales (Y);
      Limit_Z : constant GL.Types.Double := 1.0 / Object.Scales (Z);
      Limit_W : constant GL.Types.Double := 1.0 / Object.Scales (W);

      Pointer_State : constant Inputs.Pointers.Pointer_State := Object.Pointer.State;
   begin
      if Pointer_State.Mode = Locked then
         Object.Axes (X) :=
           Clamp (Object.Axes (X) + Pointer_State.Relative (X), -Limit_X, Limit_X);
         Object.Axes (Y) :=
           Clamp (Object.Axes (Y) - Pointer_State.Relative (Y), -Limit_Y, Limit_Y);
      end if;
      Object.Axes (Z) := Clamp (Object.Axes (Z) + Pointer_State.Relative (X), -Limit_Z, Limit_Z);
      Object.Axes (W) := Clamp (Object.Axes (W) - Pointer_State.Relative (Y), -Limit_W, Limit_W);

      return Result : Joystick_State
        (Button_Count => 3,
         Axis_Count   => 4,
         Hat_Count    => 0)
      do
         Result.Buttons (1) := To_Button (Pointer_State.Buttons (Inputs.Pointers.Left));
         Result.Buttons (2) := To_Button (Pointer_State.Buttons (Inputs.Pointers.Right));
         Result.Buttons (3) := To_Button (Pointer_State.Buttons (Inputs.Pointers.Middle));

         Result.Axes (1) := Axis_Position (Object.Axes (X) * Object.Scales (X));
         Result.Axes (2) := Axis_Position (Object.Axes (Y) * Object.Scales (Y));
         Result.Axes (3) := Axis_Position (Object.Axes (Z) * Object.Scales (Z));
         Result.Axes (4) := Axis_Position (Object.Axes (W) * Object.Scales (W));
      end return;
   end State;

   function Create_Joystick_Input
     (Pointer : Inputs.Pointers.Pointer_Input_Ptr;
      Scales  : Orka.Transforms.Doubles.Vectors.Vector4) return Joystick_Input_Ptr is
   begin
      return new Pointer_Joystick_Input'(Abstract_Joystick_Input with
        Pointer => Pointer,
        Scales  => Scales,
        others  => <>);
   end Create_Joystick_Input;

end Orka.Inputs.Joysticks.Default;
