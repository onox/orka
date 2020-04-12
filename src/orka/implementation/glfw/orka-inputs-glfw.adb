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

with Ada.Strings.Unbounded;

with Glfw.Input.Joysticks;

with Orka.Inputs.Joysticks.Default;
with Orka.Inputs.Joysticks.Gamepads;

package body Orka.Inputs.GLFW is

   overriding
   procedure Set_Cursor_Mode
     (Object  : in out GLFW_Pointer_Input;
      Mode    : Pointers.Default.Cursor_Mode)
   is
      package Mouse renames Standard.Glfw.Input.Mouse;

      use all type Pointers.Default.Cursor_Mode;
   begin
      case Mode is
         when Normal =>
            Object.Window.Set_Cursor_Mode (Mouse.Normal);
         when Hidden =>
            Object.Window.Set_Cursor_Mode (Mouse.Hidden);
         when Disabled =>
            Object.Window.Set_Cursor_Mode (Mouse.Disabled);
      end case;
   end Set_Cursor_Mode;

   procedure Set_Button_State
     (Object  : in out GLFW_Pointer_Input;
      Subject : Standard.Glfw.Input.Mouse.Button;
      State   : Standard.Glfw.Input.Button_State)
   is
      use Standard.Glfw.Input;
      use all type Inputs.Pointers.Button;
      use all type Inputs.Pointers.Button_State;

      Pointer_State : constant Pointers.Button_State
        := (case State is
              when Pressed  => Pressed,
              when Released => Released);
   begin
      case Subject is
         when Mouse.Left_Button =>
            Object.Set_Button_State (Left, Pointer_State);
         when Mouse.Right_Button =>
            Object.Set_Button_State (Right, Pointer_State);
         when Mouse.Middle_Button =>
            Object.Set_Button_State (Middle, Pointer_State);
         when others =>
            raise Program_Error with "Invalid mouse button";
      end case;
   end Set_Button_State;

   procedure Set_Window
     (Object  : in out GLFW_Pointer_Input;
      Window  : Standard.Glfw.Windows.Window_Reference) is
   begin
      Object.Window := Window;
   end Set_Window;

   function Create_Pointer_Input return Inputs.Pointers.Pointer_Input_Ptr is
   begin
      return new GLFW_Pointer_Input'
        (Pointers.Default.Abstract_Pointer_Input with others => <>);
   end Create_Pointer_Input;

   -----------------------------------------------------------------------------

   use Inputs.Joysticks;

   procedure Copy_Buttons
     (From :     Standard.Glfw.Input.Joysticks.Joystick_Button_States;
      To   : out Inputs.Joysticks.Button_States)
   is
      use all type Standard.Glfw.Input.Joysticks.Joystick_Button_State;
   begin
      for Index in From'Range loop
         To (Index) := (case From (Index) is
                          when Pressed  => Pressed,
                          when Released => Released);
      end loop;
   end Copy_Buttons;

   procedure Copy_Axes
     (From :     Standard.Glfw.Input.Joysticks.Axis_Positions;
      To   : out Inputs.Joysticks.Axis_Positions) is
   begin
      for Index in From'Range loop
         To (Index) := Axis_Position (From (Index));
      end loop;
   end Copy_Axes;

   procedure Copy_Hats
     (From :     Standard.Glfw.Input.Joysticks.Joystick_Hat_States;
      To   : out Inputs.Joysticks.Hat_States)
   is
      use all type Standard.Glfw.Input.Joysticks.Joystick_Hat_State;
   begin
      for Index in From'Range loop
         To (Index) := (case From (Index) is
                          when Centered   => Centered,
                          when Up         => Up,
                          when Right      => Right,
                          when Right_Up   => Right_Up,
                          when Down       => Down,
                          when Right_Down => Right_Down,
                          when Left       => Left,
                          when Left_Up    => Left_Up,
                          when Left_Down  => Left_Down);
      end loop;
   end Copy_Hats;

   -----------------------------------------------------------------------------

   package SU renames Ada.Strings.Unbounded;

   type Abstract_GLFW_Joystick_Input is abstract
     new Joysticks.Default.Abstract_Joystick_Input with
   record
      Joystick   : Standard.Glfw.Input.Joysticks.Joystick;
      Name, GUID : SU.Unbounded_String;
      Present    : Boolean;
   end record;

   overriding
   function Is_Present (Object : Abstract_GLFW_Joystick_Input) return Boolean is
     (Object.Present and then Object.Joystick.Is_Present);

   overriding
   function Name (Object : Abstract_GLFW_Joystick_Input) return String is
     (SU.To_String (Object.Name));

   overriding
   function GUID (Object : Abstract_GLFW_Joystick_Input) return String is
     (SU.To_String (Object.GUID));

   -----------------------------------------------------------------------------

   type GLFW_Gamepad_Input  is new Abstract_GLFW_Joystick_Input with null record;

   overriding
   function Is_Gamepad (Object : GLFW_Gamepad_Input) return Boolean is (True);

   overriding
   function State (Object : in out GLFW_Gamepad_Input) return Inputs.Joysticks.Joystick_State is
      use type SU.Unbounded_String;
   begin
      if not Object.Joystick.Is_Present then
         Object.Present := False;
         raise Disconnected_Error with Object.Joystick.Index'Image & " is not connected";
      elsif Object.Joystick.Joystick_GUID = Object.GUID then
         Object.Present := True;
      end if;

      declare
         State : constant Standard.Glfw.Input.Joysticks.Joystick_Gamepad_State :=
           Object.Joystick.Gamepad_State;
      begin
         return Result : Joystick_State
           (Button_Count => State.Buttons'Length,
            Axis_Count   => State.Axes'Length,
            Hat_Count    => 0)
         do
            Copy_Buttons (State.Buttons, Result.Buttons);
            Copy_Axes (State.Axes, Result.Axes);

            Inputs.Joysticks.Gamepads.Normalize_Axes (Result.Axes);
         end return;
      end;
   end State;

   -----------------------------------------------------------------------------

   type GLFW_Joystick_Input is new Abstract_GLFW_Joystick_Input with null record;

   overriding
   function Is_Gamepad (Object : GLFW_Joystick_Input) return Boolean is (False);

   overriding
   function State (Object : in out GLFW_Joystick_Input) return Inputs.Joysticks.Joystick_State is
      use type SU.Unbounded_String;
   begin
      if not Object.Joystick.Is_Present then
         Object.Present := False;
         raise Disconnected_Error with Object.Joystick.Index'Image & " is not connected";
      elsif Object.Joystick.Joystick_GUID = Object.GUID then
         Object.Present := True;
      end if;

      declare
         Buttons : constant Standard.Glfw.Input.Joysticks.Joystick_Button_States :=
           Object.Joystick.Button_States;
         Axes : constant Standard.Glfw.Input.Joysticks.Axis_Positions :=
           Object.Joystick.Positions;
         Hats : constant Standard.Glfw.Input.Joysticks.Joystick_Hat_States :=
           Object.Joystick.Hat_States;
      begin
         return Result : Joystick_State
           (Button_Count => Buttons'Length,
            Axis_Count   => Axes'Length,
            Hat_Count    => Hats'Length)
         do
            Copy_Buttons (Buttons, Result.Buttons);
            Copy_Axes (Axes, Result.Axes);
            Copy_Hats (Hats, Result.Hats);
         end return;
      end;
   end State;

   -----------------------------------------------------------------------------

   function Create_Joystick_Input
     (Index : Standard.Glfw.Input.Joysticks.Joystick_Index)
     return Inputs.Joysticks.Joystick_Input_Ptr
   is
      Joystick : constant Standard.Glfw.Input.Joysticks.Joystick :=
        Standard.Glfw.Input.Joysticks.Get_Joystick (Index);
   begin
      if not Joystick.Is_Present then
         raise Disconnected_Error with Index'Image & " is not connected";
      end if;

      if Joystick.Is_Gamepad then
         return new GLFW_Gamepad_Input'
           (Joysticks.Default.Abstract_Joystick_Input with Joystick => Joystick,
            Present  => True,
            Name => SU.To_Unbounded_String (Joystick.Gamepad_Name),
            GUID => SU.To_Unbounded_String (Joystick.Joystick_GUID));
      else
         return new GLFW_Joystick_Input'
           (Joysticks.Default.Abstract_Joystick_Input with Joystick => Joystick,
            Present  => True,
            Name => SU.To_Unbounded_String (Joystick.Joystick_Name),
            GUID => SU.To_Unbounded_String (Joystick.Joystick_GUID));
      end if;
   end Create_Joystick_Input;

   -----------------------------------------------------------------------------

   subtype Slot_Index is Standard.Glfw.Input.Joysticks.Joystick_Index range 1 .. 16;

   type Boolean_Array is array (Slot_Index) of Boolean;

   protected type Joystick_Manager is new Inputs.Joysticks.Joystick_Manager with
      procedure Set_Present
        (Index : Slot_Index;
         Value : Boolean);

      procedure Initialize
        with Post => Is_Initialized;

      function Is_Initialized return Boolean;

      overriding
      procedure Acquire (Joystick : out Inputs.Joysticks.Joystick_Input_Access);

      overriding
      procedure Release (Joystick : Inputs.Joysticks.Joystick_Input_Access);
   private
      Acquired, Present : Boolean_Array := (others => False);
      Initialized : Boolean := False;
   end Joystick_Manager;

   package Joysticks renames Standard.Glfw.Input.Joysticks;

   protected body Joystick_Manager is
      procedure Set_Present
        (Index : Slot_Index;
         Value : Boolean) is
      begin
         Present (Index) := Value;
      end Set_Present;

      procedure Initialize is
      begin
         for Index in Present'Range loop
            Present (Index) := Joysticks.Get_Joystick (Index).Is_Present;
         end loop;

         Initialized := True;
      end Initialize;

      function Is_Initialized return Boolean is (Initialized);

      procedure Acquire (Joystick : out Inputs.Joysticks.Joystick_Input_Access) is
      begin
         for Index in Acquired'Range loop
            if not Acquired (Index) and Present (Index) then
               Joystick := Create_Joystick_Input (Index);
               Acquired (Index) := True;
               return;
            end if;
         end loop;

         Joystick := null;
      end Acquire;

      procedure Release (Joystick : Inputs.Joysticks.Joystick_Input_Access) is
         Index : constant Slot_Index :=
           Abstract_GLFW_Joystick_Input (Joystick.all).Joystick.Index;
      begin
         if not Acquired (Index) then
            raise Program_Error;
         end if;

         Acquired (Index) := False;
      end Release;
   end Joystick_Manager;

   Default_Manager : aliased Joystick_Manager;

   procedure On_Connected
     (Source : Joysticks.Joystick;
      State  : Joysticks.Connect_State)
   is
      use type Joysticks.Connect_State;
   begin
      Default_Manager.Set_Present (Source.Index, State = Joysticks.Connected);
   end On_Connected;

   function Create_Joystick_Manager return Inputs.Joysticks.Joystick_Manager_Ptr is
   begin
      if not Default_Manager.Is_Initialized then
         Default_Manager.Initialize;
         Joysticks.Set_Callback (On_Connected'Access);
      end if;

      return Default_Manager'Access;
   end Create_Joystick_Manager;

end Orka.Inputs.GLFW;
