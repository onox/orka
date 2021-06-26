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

with Interfaces.C;

with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;

with Orka.Logging;
with Orka.OS;
with Orka.Strings;

with Event_Device.Force_Feedbacks;
with Inotify;

with Wayland;

with AWT.Inputs.Gamepads.Mappings;
with AWT.OS;
with AWT.Registry;

package body AWT.Inputs.Gamepads is

   use all type Orka.Logging.Source;
   use all type Orka.Logging.Severity;
   use Orka.Logging;

   package Messages is new Orka.Logging.Messages (Window_System);

   package SF renames Ada.Strings.Fixed;
   package ED renames Event_Device;

   subtype Non_Hat_Button is Gamepad_Button range Action_Down .. Gamepad_Button'Last;

   use all type ED.Key_Kind;
   use all type ED.Absolute_Axis_Kind;
   use all type AWT.OS.Entry_Kind;
   use AWT.OS.Paths;

   Hidraw_Folder : constant String := "/sys/class/hidraw";
   Input_Folder  : constant String := "/dev/input";

   Last_Brightness : constant := 255.0;

   Gamepad_Button_To_Key : constant array (Non_Hat_Button) of ED.Key_Kind :=
     (Action_Down    => Button_South,
      Action_Right   => Button_East,
      Action_Left    => Button_West,
      Action_Up      => Button_North,
      Shoulder_Left  => Button_Trigger_Left_1,
      Shoulder_Right => Button_Trigger_Right_1,
      Thumb_Left     => Button_Thumb_Left,
      Thumb_Right    => Button_Thumb_Right,
      Center_Left    => Button_Select,
      Center_Right   => Button_Start,
      Center_Logo    => Button_Mode);
   --  FIXME Support Button_DPad_*

   Gamepad_Axis_To_Axis : constant array (Gamepad_Axis) of ED.Absolute_Axis_Kind :=
     (Stick_Left_X  => X,
      Stick_Left_Y  => Y,
      Stick_Right_X => Rx,
      Stick_Right_Y => Ry);

   Gamepad_Trigger_To_Axis : constant array (Gamepad_Trigger) of ED.Absolute_Axis_Kind :=
     (Trigger_Left  => Z,
      Trigger_Right => Rz);

   Axis_To_Sensor_Axis : constant array (AWT.Gamepads.Sensor_Axis) of Sensor_Axis :=
     (X  => X,
      Y  => Y,
      Z  => Z,
      Rx => Rx,
      Ry => Ry,
      Rz => Rz);

   function Read_File (File : AWT.OS.File) return String is
   begin
      declare
         Bytes : constant Ada.Streams.Stream_Element_Array := File.Read;

         subtype Bytes_String is String (1 .. Bytes'Length);

         function Convert is new Ada.Unchecked_Conversion
           (Source => Ada.Streams.Stream_Element_Array, Target => Bytes_String);
      begin
         return Convert (Bytes);
      end;
   exception
      when Constraint_Error =>
         return "";
   end Read_File;

   function Find_HID_Device (ID : String) return String is
      Filter_Link : constant AWT.OS.Filter_Type := (Symbolic_Link => True, others => False);
   begin
      for File of AWT.OS.Scan_Directory (Hidraw_Folder, Filter_Link) loop
         declare
            Name : constant String := +File.Name;
         begin
            if SF.Index (Name, "hidraw") = Name'First then
               declare
                  Hidraw_Path : constant String := Hidraw_Folder / Name / "device";
                  UEvent_File : AWT.OS.File := AWT.OS.Open (Hidraw_Path / "uevent");
               begin
                  if UEvent_File.Is_Open then
                     declare
                        Lines : constant Orka.Strings.String_List :=
                          Orka.Strings.Split (Read_File (UEvent_File), "" & L1.LF);
                     begin
                        UEvent_File.Close;

                        if (for some Line of Lines => (+Line) = "HID_UNIQ=" & ID) then
                           return Hidraw_Path;
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;
      end loop;

      return "";
   end Find_HID_Device;

   procedure Apply_Default_Mapping (Object : in out Gamepad) is
   begin
      for GP_Axis in Gamepad_Axis'Range loop
         Object.Axes (Gamepad_Axis_To_Axis (GP_Axis)) :=
           (Kind   => Axis,
            Output => (Kind => Axis, Axis => GP_Axis, others => <>),
            others => <>);
      end loop;

      for GP_Trigger in Gamepad_Trigger'Range loop
         Object.Axes (Gamepad_Trigger_To_Axis (GP_Trigger)) :=
           (Kind   => Trigger,
            Output => (Kind => Trigger, Trigger => GP_Trigger),
            others => <>);
      end loop;

      for GP_Button in Non_Hat_Button'Range loop
         Object.Keys (Gamepad_Button_To_Key (GP_Button)) :=
           (Kind   => Button,
            Output => (Kind => Button, Button => GP_Button),
            others => <>);
      end loop;
   end Apply_Default_Mapping;

   procedure Apply_Mapping (Object : in out Gamepad; Line : String) is
      Features_Axes : constant ED.Absolute_Axis_Features := Object.Device.Features;
      Features_Keys : constant ED.Key_Features           := Object.Device.Features;

      function Name_To_Axis (Number : String) return AWT.Gamepads.Input_Axis is
         Index : constant Natural := Natural'Value (Number);

         Count : Natural := 0;
      begin
         for Axis in AWT.Gamepads.Input_Axis'Range loop
            if Features_Axes (Axis) and Axis not in Hat_0X .. Hat_3Y then
               if Count = Index then
                  return Axis;
               end if;
               Count := Count + 1;
            end if;
         end loop;
         raise Constraint_Error with "Unknown axis index " & Index'Image;
      end Name_To_Axis;

      type Hat_Mask is record
         Axis : AWT.Gamepads.Input_Hat;
         Side : Side_Type;
      end record;

      function Name_To_Hat (Number : String) return Hat_Mask is
         Index_Dot : constant Natural := SF.Index (Number, ".");

         Index : constant Natural := Natural'Value (Number (Number'First .. Index_Dot - 1));
         Mask  : constant Natural := Natural'Value (Number (Index_Dot + 1 .. Number'Last));

         Count : Natural := 0;
      begin
         for Axis in AWT.Gamepads.Input_Hat'Range loop
            if Features_Axes (Axis) then
               declare
                  Is_Horizontal : constant Boolean := Axis in Hat_0X | Hat_1X | Hat_2X | Hat_3X;
               begin
                  if Index = Count then
                     --  Up
                     if Mask = 1 and not Is_Horizontal then
                        return (Axis => Axis, Side => Negative_Half);
                     --  Right
                     elsif Mask = 2 and Is_Horizontal then
                        return (Axis => Axis, Side => Positive_Half);
                     --  Down
                     elsif Mask = 4 and not Is_Horizontal then
                        return (Axis => Axis, Side => Positive_Half);
                     --  Left
                     elsif Mask = 8 and Is_Horizontal then
                        return (Axis => Axis, Side => Negative_Half);
                     end if;
                  end if;

                  if not Is_Horizontal then
                     Count := Count + 1;
                  end if;
               end;
            end if;
         end loop;
         raise Constraint_Error with
           "Unknown hat index " & Index'Image & " or mask " & Mask'Image;
      end Name_To_Hat;

      function Name_To_Button (Number : String) return AWT.Gamepads.Input_Button is
         Index : constant Natural := Natural'Value (Number);

         Count : Natural := 0;
      begin
         for Key in AWT.Gamepads.Input_Button'Range loop
            if Features_Keys (Key) then
               if Count = Index then
                  return Key;
               end if;
               Count := Count + 1;
            end if;
         end loop;
         raise Constraint_Error with "Unknown button index " & Index'Image;
      end Name_To_Button;

      Index : constant Natural := SF.Index (Line, ",");
      pragma Assert (Index > 0);

      Name  : constant String := Line (Line'First .. Index - 1);
      Value : constant String := Line (Index + 1 .. Line'Last);

      Button_Mappings : constant Orka.Strings.String_List := Orka.Strings.Split (Value, ",");
   begin
      for Mapping of Button_Mappings loop
         declare
            Button_Input : constant Orka.Strings.String_List := Orka.Strings.Split (+Mapping, ":");
            pragma Assert (Button_Input'Length = 2);

            Button : constant String := (+Button_Input (1));
            Input  : constant String := (+Button_Input (2));

            Index : Natural := Button'First;
            Last  : Natural := Input'Last;

            Output_Positive : Boolean := False;
            Output_Negative : Boolean := False;

            Input_Positive : Boolean := False;
            Input_Negative : Boolean := False;
            Input_Invert   : Boolean := False;

            Input_Map  : Input_Mapping;
            Output_Map : Output_Mapping;
         begin
            if Button (Index) = '+' then
               Output_Positive := True;
               Index := Index + 1;
            elsif Button (Index) = '-' then
               Output_Negative := True;
               Index := Index + 1;
            end if;

            Output_Map := Mappings.Name_To_Output (Button (Index .. Button'Last));

            if Output_Map.Kind = Axis then
               if Output_Positive then
                  Output_Map.Scale := 1.0;
                  Output_Map.Offset := 0.0;
               elsif Output_Negative then
                  Output_Map.Scale := -1.0;
                  Output_Map.Offset := 0.0;
               end if;
            end if;

            Index := Input'First;

            if Input (Index) in '+' then
               Input_Positive := True;
               Index := Index + 1;
            elsif Input (Index) in '-' then
               Input_Negative := True;
               Index := Index + 1;
            end if;

            if Input (Last) in '~' then
               Input_Invert := True;
               Last := Last - 1;
            end if;

            if Input (Index) = 'a' then
               declare
                  Axis : constant AWT.Gamepads.Input_Axis :=
                    Name_To_Axis (Input (Index + 1 .. Last));
               begin
                  if Input_Positive then
                     Input_Map.Side := Positive_Half;
                  elsif Input_Negative then
                     Input_Map.Side := Negative_Half;
                  end if;
                  if Input_Invert then
                     Input_Map.Invert := True;
                  end if;

                  Object.Axes (Axis) :=
                    (Kind => Output_Map.Kind, Input => Input_Map, Output => Output_Map);
               end;
            elsif Input (Index) = 'b' then
               declare
                  Key : constant ED.Key_Kind := Name_To_Button (Input (Index + 1 .. Last));
               begin
                  Object.Keys (Key) :=
                    (Kind => Output_Map.Kind, Input => Input_Map, Output => Output_Map);
               end;
            elsif Input (Index) = 'h' then
               declare
                  Hat : constant Hat_Mask := Name_To_Hat (Input (Index + 1 .. Last));
               begin
                  Object.Axes (Hat.Axis) :=
                    (Kind => Output_Map.Kind, Input => Input_Map, Output => Output_Map);
                  --  For hats, Output of Object.Hats is used

                  Object.Hats (Hat.Axis, Hat.Side) :=
                    (Kind => Output_Map.Kind, Input => Input_Map, Output => Output_Map);
               end;
            else
               raise Constraint_Error;
            end if;
         end;
      end loop;

      Object.Name := +Name;
   end Apply_Mapping;

   function Info_To_Modifier (Info : ED.Axis_Info) return Axis_Modifier is
     (Offset     => Axis_Value (Info.Minimum),
      Scale      => 1.0 / Axis_Value (Info.Maximum - Info.Minimum),
      Resolution => 1.0 / Axis_Value'Max (1.0, Axis_Value (Info.Resolution)));

   procedure Set_Motion_Sensor_Modifiers (Object : in out Gamepad) is
      Features : constant ED.Absolute_Axis_Features := Object.Sensor_Device.Features;
   begin
      for Axis in AWT.Gamepads.Sensor_Axis loop
         if Features (Axis) then
            declare
               Info : constant ED.Axis_Info := Object.Sensor_Device.Axis (Axis);
            begin
               Object.Sensors (Axis) := Info_To_Modifier (Info);
               Object.Sensor_Data.Absolute (Axis) := Info.Value;
            end;
         end if;
      end loop;
   end Set_Motion_Sensor_Modifiers;

   function Initialize (Object : in out Gamepad; Path : String) return Boolean is
   begin
      pragma Assert (not Object.Device.Is_Open);

      if not Object.Device.Open (Path) then
         return False;
      end if;

      Object.Name := +Object.Device.Name;
      Object.ID   := +Object.Device.Unique_ID;
      Object.GUID := GUID_String (ED.GUID (Object.Device.ID));

      declare
         Events : constant ED.Device_Events := Object.Device.Events;
      begin
         if not (Events.Synchronization and Events.Absolute_Axes and Events.Keys) then
            Object.Device.Close;
            return False;
         end if;

         declare
            Features : constant ED.Key_Features := Object.Device.Features;
         begin
            --  Gamepads must have Button_South according to kernel documentation
            if not Features (Button_South) then
               Object.Device.Close;
               return False;
            end if;
         end;

         declare
            Key_Statuses : constant ED.Key_Features := Object.Device.Key_Statuses;
         begin
            for Button in Non_Hat_Button'Range loop
               declare
                  Key : constant ED.Key_Kind := Gamepad_Button_To_Key (Button);
               begin
                  Object.Data.Keys (Key) :=
                    (if Key_Statuses (Key) then ED.Pressed else ED.Released);
               end;
            end loop;
         end;

         Object.Has_Rumble  := Events.Force_Feedback;
         Object.Max_Effects := Object.Device.Force_Feedback_Effects;

         if Object.Has_Rumble then
            Object.Device.Set_Force_Feedback_Gain (1.0);
         end if;
      end;

      declare
         Features : constant ED.Absolute_Axis_Features := Object.Device.Features;
      begin
         if not (for all Axis in Gamepad_Axis => Features (Gamepad_Axis_To_Axis (Axis))) then
            Object.Device.Close;
            return False;
         end if;

         if not (for all Axis in Gamepad_Trigger => Features (Gamepad_Trigger_To_Axis (Axis))) then
            Object.Device.Close;
            return False;
         end if;

         if Mappings.Contains (Object.GUID) then
            Object.Apply_Mapping (Mappings.Get (Object.GUID));
         else
            Object.Apply_Default_Mapping;
         end if;

         for Axis in AWT.Gamepads.Input_Axis loop
            if Features (Axis) then
               declare
                  Info : constant ED.Axis_Info := Object.Device.Axis (Axis);
               begin
                  Object.Axes (Axis).Input.Modifier := Info_To_Modifier (Info);
                  Object.Data.Absolute (Axis) := Info.Value;
               end;
            end if;
         end loop;
      end;

      Object.Initialized := True;
      Object.Path := +Path;

      declare
         Filter_Dir  : constant AWT.OS.Filter_Type := (Directory => True, others => False);

         function Is_Color (Name : SU.Unbounded_String; Color : String) return Boolean is
           (SU.Index (Name, ":" & Color) = SU.Length (Name) - Color'Length);

         HID_Path : constant String := Find_HID_Device (Object.Serial_Number);
      begin
         if HID_Path /= "" then
            for File of AWT.OS.Scan_Directory (HID_Path / "power_supply", Filter_Dir) loop
               if +File.Name not in "." | ".." then
                  Object.Battery := +(HID_Path / "power_supply" / (+File.Name));
               end if;
            end loop;

            for File of AWT.OS.Scan_Directory (HID_Path / "leds", Filter_Dir) loop
               if +File.Name not in "." | ".." then
                  if Is_Color (File.Name, "red") then
                     Object.LED_Red := +(HID_Path / "leds" / (+File.Name));
                  elsif Is_Color (File.Name, "green") then
                     Object.LED_Green := +(HID_Path / "leds" / (+File.Name));
                  elsif Is_Color (File.Name, "blue") then
                     Object.LED_Blue := +(HID_Path / "leds" / (+File.Name));
                  end if;
               end if;
            end loop;

            Find_Motion_Input_Event :
            for Input_File of AWT.OS.Scan_Directory (HID_Path / "input", Filter_Dir) loop
               for File of AWT.OS.Scan_Directory
                 (HID_Path / "input" / (+Input_File.Name), Filter_Dir)
               loop
                  if SU.Index (File.Name, "event") = 1 then
                     declare
                        Sensor_Path : constant String := Input_Folder / (+File.Name);
                     begin
                        if Sensor_Path /= Path
                          and then Object.Sensor_Device.Open (Sensor_Path)
                        then
                           if not Object.Sensor_Device.Properties.Accelerometer then
                              Object.Sensor_Device.Close;
                           else
                              Object.Set_Motion_Sensor_Modifiers;
                              exit Find_Motion_Input_Event;
                           end if;
                        end if;
                     end;
                  end if;
               end loop;
            end loop Find_Motion_Input_Event;
         end if;
      end;

      return True;
   end Initialize;

   function Name (Object : Gamepad) return String is (+Object.Name);

   function Serial_Number (Object : Gamepad) return String is (+Object.ID);

   function GUID (Object : Gamepad) return GUID_String is (Object.GUID);

   function Connection (Object : Gamepad) return Connection_Kind is
      function Uses_USB (Location : String) return Boolean is
        (SF.Index (Source => Location, Pattern => "usb") > 0);
   begin
      if Object.Initialized and Object.Device.Is_Open then
         declare
            Location : constant String := Object.Device.Location;
         begin
            if Location'Length > 0 then
               return (if Uses_USB (Location) then Wired else Wireless);
            end if;
         end;
      end if;

      return Disconnected;
   end Connection;

   procedure Set_State
     (Object : in out Gamepad;
      State  : Gamepad_State)
   is
      function Convert is new Ada.Unchecked_Conversion
        (Source => Gamepad_Buttons,
         Target => Changed_Gamepad_Buttons);

      use type Changed_Gamepad_Buttons;

      Old_State : constant Gamepad_State := Object.Gamepad;

      Old_State_Buttons : constant Changed_Gamepad_Buttons := Convert (Old_State.Buttons);
      New_State_Buttons : constant Changed_Gamepad_Buttons := Convert (State.Buttons);

      Changed : constant Changed_Gamepad_Buttons := Old_State_Buttons xor New_State_Buttons;
   begin
      Object.Gamepad := State;
      Object.Gamepad.Pressed  := Old_State.Pressed  or (Changed and     New_State_Buttons);
      Object.Gamepad.Released := Old_State.Released or (Changed and not New_State_Buttons);
   end Set_State;

   procedure Set_State
     (Object : in out Gamepad;
      State  : Motion_State) is
   begin
      Object.Sensor := State;
   end Set_State;

   procedure Poll_State_Gamepad (Object : in out Gamepad) is
      State : Gamepad_State;

      procedure Set_Value (Value : Axis_Value; Output : Output_Mapping) is
      begin
         case Output.Kind is
            when Axis =>
               State.Axes (Output.Axis) :=
                 Axis_Position (Axis_Value'(Value * Output.Scale - Output.Offset));
            when Trigger =>
               State.Triggers (Output.Trigger) := Trigger_Position (Value);
            when Button =>
               State.Buttons (Output.Button) := (if Value >= 0.5 then Pressed else Released);
            when None =>
               raise Program_Error;
         end case;
      end Set_Value;

      Device : ED.Input_Device renames Object.Device;
   begin
      if not Device.Is_Open or else not Device.Read (Object.Data) then
         Object.Set_State (State);
         return;
      end if;

      for Absolute_Axis in Object.Axes'Range loop
         declare
            Map : Mapping renames Object.Axes (Absolute_Axis);
         begin
            if Map.Kind /= None then
               declare
                  Modifier : Axis_Modifier renames Map.Input.Modifier;
                  Value : Axis_Value := Axis_Value (Object.Data.Absolute (Absolute_Axis));

                  Valid : Boolean := True;
               begin
                  Value := Value * Modifier.Resolution;
                  Value := (Value - Modifier.Offset) * Modifier.Scale;

                  case Map.Input.Side is
                     when Positive_Half =>
                        if Value in 0.5 .. 1.0 then
                           Value := (Value - 0.5) * 2.0;
                        else
                           Valid := False;
                        end if;
                     when Negative_Half =>
                        if Value in 0.0 .. 0.5 then
                           Value := Value * 2.0;
                        else
                           Valid := False;
                        end if;
                     when Full_Range =>
                        null;
                  end case;

                  if Map.Input.Invert then
                     Value := 1.0 - Value;
                  end if;

                  if Valid then
                     if Absolute_Axis in AWT.Gamepads.Input_Hat then
                        if Value in 0.5 .. 1.0 then
                           Value := (Value - 0.5) * 2.0;

                           declare
                              Map : Mapping renames Object.Hats (Absolute_Axis, Positive_Half);
                           begin
                              Set_Value (Value, Map.Output);
                           end;
                        else
                           Value := 1.0 - Value * 2.0;

                           declare
                              Map : Mapping renames Object.Hats (Absolute_Axis, Negative_Half);
                           begin
                              Set_Value (Value, Map.Output);
                           end;
                        end if;
                     else
                        Set_Value (Value, Map.Output);
                     end if;
                  end if;
               end;
            end if;
         end;
      end loop;

      for Key in Object.Keys'Range loop
         declare
            Map : Mapping renames Object.Keys (Key);
         begin
            if Map.Kind /= None then
               declare
                  use type ED.Key_State;
                  Is_Pressed : constant Boolean := Object.Data.Keys (Key) = ED.Pressed;
               begin
                  Set_Value (Axis_Value (if Is_Pressed then 1.0 else 0.0), Map.Output);
               end;
            end if;
         end;
      end loop;

      Object.Set_State (State);
   end Poll_State_Gamepad;

   procedure Poll_State_Sensor (Object : in out Gamepad) is
      Device : ED.Input_Device renames Object.Sensor_Device;
   begin
      if not Device.Is_Open or else not Device.Read (Object.Sensor_Data) then
         Object.Set_State (Motion_State'(Is_Present => False));
         return;
      end if;

      declare
         State : Motion_State (Is_Present => True);
      begin
         for Absolute_Axis in Object.Sensors'Range loop
            declare
               Modifier : Axis_Modifier renames Object.Sensors (Absolute_Axis);
               Value : Axis_Value := Axis_Value (Object.Sensor_Data.Absolute (Absolute_Axis));
            begin
               Value := Value * Modifier.Resolution;
               State.Axes (Axis_To_Sensor_Axis (Absolute_Axis)) := Sensor_Axis_Value (Value);
            end;
         end loop;

         --  TODO Numerical Integration for orientation and position?

         Object.Set_State (State);
      end;
   end Poll_State_Sensor;

   procedure Poll_State (Object : in out Gamepad) is
   begin
      Object.Poll_State_Gamepad;
      Object.Poll_State_Sensor;
   end Poll_State;

   function State (Object : in out Gamepad) return Gamepad_State is
   begin
      return Result : constant Gamepad_State := Object.Gamepad do
         --  Reset Pressed and Released after the state has been read by
         --  the application. New state will be accumulated in procedure Set_State
         Object.Gamepad.Pressed  := (others => False);
         Object.Gamepad.Released := (others => False);
      end return;
   end State;

   function State (Object : Gamepad) return Motion_State is
   begin
      return Object.Sensor;
   end State;

   function State (Object : Gamepad) return Battery_State is
      type Pair is record
         Key, Value : SU.Unbounded_String;
      end record;

      function Get (Source : String) return Pair is
         Index : constant Natural := SF.Index (Source, "=");
      begin
         return
           (Key   => +Source (Source'First .. Index - 1),
            Value => +Source (Index + 1 .. Source'Last));
      end Get;

      Present  : Boolean := False;
      Capacity : Battery_Capacity := Battery_Capacity'First;
      Status   : Battery_Status   := Battery_Status'First;

      UEvent_File : AWT.OS.File := AWT.OS.Open ((+Object.Battery) / "uevent");
   begin
      if UEvent_File.Is_Open then
         declare
            Lines : constant Orka.Strings.String_List :=
              Orka.Strings.Split (Read_File (UEvent_File), "" & L1.LF);
         begin
            UEvent_File.Close;

            for Line of Lines loop
               declare
                  Key_Value : constant Pair := Get (+Line);

                  use type SU.Unbounded_String;
               begin
                  if Key_Value.Key = "POWER_SUPPLY_PRESENT" then
                     Present := True;
                  elsif Key_Value.Key = "POWER_SUPPLY_CAPACITY" then
                     Capacity := Battery_Capacity'Value (+Key_Value.Value);
                  elsif Key_Value.Key = "POWER_SUPPLY_STATUS" then
                     if +Key_Value.Value in "Full" | "Not charging" then
                        Status := Not_Charging;
                     elsif +Key_Value.Value in "Charging" then
                        Status := Charging;
                     elsif +Key_Value.Value in  "Discharging" | "Unknown" then
                        Status := Discharging;
                     end if;
                  end if;
               end;
            end loop;
         end;
      end if;

      return Result : Battery_State (Present) do
         if Present then
            Result.Capacity := Capacity;
            Result.Status   := Status;
         end if;
      end return;
   end State;

   function State (Object : Gamepad) return LED_State is
      use type SU.Unbounded_String;

      R_File : AWT.OS.File := AWT.OS.Open ((+Object.LED_Red) / "brightness");
      G_File : AWT.OS.File := AWT.OS.Open ((+Object.LED_Green) / "brightness");
      B_File : AWT.OS.File := AWT.OS.Open ((+Object.LED_Blue) / "brightness");

      R_Text, G_Text, B_Text : SU.Unbounded_String;
   begin
      if R_File.Is_Open then
         R_Text := +Orka.Strings.Strip_Line_Term (Read_File (R_File));
         R_File.Close;
      end if;

      if G_File.Is_Open then
         G_Text := +Orka.Strings.Strip_Line_Term (Read_File (G_File));
         G_File.Close;
      end if;

      if B_File.Is_Open then
         B_Text := +Orka.Strings.Strip_Line_Term (Read_File (B_File));
         B_File.Close;
      end if;

      return Result : LED_State (R_Text /= "" and G_Text /= "" and B_Text /= "") do
         if Result.Is_Present then
            declare
               R : constant Float := Float'Value (+R_Text);
               G : constant Float := Float'Value (+G_Text);
               B : constant Float := Float'Value (+B_Text);

               Maximum_Brightness : constant Float := Float'Max (Float'Max (R, G), B);
            begin
               Result.Brightness := Normalized (Maximum_Brightness / Last_Brightness);
               if Maximum_Brightness > 0.0 then
                  Result.Color :=
                    (Red   => Normalized (R / Maximum_Brightness),
                     Green => Normalized (G / Maximum_Brightness),
                     Blue  => Normalized (B / Maximum_Brightness));
               else
                  Result.Color := (0.0, 0.0, 0.0);
               end if;
            end;
         end if;
      end return;
   end State;

   procedure Set_LED
     (Object     : in out Gamepad;
      Brightness : Normalized;
      Color      : RGB_Color)
   is
      Maximum_Color : constant Normalized :=
        Normalized'Max (Normalized'Max (Color (Red), Color (Green)), Color (Blue));

      Max_Brightness : constant Normalized :=
        (if Maximum_Color > 0.0 then 1.0 / Maximum_Color else 0.0) * Brightness;

      function Image (Value : Normalized) return String is
         Text : constant String := Natural'Image (Natural (Float (Value) * Last_Brightness));
      begin
         return Text (Text'First + 1 .. Text'Last);
      end Image;

      R_File : AWT.OS.File := AWT.OS.Open ((+Object.LED_Red) / "brightness", AWT.OS.Write);
      G_File : AWT.OS.File := AWT.OS.Open ((+Object.LED_Green) / "brightness", AWT.OS.Write);
      B_File : AWT.OS.File := AWT.OS.Open ((+Object.LED_Blue) / "brightness", AWT.OS.Write);
   begin
      if R_File.Is_Open then
         R_File.Write (Image (Color (Red) * Max_Brightness));
         R_File.Close;
      end if;

      if G_File.Is_Open then
         G_File.Write (Image (Color (Green) * Max_Brightness));
         G_File.Close;
      end if;

      if B_File.Is_Open then
         B_File.Write (Image (Color (Blue) * Max_Brightness));
         B_File.Close;
      end if;
   end Set_LED;

   procedure Log_Information (Gamepad : AWT.Inputs.Gamepads.Gamepad'Class) is separate;

   ----------------------------------------------------------------------------

   All_Effects : AWT.Gamepads.Effect_Vectors.Vector;

   All_Gamepads : array (1 .. 16) of aliased Gamepad;

   Input_Notifier : Inotify.Instance;

   type Gamepad_Event_Listener_Ptr is access all Gamepad_Event_Listener'Class;

   Gamepad_Listener : Gamepad_Event_Listener_Ptr;

   function Get_ID (Path : String) return String is
      Device : ED.Input_Device;
   begin
      if not Device.Open (Path) then
         return "";
      end if;

      return Result : constant String := Device.Unique_ID do
         Device.Close;
      end return;
   end Get_ID;

   procedure Process_Events is
      procedure Handle_Events
        (Subject      : Inotify.Watch;
         Event        : Inotify.Event_Kind;
         Is_Directory : Boolean;
         Path         : String)
      is
         use type SU.Unbounded_String;
         use all type Inotify.Event_Kind;

         Name : String renames Path (Path'First - 1 + Input_Folder'Length + 2 .. Path'Last);
      begin
         if Is_Directory or else SF.Index (Name, "event") = 0 then
            return;
         end if;

         case Event is
            when Created | Metadata =>
               --  Metadata event may be fired multiple times during the lifetime
               --  of the file, and may also be fired just before the Deleted event
               if (for some Gamepad of All_Gamepads =>
                     Gamepad.Initialized and Gamepad.Path = Path)
               then
                  return;
               end if;

               declare
                  ID : constant String := Get_ID (Path);
               begin
                  if ID = "" then
                     return;
                  end if;

                  --  Find original gamepad object
                  for Gamepad of All_Gamepads loop
                     if not Gamepad.Initialized and Gamepad.ID = ID then
                        if Gamepad.Initialize (Path) then
                           if Gamepad_Listener /= null then
                              Gamepad_Listener.On_Connect (Gamepad'Access);
                           end if;
                        end if;
                        return;
                     end if;
                  end loop;

                  --  Find an unused slot
                  for Gamepad of All_Gamepads loop
                     if not Gamepad.Initialized then
                        if Gamepad.Initialize (Path) then
                           if Gamepad_Listener /= null then
                              Gamepad_Listener.On_Connect (Gamepad'Access);
                           end if;
                        end if;
                        return;
                     end if;
                  end loop;
               end;
            when Deleted =>
               for Gamepad of All_Gamepads loop
                  if Gamepad.Initialized and Gamepad.Path = Path then
                     Gamepad.Initialized := False;
                     if Gamepad_Listener /= null then
                        Gamepad_Listener.On_Disconnect (Gamepad'Access);
                     end if;
                     if Gamepad.Sensor_Device.Is_Open then
                        Gamepad.Sensor_Device.Close;
                     end if;
                     Gamepad.Device.Close;
                     return;
                  end if;
               end loop;
            when others =>
               raise Program_Error;
         end case;
      end Handle_Events;
   begin
      Input_Notifier.Process_Events (Handle_Events'Access);
   end Process_Events;

   ----------------------------------------------------------------------------

   procedure Set_Mappings (Text : String) is
   begin
      Mappings.Set_Mappings (Mappings.Linux, Text);
   end Set_Mappings;

   procedure Initialize is
      Filter_File : constant AWT.OS.Filter_Type := (Device_File => True, others => False);

      Index : Positive := All_Gamepads'First;
   begin
      pragma Assert (for all Gamepad of All_Gamepads => not Gamepad.Initialized);
      pragma Assert (for all Gamepad of All_Gamepads => not Gamepad.Device.Is_Open);

      AWT.Registry.Gamepad_Notify_FD := Wayland.File_Descriptor (Input_Notifier.File_Descriptor);
      AWT.Registry.Gamepad_Notify_Callback := Process_Events'Access;

      Input_Notifier.Add_Watch
        (Path => Input_Folder,
         Mask => (Metadata | Created | Deleted => True, others => False));

      for File of AWT.OS.Scan_Directory (Input_Folder, Filter_File) loop
         declare
            Name : constant String := SU.To_String (File.Name);
         begin
            if SF.Index (Name, "event") = Name'First then
               if All_Gamepads (Index).Initialize (Input_Folder / Name) then
                  Index := Index + 1;
               end if;
            end if;
         end;
      end loop;
   end Initialize;

   procedure Poll is
   begin
      for Gamepad of All_Gamepads loop
         if Gamepad.Initialized then
            Gamepad.Poll_State;
         end if;
      end loop;
   end Poll;

   function Gamepads return Gamepad_Array is
      Count : Natural  := 0;

      First_Gamepad : constant Gamepad_Ptr :=
        All_Gamepads (All_Gamepads'First)'Access;
   begin
      for Gamepad of All_Gamepads loop
         if Gamepad.Initialized then
            Count := Count + 1;
         end if;
      end loop;

      --  Initialize Result because Gamepad_Ptr has a "not null" constraint
      return Result : Gamepad_Array (1 .. Count) := (others => First_Gamepad) do
         declare
            Index : Positive := Result'First;
         begin
            for Gamepad of All_Gamepads loop
               if Gamepad.Initialized then
                  Result (Index) := Gamepad'Access;
                  Index := Index + 1;
               end if;
            end loop;
            pragma Assert (Index - 1 = Count);
         end;
      end return;
   end Gamepads;

   ----------------------------------------------------------------------------

   package FF renames ED.Force_Feedbacks;

   use all type FF.Direction_Kind;
   use all type FF.Force_Feedback_Effect_Kind;
   use type Event_Device.Force_Feedback_Effect_ID;

   subtype Unsigned_16 is Event_Device.Unsigned_16;
   subtype short is Interfaces.C.short;

   function Rumble_Effect
     (Length, Offset : Duration;
      Strong, Weak   : Normalized) return FF.Force_Feedback_Effect
   is (Kind      => Rumble,
       ID        => -1,
       Direction => Down,
       Trigger   => (Button => 0, Interval => FF.From_Duration (0.0)),
       Replay    => (Length      => FF.From_Duration (Length),
                     Start_Delay => FF.From_Duration (Offset)),
       Effect =>
         (Kind => Rumble,
          Rumble_Effect =>
            (Strong_Magnitude => Unsigned_16 (Strong * Normalized'Base (Unsigned_16'Last)),
             Weak_Magnitude   => Unsigned_16 (Weak   * Normalized'Base (Unsigned_16'Last)))));

   function Periodic_Effect
     (Length, Offset : Duration;
      Magnitude      : Normalized;
      Attack, Fade   : Duration) return FF.Force_Feedback_Effect
   is (Kind      => Periodic,
       ID        => -1,
       Direction => Down,
       Trigger   => (Button => 0, Interval => FF.From_Duration (0.0)),
       Replay    => (Length      => FF.From_Duration (Length),
                     Start_Delay => FF.From_Duration (Offset)),
       Effect =>
         (Kind => Periodic,
          Periodic_Effect => (Waveform  => FF.Sine,
                              Period    => FF.From_Duration (1.0),
                              Magnitude => short (Magnitude * Normalized'Base (short'Last)),
                              Offset    => 0,
                              Phase     => 0,
                              Envelope  => (Attack_Length => FF.From_Duration (Attack),
                                            Attack_Level  => 0,
                                            Fade_Length   => FF.From_Duration (Fade),
                                            Fade_Level    => 0),
                              others => <>)));

   function Rumble_Effect
     (Length, Offset : Duration;
      Strong, Weak   : Normalized) return Effect is
   begin
      All_Effects.Append (Rumble_Effect (Length, Offset, Strong, Weak));
      return (Cursor => All_Effects.Last);
   end Rumble_Effect;

   function Periodic_Effect
     (Length, Offset : Duration;
      Magnitude      : Normalized;
      Attack, Fade   : Duration) return Effect is
   begin
      All_Effects.Append (Periodic_Effect (Length, Offset, Magnitude, Attack, Fade));
      return (Cursor => All_Effects.Last);
   end Periodic_Effect;

   procedure Play_Uploaded_Effect (Object : in out Gamepad; Subject : Effect; Count : Natural) is
      use type AWT.Gamepads.Effect_Vectors.Cursor;
   begin
      for Effect of Object.Effects loop
         if Effect.Cursor = Subject.Cursor then
            if Object.Device.Play_Force_Feedback_Effect (Effect.Effect.ID, Count) then
               Effect.Stop_At := Orka.OS.Monotonic_Clock +
                 (if Count > 0 then Count * FF.To_Duration (Effect.Effect.Replay.Length) else 0.0);
            else
               Messages.Log (Error,
                 "Failed to play " & Effect.Effect.Kind'Image & " force-feedback effect");
            end if;
            exit;
         end if;
      end loop;
   end Play_Uploaded_Effect;

   function Sort_Stop (Left, Right : AWT.Gamepads.Uploaded_Effect) return Boolean is
     (Left.Stop_At < Right.Stop_At);

   package Effect_Sorting is new AWT.Gamepads.Uploaded_Effect_Vectors.Generic_Sorting (Sort_Stop);
   --  The effect with the earliest stop time needs to be removed if the
   --  application tries to play more effects than the device can support

   procedure Play_Effect (Object : in out Gamepad; Subject : Effect) is
      Uploaded_OK : Boolean := True;

      procedure Upload_Effect (Element : in out AWT.Gamepads.Uploaded_Effect) is
      begin
         pragma Assert (Element.Effect.ID = -1);

         FF.Upload_Force_Feedback_Effect (Object.Device, Element.Effect);
         Uploaded_OK := Element.Effect.ID /= -1;

         if not Uploaded_OK then
            Messages.Log (Error,
              "Failed to upload " & Element.Effect.Kind'Image & " force-feedback effect");
         end if;
      end Upload_Effect;

      use type AWT.Gamepads.Effect_Vectors.Cursor;
   begin
      --  If the maximum number of effects have been uploaded to the
      --  device, remove the effect with the earliest stop time. It may
      --  or may not be the case that the stop time is already in the past.
      if Object.Max_Effects = Natural (Object.Effects.Length) then
         Effect_Sorting.Sort (Object.Effects);
         FF.Remove_Force_Feedback_Effect (Object.Device, Object.Effects.First_Element.Effect.ID);
         Object.Effects.Delete_First;
      end if;

      if not (for some Effect of Object.Effects => Effect.Cursor = Subject.Cursor) then
         Object.Effects.Append
           ((Stop_At => 0.0,
             Cursor  => Subject.Cursor,
             Effect  => AWT.Gamepads.Effect_Vectors.Element (Subject.Cursor)));

         Object.Effects.Update_Element (Object.Effects.Last, Upload_Effect'Access);

         if not Uploaded_OK then
            Object.Effects.Delete_Last;
            return;
         end if;
      end if;

      Object.Play_Uploaded_Effect (Subject, 1);
   end Play_Effect;

   procedure Cancel_Effect (Object : in out Gamepad; Subject : Effect) is
   begin
      Object.Play_Uploaded_Effect (Subject, 0);
   end Cancel_Effect;

   function Effects (Object : Gamepad) return Natural is (Object.Max_Effects);

   ----------------------------------------------------------------------------

   overriding procedure Initialize (Object : in out Gamepad_Event_Listener) is
      use type Wayland.File_Descriptor;
   begin
      if Gamepad_Listener /= null then
         raise Program_Error;
      end if;

      Gamepad_Listener := Object'Unchecked_Access;
   end Initialize;

   overriding procedure Finalize (Object : in out Gamepad_Event_Listener) is
      use type Wayland.File_Descriptor;
   begin
      if Gamepad_Listener /= null
        and Gamepad_Listener /= Object'Unchecked_Access
      then
         raise Program_Error;
      end if;

      Gamepad_Listener := null;
   end Finalize;

end AWT.Inputs.Gamepads;
