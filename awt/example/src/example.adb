with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Text_IO;

with AWT.Clipboard;
with AWT.Drag_And_Drop;
with AWT.Inputs.Gamepads;
with AWT.Monitors;
with AWT.Windows;

with Orka.Contexts.AWT;
with Orka.Debug;
with Orka.Resources.Locations.Directories;

with Package_Test;

procedure Example is
   use Ada.Text_IO;

   Index : Positive := 1;
   Monitor_Events, Last_Monitor_Events : Positive := 1;
   Border_Size : constant := 50;
   Should_Be_Visible : Boolean := True;
   Visible_Index : Positive := 1;

   Print_Axes_And_Triggers : constant Boolean := False;

   subtype Normalized is AWT.Inputs.Gamepads.Normalized;

   Color      : AWT.Inputs.Gamepads.RGB_Color := (others => 0.0);
   Brightness : Normalized := 0.0;

   type Extra_Info_Kind is (Info_Motion, Info_Battery, Info_LED);

   Extra_Info : Extra_Info_Kind := Extra_Info_Kind'First;

   Visible_Index_Count : constant := 100;

   procedure Print_Monitor (Monitor : AWT.Monitors.Monitor_Ptr) is
      State : constant AWT.Monitors.Monitor_State := Monitor.State;

      use AWT.Monitors;
   begin
      Put_Line ("monitor:" & Monitor.ID'Image);
      Put_Line ("      name: " & (+State.Name));
      Put_Line ("      x, y: " & State.X'Image & ", " & State.Y'Image);
      Put_Line ("      w x h: " & State.Width'Image & " × " & State.Height'Image);
      Put_Line ("      refresh: " & State.Refresh'Image);
   end Print_Monitor;

   type Test_Listener is new AWT.Monitors.Monitor_Event_Listener with null record;

   overriding procedure On_Connect    (Object : Test_Listener; Monitor : AWT.Monitors.Monitor_Ptr);
   overriding procedure On_Disconnect (Object : Test_Listener; Monitor : AWT.Monitors.Monitor_Ptr);

   overriding
   procedure On_Connect
     (Object  : Test_Listener;
      Monitor : AWT.Monitors.Monitor_Ptr) is
   begin
      Put_Line ("connected:");
      Print_Monitor (Monitor);
      Monitor_Events := Monitor_Events + 1;
   end On_Connect;

   overriding
   procedure On_Disconnect
     (Object  : Test_Listener;
      Monitor : AWT.Monitors.Monitor_Ptr) is
   begin
      Put_Line ("disconnected:");
      Print_Monitor (Monitor);
      Monitor_Events := Monitor_Events + 1;
   end On_Disconnect;

   Monitor_Listener : Test_Listener;

   type Print_Gamepad_Listener is new AWT.Inputs.Gamepads.Gamepad_Event_Listener with null record;

   overriding
   procedure On_Connect
     (Object  : Print_Gamepad_Listener;
      Gamepad : AWT.Inputs.Gamepads.Gamepad_Ptr);

   overriding
   procedure On_Disconnect
     (Object  : Print_Gamepad_Listener;
      Gamepad : AWT.Inputs.Gamepads.Gamepad_Ptr);

   overriding
   procedure On_Connect
     (Object  : Print_Gamepad_Listener;
      Gamepad : AWT.Inputs.Gamepads.Gamepad_Ptr) is
   begin
      Gamepad.Log_Information;
   end On_Connect;

   overriding
   procedure On_Disconnect
     (Object  : Print_Gamepad_Listener;
      Gamepad : AWT.Inputs.Gamepads.Gamepad_Ptr) is
   begin
      Gamepad.Log_Information;
   end On_Disconnect;

   Test_Print_Gamepad_Listener : Print_Gamepad_Listener;

   Effect_1 : constant AWT.Inputs.Gamepads.Effect :=
     AWT.Inputs.Gamepads.Rumble_Effect (0.15, 0.0, 0.7, 1.0);

   Effect_2 : constant AWT.Inputs.Gamepads.Effect :=
     AWT.Inputs.Gamepads.Periodic_Effect (8.0, 0.0, 0.8, 3.0, 2.0);

   Location_Data : constant Orka.Resources.Locations.Location_Ptr :=
     (Orka.Resources.Locations.Directories.Create_Location ("./"));

   use all type AWT.Inputs.Gamepads.Connection_Kind;
begin
   Put_Line ("Initializing...");
   AWT.Initialize;
   Put_Line ("Initialized");

   for Monitor of AWT.Monitors.Monitors loop
      Print_Monitor (Monitor);
   end loop;

   if Location_Data.Exists ("gamecontrollerdb.txt") then
      AWT.Inputs.Gamepads.Set_Mappings
        (Orka.Resources.Convert (Location_Data.Read_Data ("gamecontrollerdb.txt").Get));
      Put_Line ("Mappings added");
   end if;
   AWT.Inputs.Gamepads.Initialize;

   AWT.Inputs.Gamepads.Poll;
   for Gamepad of AWT.Inputs.Gamepads.Gamepads loop
      Gamepad.Log_Information;
   end loop;

   declare
      Next_Cursor : AWT.Inputs.Cursors.Pointer_Cursor :=
        AWT.Inputs.Cursors.Pointer_Cursor'First;

      Last_Pointer  : AWT.Inputs.Pointer_State;

      use Ada.Real_Time;

      Interval : constant Duration := To_Duration (Microseconds (16_667));
      Flip_Size : Boolean := False;

      Context : constant Orka.Contexts.Surface_Context'Class :=
        Orka.Contexts.AWT.Create_Context
          (Version => (4, 2),
           Flags   => (Debug | Robust => True, others => False));

      Window : Package_Test.Test_Window := Package_Test.Create_Window
        (Context, 600, 400, Visible => True, Transparent => True, Title => "init test");
      AWT_Window : AWT.Windows.Window'Class renames AWT.Windows.Window'Class (Window);

      task Render_Task is
         entry Start_Rendering;
      end Render_Task;

      task body Render_Task is
      begin
         Put_Line ("Render task waiting to get started...");
         accept Start_Rendering;
         Put_Line ("Render task started");
         Context.Make_Current (Window);
         Put_Line ("Window made current on context");

         Orka.Debug.Set_Log_Messages (Enable => True);
         Put_Line ("Context version: " & Orka.Contexts.Image (Context.Version));

         Window.Post_Initialize;

         Put_Line ("Rendering...");
         loop
            exit when Window.Should_Close;
            delay until Clock + Microseconds (15000);
            Window.Render;
         end loop;
         Put_Line ("Rendering done");
         Context.Make_Not_Current;
         Put_Line ("Render task, context made not current");
      exception
         when E : others =>
            Put_Line ("Error render task: " & Ada.Exceptions.Exception_Information (E));
            Context.Make_Not_Current;
            raise;
      end Render_Task;

      task Poll_Joysticks;

      task body Poll_Joysticks is
         Poll_Interval : constant Time_Span := Milliseconds (4);

         Next_Time : Time := Clock + Poll_Interval;
      begin
         Put_Line ("Polling joysticks...");
         loop
            exit when Window.Should_Close;

            AWT.Inputs.Gamepads.Poll;

            delay until Next_Time;
            Next_Time := Next_Time + Poll_Interval;
         end loop;
         Put_Line ("Polling done");
      exception
         when E : others =>
            Put_Line ("Error joystick task: " & Ada.Exceptions.Exception_Information (E));
            raise;
      end Poll_Joysticks;
   begin
      Last_Pointer  := AWT_Window.State;

      Context.Make_Not_Current;
      Put_Line ("Context made not current in main task");
      Render_Task.Start_Rendering;
      Put_Line ("Render task started by main task");

      Window.Set_Margin (Border_Size);
      Put_Line ("Starting event loop...");
      while not Window.Should_Close loop
         AWT.Process_Events (Interval);
--         AWT_Window.Set_Title (Index'Image & " " & Next_Cursor'Image);
         Index := Index + 1;

         select
            Package_Test.Dnd_Signal.Wait;

            declare
               Result : constant String := AWT.Drag_And_Drop.Get;
            begin
               Put_Line ("value: '" & Result & "'");
               AWT.Drag_And_Drop.Finish (AWT.Inputs.Copy);
            end;
         else
            null;
         end select;

         if not Should_Be_Visible  then
            Put_Line (Positive'Image (Visible_Index + Visible_Index_Count) & Index'Image);
            if Index > Visible_Index + Visible_Index_Count then
               Should_Be_Visible := True;
               AWT_Window.Set_Title ("visible! " & Visible_Index'Image);
               AWT_Window.Set_Visible (True);
               Put_Line ("window visible");
            end if;
         end if;

--         if Index = 100 then
--            AWT_Window.Set_Visible (True);
--         end if;

         if Monitor_Events /= Last_Monitor_Events then
            Put_Line ("Monitor count: " & Natural'Image (AWT.Monitors.Monitors'Length));
            Last_Monitor_Events := Monitor_Events;
         end if;

         declare
            Pointer : constant AWT.Inputs.Pointer_State := AWT_Window.State;

            use all type AWT.Inputs.Button_State;
            use all type AWT.Inputs.Dimension;
            use all type AWT.Inputs.Pointer_Button;
            use all type AWT.Inputs.Pointer_Mode;
            use type AWT.Inputs.Cursors.Pointer_Cursor;
         begin
--            if False then
--               Put_Line ("focused:   " & Pointer.Focused'Image);
--               Put_Line ("scrolling: " & Pointer.Scrolling'Image);
--               Put_Line ("buttons:");
--               Put_Line ("    left: " & Pointer.Buttons (Left)'Image);
--               Put_Line ("  middle: " & Pointer.Buttons (Middle)'Image);
--               Put_Line ("   right: " & Pointer.Buttons (Right)'Image);
--               Put_Line ("mode: " & Pointer.Mode'Image);
--               Put_Line ("position: " & Pointer.Position (X)'Image & Pointer.Position (Y)'Image);
--               Put_Line ("relative: " & Pointer.Relative (X)'Image & Pointer.Relative (Y)'Image);
--               Put_Line ("scroll: " & Pointer.Scroll (X)'Image & Pointer.Scroll (Y)'Image);
--            end if;

--            if Last_Pointer.Focused and not Pointer.Focused then
--               Window.Set_Pointer_Cursor (AWT.Inputs.Cursors.Pointer_Cursor'Last);
--            end if;

            if Pointer.Buttons (Left) = Pressed
              and Last_Pointer.Buttons (Left) = Released
            then
               Next_Cursor :=
                 (if Next_Cursor = AWT.Inputs.Cursors.Pointer_Cursor'Last then
                    AWT.Inputs.Cursors.Pointer_Cursor'First
                  else
                    AWT.Inputs.Cursors.Pointer_Cursor'Succ (Next_Cursor));
               AWT_Window.Set_Pointer_Cursor (Next_Cursor);
            end if;

            if Pointer.Buttons (Right) = Pressed
              and Last_Pointer.Buttons (Right) = Released
              and Pointer.Mode /= Locked
            then
               Put_Line ("locking!");
               AWT_Window.Set_Pointer_Mode (Locked);
            elsif Pointer.Buttons (Right) = Released
              and Last_Pointer.Buttons (Right) = Pressed
              and Pointer.Mode = Locked
            then
               Put_Line ("unlocking!");
               AWT_Window.Set_Pointer_Mode (Visible);
            end if;

            Last_Pointer := Pointer;
         end;

         declare
            Keyboard : constant AWT.Inputs.Keyboard_State := AWT_Window.State;

            use type AWT.Inputs.Keyboard_Modifiers;
            use all type AWT.Inputs.Button_State;
            use all type AWT.Inputs.Keyboard_Button;
         begin
            if Keyboard.Pressed (Key_Escape) then
               Window.Close;
            end if;

            if Keyboard.Modifiers.Ctrl and Keyboard.Pressed (Key_C) then
               declare
                  Value : constant String := "foobar" & Index'Image;
               begin
                  AWT.Clipboard.Set (Value);
                  Put_Line ("Set clipboard: '" & Value & "'");
               end;
            end if;

            if Keyboard.Modifiers.Ctrl and Keyboard.Pressed (Key_V) then
               Put_Line ("Get clipboard: '" & AWT.Clipboard.Get & "'");
            end if;

--            Put_Line ("focused: " & Keyboard.Focused'Image);
--            Put_Line ("repeat:");
--            Put_Line ("  rate:  " & Keyboard.Repeat_Rate'Image);
--            Put_Line ("  delay: " & Keyboard.Repeat_Delay'Image);
--            if False and Keyboard.Modifiers /= Last_Keyboard.Modifiers then
--               Put_Line ("Shift:       " & Keyboard.Modifiers.Shift'Image);
--               Put_Line ("Caps_Lock:   " & Keyboard.Modifiers.Caps_Lock'Image);
--               Put_Line ("Ctrl:        " & Keyboard.Modifiers.Ctrl'Image);
--               Put_Line ("Alt:         " & Keyboard.Modifiers.Alt'Image);
--               Put_Line ("Num_Lock:    " & Keyboard.Modifiers.Num_Lock'Image);
--               Put_Line ("Logo:        " & Keyboard.Modifiers.Logo'Image);
--            end if;

            declare
               use all type AWT.Windows.Size_Mode;
            begin
               if Keyboard.Modifiers.Ctrl and Keyboard.Pressed (Key_F) then
                  if AWT_Window.State.Mode = Fullscreen then
                     AWT_Window.Set_Size_Mode (Default);
                  else
                     AWT_Window.Set_Size_Mode (Fullscreen);
                  end if;
               end if;

               if Keyboard.Modifiers.Ctrl and Keyboard.Pressed (Key_M) then
                  if AWT_Window.State.Mode = Maximized then
                     AWT_Window.Set_Size_Mode (Default);
                  else
                     AWT_Window.Set_Size_Mode (Maximized);
                  end if;
               end if;

               if Keyboard.Modifiers.Ctrl and Keyboard.Pressed (Key_S) then
                  Flip_Size := not Flip_Size;
                  if Flip_Size then
                     AWT_Window.Set_Size (1280, 720);
                  else
                     AWT_Window.Set_Size (600, 400);
                  end if;
               end if;

               if Keyboard.Modifiers.Ctrl and Keyboard.Pressed (Key_H) then
                  Should_Be_Visible := False;
                  Visible_Index := Index;
                  AWT_Window.Set_Visible (False);
                  Put_Line ("window hidden");
               end if;
            end;

            declare
               Gamepads : constant AWT.Inputs.Gamepads.Gamepad_Array :=
                 AWT.Inputs.Gamepads.Gamepads;

               Title : AWT.SU.Unbounded_String;
            begin
               if Gamepads'Length > 0 then
                  declare
                     Gamepad : AWT.Inputs.Gamepads.Gamepad_Ptr renames Gamepads (1);

                     State   : constant AWT.Inputs.Gamepads.Gamepad_State := Gamepad.State;
                     Motion  : constant AWT.Inputs.Gamepads.Motion_State  := Gamepad.State;
                     Battery : constant AWT.Inputs.Gamepads.Battery_State := Gamepad.State;
                     LED     : constant AWT.Inputs.Gamepads.LED_State     := Gamepad.State;

                     use all type AWT.Inputs.Gamepads.Gamepad_Button;
                     use all type AWT.Inputs.Gamepads.Gamepad_Trigger;
                     use all type AWT.Inputs.Gamepads.Color_Kind;
                     use type Normalized;

                     Show_Extra_Info : constant Boolean := State.Buttons (Center_Right) /= Pressed;
                  begin
                     AWT.SU.Append (Title,
                       "serial: " & Gamepad.Serial_Number &
                       " connected? " & Gamepad.Connection'Image &
                       " (" & Natural'Image (Gamepads'Length) & " gamepads) ");

                     if State.Pressed (Shoulder_Right) then
                        Gamepad.Play_Effect (Effect_1);
                     elsif State.Released (Shoulder_Right) then
                        Gamepad.Cancel_Effect (Effect_1);
                     end if;

                     if State.Pressed (Shoulder_Left) then
                        Gamepad.Play_Effect (Effect_2);
                     elsif State.Released (Shoulder_Left) then
                        Gamepad.Cancel_Effect (Effect_2);
                     end if;

                     if State.Pressed (Center_Right) then
                        Extra_Info := (if Extra_Info = Extra_Info_Kind'Last then
                                         Extra_Info_Kind'First
                                       else
                                         Extra_Info_Kind'Succ (Extra_Info));
                     end if;

                     --  Toggle LED red when pressing button B
                     if State.Pressed (Action_Right) then
                        Color (Red) := 1.0 - Color (Red);
                     end if;

                     --  Toggle LED green when pressing button Y
                     if State.Pressed (Action_Up) then
                        Color (Green) := 1.0 - Color (Green);
                     end if;

                     --  Toggle LED blue when pressing button A
                     if State.Pressed (Action_Down) then
                        Color (Blue) := 1.0 - Color (Blue);
                     end if;

                     --  Set brightness with left trigger while holding button X
                     if State.Buttons (Action_Left) = Pressed then
                        Brightness := Normalized (State.Triggers (Trigger_Left));
                     end if;

                     Gamepad.Set_LED (Brightness, Color);

                     for Button in State.Buttons'Range loop
                        if State.Pressed (Button) then
--                           pragma Assert (State.Buttons (Button) = Pressed);
                           Put_Line ("pressed " & Button'Image);
                        end if;

                        if State.Released (Button) then
--                           pragma Assert (State.Buttons (Button) = Released);
                           Put_Line ("released " & Button'Image);
                        end if;

                        if State.Buttons (Button) = Pressed then
                           AWT.SU.Append (Title, " " & Button'Image);
                        end if;
                     end loop;

                     if Show_Extra_Info then
                        if Extra_Info = Info_Motion and Motion.Is_Present then
                           AWT.SU.Append (Title, "motion:");
                           for Axis in Motion.Axes'Range loop
                              AWT.SU.Append (Title, " " & Motion.Axes (Axis)'Image);
                           end loop;
                        end if;

                        if Extra_Info = Info_Battery and Battery.Is_Present then
                           AWT.SU.Append (Title, "battery: " & Battery.Capacity'Image &
                             " (" & Battery.Status'Image & ")");
                        end if;

                        if Extra_Info = Info_LED and LED.Is_Present then
                           AWT.SU.Append (Title, "L: " & LED.Brightness'Image & " color: " &
                             LED.Color (Red)'Image &
                             LED.Color (Green)'Image &
                             LED.Color (Blue)'Image);
                        end if;
                     end if;

                     if Print_Axes_And_Triggers then
                        for Axis in State.Axes'Range loop
                           Put (Axis'Image & ": " & State.Axes (Axis)'Image & " ");
                        end loop;
                        for Trigger in State.Triggers'Range loop
                           Put (Trigger'Image & ": " & State.Triggers (Trigger)'Image & " ");
                        end loop;
                        New_Line;
                     end if;
                  end;
               end if;

               if Gamepads'Length > 0 then
                  AWT_Window.Set_Title (AWT.SU.To_String (Title));
               else
                  AWT_Window.Set_Title ("No gamepads");
               end if;
            end;
         end;
      end loop;
      Put_Line ("Exited event loop");
   exception
      when E : others =>
         Put_Line ("Error in main task: " & Ada.Exceptions.Exception_Information (E));
         raise;
   end;
end Example;
