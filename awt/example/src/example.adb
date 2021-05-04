with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Text_IO;

with AWT.Clipboard;
with AWT.Drag_And_Drop;
with AWT.Inputs;
with AWT.Monitors;
with AWT.Windows;

with Orka.Contexts.AWT;
with Orka.Debug;

with Package_Test;

procedure Example is
   use Ada.Text_IO;

   Index : Positive := 1;
   Monitor_Events, Last_Monitor_Events : Positive := 1;
   Border_Size : constant := 50;
   Should_Be_Visible : Boolean := True;
   Visible_Index : Positive := 1;

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
begin
   Put_Line ("Initializing...");
   AWT.Initialize;
   Put_Line ("Initialized");

   for Monitor of AWT.Monitors.Monitors loop
      Print_Monitor (Monitor);
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
            Put_Line (Ada.Exceptions.Exception_Information (E));
            Context.Make_Not_Current;
            raise;
      end Render_Task;
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
         end;
      end loop;
      Put_Line ("Exited event loop");
   exception
      when E : others =>
         Put_Line ("main: " & Ada.Exceptions.Exception_Information (E));
         raise;
   end;
end Example;
