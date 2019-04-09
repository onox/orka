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

with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Events.Mice;
with SDL.Events.Windows;
with SDL.Video.Windows.Makers;

with Orka.Inputs.SDL;
with Orka.Logging;

package body Orka.Windows.SDL is

   use Orka.Logging;
   package Messages is new Orka.Logging.Messages (Other);

   function Initialize
     (Major, Minor : Natural;
      Debug : Boolean := False) return Active_SDL'Class
   is
      package GL renames Standard.SDL.Video.GL;
      use type GL.Flags;

      Flags : GL.Flags := GL.Context_Forward_Compatible;
   begin
      if Debug then
         Flags := Flags or GL.Context_Debug;
      end if;

      --  Initialize SDL
      if not Standard.SDL.Initialise then
         raise Program_Error with "Initializing SDL failed";
      end if;
      pragma Assert (Standard.SDL.Was_Initialised (Standard.SDL.Enable_Screen));

      Standard.SDL.Video.Disable_Screen_Saver;

      Messages.Insert (Logging.Debug, "SDL driver: " & Standard.SDL.Video.Current_Driver_Name);
      Messages.Insert (Logging.Debug, "SDL OpenGL context: " &
        Trim (Major'Image) & "." & Trim (Minor'Image));

      --  Initialize OpenGL context
      GL.Set_Context_Major_Version (GL.Major_Versions (Major));
      GL.Set_Context_Minor_Version (GL.Minor_Versions (Minor));
      GL.Set_Context_Profile (GL.Core);
      GL.Set_Context_Flags (Flags);

      return Active_SDL'(Ada.Finalization.Limited_Controlled
        with Debug => Debug, Finalized => False);
   end Initialize;

   overriding
   procedure Finalize (Object : in out Active_SDL) is
   begin
      if not Object.Finalized then
         if Object.Debug then
            Messages.Insert (Debug, "Shutting down SDL");
         end if;
         Standard.SDL.Finalise;
         Object.Finalized := True;
      end if;
   end Finalize;

   overriding
   procedure Finalize (Object : in out SDL_Window) is
   begin
      if not Object.Finalized then
         Messages.Insert (Debug, "Closing SDL window");
         Object.Window.Finalize;
         Object.Finalized := True;
      end if;
   end Finalize;

   function Create_Window
     (Width, Height : Positive;
      Samples : Natural := 0;
      Visible, Resizable : Boolean := True) return Window'Class
   is
      package Windows renames Standard.SDL.Video.Windows;
      package GL renames Standard.SDL.Video.GL;
      use type Windows.Window_Flags;
   begin
      return Result : aliased SDL_Window := SDL_Window'(Ada.Finalization.Limited_Controlled
        with Input => Inputs.SDL.Create_Pointer_Input, Finalized => False, others => <>)
      do
         declare
            Reference : Windows.Window renames Result.Window;

            Position : constant Standard.SDL.Natural_Coordinates
              := (X => Windows.Centered_Window_Position (0),
                  Y => Windows.Centered_Window_Position (1));
            Extents  : constant Standard.SDL.Positive_Sizes
              := (Standard.SDL.Positive_Dimension (Width),
                  Standard.SDL.Positive_Dimension (Height));
            Flags : Windows.Window_Flags := Windows.OpenGL;
         begin
            if Resizable then
               Flags := Flags or Windows.Resizable;
            end if;
            if Visible then
               Flags := Flags or Windows.Shown;
            end if;
            GL.Set_Multisampling (Samples > 0);
            GL.Set_Multisampling_Samples (GL.Multisample_Samples (Samples));

            -- Create window and make GL context current
            Windows.Makers.Create (Reference, "", Position, Extents, Flags);
            GL.Create (Result.Context, Reference);
            pragma Assert (Windows.Exist);

            Inputs.SDL.SDL_Pointer_Input (Result.Input.all).Set_Window (Reference);

            declare
               Extents : constant Standard.SDL.Sizes := Reference.Get_Size;
            begin
               Result.Width  := Positive (Extents.Width);
               Result.Height := Positive (Extents.Height);

               Messages.Insert (Debug, "Created SDL window and GL context");
               Messages.Insert (Debug, "  size:      " &
                 Trim (Result.Width'Image) & " x " & Trim (Result.Height'Image));
               Messages.Insert (Debug, "  visible:   " & (if Visible then "yes" else "no"));
               Messages.Insert (Debug, "  resizable: " & (if Resizable then "yes" else "no"));
            end;
         end;
      end return;
   end Create_Window;

   overriding
   function Pointer_Input
     (Object : SDL_Window) return Inputs.Pointers.Pointer_Input_Ptr
   is (Object.Input);

   overriding
   function Width (Object : SDL_Window) return Positive is
     (Object.Width);

   overriding
   function Height (Object : SDL_Window) return Positive is
     (Object.Height);

   overriding
   procedure Set_Title (Object : in out SDL_Window; Value : String) is
   begin
      Object.Window.Set_Title (Value);
   end Set_Title;

   overriding
   procedure Close (Object : in out SDL_Window) is
   begin
      Object.Close_Window := True;
   end Close;

   overriding
   function Should_Close (Object : in out SDL_Window) return Boolean is
   begin
      return Object.Close_Window;
   end Should_Close;

   overriding
   procedure Process_Input (Object : in out SDL_Window) is
      package Events renames Standard.SDL.Events;

      Event : Events.Events.Events;
      Quit  : Boolean := False;

      use type Events.Event_Types;
      use type Events.Keyboards.Key_Codes;
      use type GL.Types.Double;
   begin
      while Events.Events.Poll (Event) loop
         case Event.Common.Event_Type is
            when Events.Quit =>
               Object.Close;
               Quit := True;
            when Events.Keyboards.Key_Down =>
               --  TODO Add Button_Input object
               if Event.Keyboard.Key_Sym.Key_Code = Events.Keyboards.Code_Escape then
                  Object.Close;
                  Quit := True;
               end if;
            when Events.Keyboards.Key_Up =>
               null;
            when Events.Mice.Motion =>
               Object.Position_X := GL.Types.Double (Event.Mouse_Motion.X);
               Object.Position_Y := GL.Types.Double (Event.Mouse_Motion.Y);
            when Events.Mice.Wheel =>
               Object.Scroll_X := Object.Scroll_X + GL.Types.Double (Event.Mouse_Wheel.X);
               Object.Scroll_Y := Object.Scroll_Y + GL.Types.Double (Event.Mouse_Wheel.Y);
            when Events.Mice.Button_Down =>
               Inputs.SDL.SDL_Pointer_Input (Object.Input.all).Set_Button_State
                 (Event.Mouse_Button.Button, Inputs.Pointers.Pressed);
            when Events.Mice.Button_Up =>
               Inputs.SDL.SDL_Pointer_Input (Object.Input.all).Set_Button_State
                 (Event.Mouse_Button.Button, Inputs.Pointers.Released);
            when Events.Windows.Window =>
               case Event.Window.Event_ID is
                  when Events.Windows.Resized | Events.Windows.Size_Changed =>
                     Object.Width  := Integer (Event.Window.Data_1);
                     Object.Height := Integer (Event.Window.Data_2);
                  when Events.Windows.Close =>
                     Object.Close;
                     Quit := True;
                  when others =>
                     --  Ignore other window events
                     null;
               end case;
            when others =>
               --  Ignore other events
               null;
         end case;

         exit when Quit;
      end loop;

      --  Update position of mouse
      Inputs.SDL.SDL_Pointer_Input (Object.Input.all).Set_Position
        (Object.Position_X, Object.Position_Y);

      --  Update scroll offset of mouse
      Inputs.SDL.SDL_Pointer_Input (Object.Input.all).Set_Scroll_Offset
        (Object.Scroll_X, Object.Scroll_Y);
   end Process_Input;

   overriding
   procedure Swap_Buffers (Object : in out SDL_Window) is
   begin
      Standard.SDL.Video.GL.Swap (Object.Window);
   end Swap_Buffers;

   overriding
   procedure Enable_Vertical_Sync (Object : in out SDL_Window; Enable : Boolean) is
      use all type Standard.SDL.Video.GL.Swap_Intervals;
   begin
      if not Standard.SDL.Video.GL.Set_Swap_Interval
        ((if Enable then Synchronised else Not_Synchronised), Late_Swap_Tear => True)
      then
         Messages.Insert (Debug,
           (if Enable then "Enabling" else "Disabling") & " vertical sync failed");
      end if;
   end Enable_Vertical_Sync;

end Orka.Windows.SDL;
