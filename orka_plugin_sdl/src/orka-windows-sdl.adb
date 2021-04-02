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

with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Events.Mice;
with SDL.Events.Windows;
with SDL.Video.Windows.Makers;

with Orka.Inputs.SDL;
with Orka.Logging;

with GL.Context;
with GL.Viewports;

package body Orka.Windows.SDL is

   use all type Orka.Logging.Source;
   use all type Orka.Logging.Severity;
   use Orka.Logging;

   package Messages is new Orka.Logging.Messages (Window_System);

   overriding
   procedure Finalize (Object : in out SDL_Context) is
   begin
      if Object.Flags.Debug then
         Messages.Log (Debug, "Shutting down SDL");
      end if;

      Standard.SDL.Finalise;
   end Finalize;

   overriding
   procedure Enable (Object : in out SDL_Context; Subject : Contexts.Feature) is
   begin
      Contexts.Enable (Object.Features, Subject);
   end Enable;

   overriding
   function Enabled (Object : SDL_Context; Subject : Contexts.Feature) return Boolean
     is (Contexts.Enabled (Object.Features, Subject));

   overriding
   function Is_Current (Object : SDL_Context; Kind : Orka.Contexts.Task_Kind) return Boolean is
   begin
      raise GL.Feature_Not_Supported_Exception;
      return True;
   end Is_Current;

   overriding
   procedure Make_Current (Object : SDL_Context) is
   begin
      raise GL.Feature_Not_Supported_Exception;
   end Make_Current;

   overriding
   procedure Make_Current
     (Object : SDL_Context;
      Window : in out Orka.Windows.Window'Class)
   is
      package GL      renames Standard.SDL.Video.GL;
      package Windows renames Standard.SDL.Video.Windows;
   begin
      GL.Set_Current (SDL_Window (Window).Context, Windows.Window (Window));
   end Make_Current;

   overriding
   procedure Make_Not_Current (Object : SDL_Context) is
   begin
      raise GL.Feature_Not_Supported_Exception;
      --  TODO Make sure Object is current on calling task
   end Make_Not_Current;

   overriding
   function Version (Object : SDL_Context) return Contexts.Context_Version is
   begin
      return
        (Major => GL.Context.Major_Version,
         Minor => GL.Context.Minor_Version);
   end Version;

   overriding
   function Flags (Object : SDL_Context) return Contexts.Context_Flags is
      Flags : constant GL.Context.Context_Flags := GL.Context.Flags;

      Result : Contexts.Context_Flags;
   begin
      pragma Assert (Flags.Forward_Compatible);

      Result.Debug    := Flags.Debug;
      Result.Robust   := Flags.Robust_Access;
      Result.No_Error := Flags.No_Error;

      return Result;
   end Flags;

   overriding
   function Create_Context
     (Version : Contexts.Context_Version;
      Flags   : Contexts.Context_Flags := (others => False)) return SDL_Context
   is
      package GL renames Standard.SDL.Video.GL;
      use type GL.Flags;

      Context_Flags : GL.Flags := GL.Context_Forward_Compatible;
   begin
      if Flags.Debug then
         Context_Flags := Context_Flags or GL.Context_Debug;
      end if;
      if Flags.Robust then
         Context_Flags := Context_Flags or GL.Context_Robust_Access;
      end if;

      --  Initialize SDL
      if not Standard.SDL.Initialise then
         raise Program_Error with "Initializing SDL failed";
      end if;
      pragma Assert (Standard.SDL.Was_Initialised (Standard.SDL.Enable_Screen));

      Standard.SDL.Video.Disable_Screen_Saver;

      Messages.Log (Debug, "SDL driver: " & Standard.SDL.Video.Current_Driver_Name);

      --  Initialize OpenGL context
      GL.Set_Context_Major_Version (GL.Major_Versions (Version.Major));
      GL.Set_Context_Minor_Version (GL.Minor_Versions (Version.Minor));
      GL.Set_Context_Profile (GL.Core);
      GL.Set_Context_Flags (Context_Flags);

      return (Ada.Finalization.Limited_Controlled with
        Version  => Version,
        Flags    => Flags,
        Features => <>);
   end Create_Context;

   overriding
   procedure Finalize (Object : in out SDL_Window) is
   begin
      if not Object.Finalized then
         Messages.Log (Debug, "Closing SDL window");
         Object.Window.Finalize;
         Object.Finalized := True;
      end if;
   end Finalize;

   overriding
   function Create_Window
     (Context            : Contexts.Surface_Context'Class;
      Width, Height      : Positive;
      Title              : String  := "";
      Samples            : Natural := 0;
      Visible, Resizable : Boolean := True;
      Transparent        : Boolean := False) return SDL_Window
   is
      package SDL_GL      renames Standard.SDL.Video.GL;
      package SDL_Windows renames Standard.SDL.Video.Windows;

      use type SDL_Windows.Window_Flags;
   begin
      return Result : aliased SDL_Window := SDL_Window'(Ada.Finalization.Limited_Controlled
        with Input => Inputs.SDL.Create_Pointer_Input, Finalized => False, others => <>)
      do
         declare
            Reference : SDL_Windows.Window renames Result.Window;

            Position : constant Standard.SDL.Natural_Coordinates
              := (X => SDL_Windows.Centered_Window_Position (0),
                  Y => SDL_Windows.Centered_Window_Position (1));
            Extents  : constant Standard.SDL.Positive_Sizes
              := (Standard.SDL.Positive_Dimension (Width),
                  Standard.SDL.Positive_Dimension (Height));

            Flags : SDL_Windows.Window_Flags := SDL_Windows.OpenGL;
         begin
            if Resizable then
               Flags := Flags or SDL_Windows.Resizable;
            end if;
            if Visible then
               Flags := Flags or SDL_Windows.Shown;
            end if;
            SDL_GL.Set_Multisampling (Samples > 0);
            SDL_GL.Set_Multisampling_Samples (SDL_GL.Multisample_Samples (Samples));

            --  Create window and make GL context current
            SDL_Windows.Makers.Create (Reference, Title, Position, Extents, Flags);
            SDL_GL.Create (Result.Context, Reference);
            pragma Assert (SDL_Windows.Exist);

            Inputs.SDL.SDL_Pointer_Input (Result.Input.all).Set_Window (Reference);

            declare
               Extents : constant Standard.SDL.Sizes := Reference.Get_Size;
            begin
               Result.Width  := Positive (Extents.Width);
               Result.Height := Positive (Extents.Height);

               Messages.Log (Debug, "Created SDL window and GL context");
               Messages.Log (Debug, "  size:      " &
                 Trim (Result.Width'Image) & " × " & Trim (Result.Height'Image));
               Messages.Log (Debug, "  visible:     " & (if Visible then "yes" else "no"));
               Messages.Log (Debug, "  resizable:   " & (if Resizable then "yes" else "no"));
            end;

            SDL_GL.Set_Current (Result.Context, Reference);

            Messages.Log (Debug, "  context:");
            Messages.Log (Debug, "    flags:    " & Orka.Contexts.Image (Context.Flags));
            Messages.Log (Debug, "    version:  " & GL.Context.Version_String);
            Messages.Log (Debug, "    renderer: " & GL.Context.Renderer);

            GL.Viewports.Set_Clipping (GL.Viewports.Lower_Left, GL.Viewports.Zero_To_One);
            Result.Vertex_Array.Create;
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
   function Should_Close (Object : SDL_Window) return Boolean is
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
      Object.Scroll_X := 0.0;
      Object.Scroll_Y := 0.0;

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
               --  Accumulate the offset in case multiple events are processed
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
   procedure Set_Vertical_Sync (Object : in out SDL_Window; Enable : Boolean) is
      use all type Standard.SDL.Video.GL.Swap_Intervals;
   begin
      if not Standard.SDL.Video.GL.Set_Swap_Interval
        ((if Enable then Synchronised else Not_Synchronised), Late_Swap_Tear => True)
      then
         Messages.Log (Debug,
           (if Enable then "Enabling" else "Disabling") & " vertical sync failed");
      end if;
   end Set_Vertical_Sync;

end Orka.Windows.SDL;
