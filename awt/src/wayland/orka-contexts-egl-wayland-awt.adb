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

with Ada.Strings.Fixed;

with Orka.Logging;
with Orka.Terminals;

with EGL.Objects.Configs;
with EGL.Objects.Surfaces;

with Wayland.Protocols.Client.AWT;

package body Orka.Contexts.EGL.Wayland.AWT is

   use all type Orka.Logging.Source;
   use all type Orka.Logging.Severity;
   use Orka.Logging;

   package Messages is new Orka.Logging.Messages (Window_System);

   procedure Print_Monitor (Monitor : Standard.AWT.Monitors.Monitor'Class) is
      State : constant Standard.AWT.Monitors.Monitor_State := Monitor.State;

      use Standard.AWT.Monitors;
   begin
      Messages.Log (Debug, "Window visible on monitor " & (+State.Name));
      Messages.Log (Debug, "  offset:     " &
        Trim (State.X'Image) & ", " & Trim (State.Y'Image));
      Messages.Log (Debug, "  size:       " &
        Trim (State.Width'Image) & " × " & Trim (State.Height'Image));
      Messages.Log (Debug, "  refresh: " & Orka.Terminals.Image (State.Refresh));
   end Print_Monitor;

   ----------------------------------------------------------------------------

   overriding
   function Width (Object : AWT_Window) return Positive is
      State : Standard.AWT.Windows.Framebuffer_State renames Object.State;
   begin
      return State.Width;
   end Width;

   overriding
   function Height (Object : AWT_Window) return Positive is
      State : Standard.AWT.Windows.Framebuffer_State renames Object.State;
   begin
      return State.Height;
   end Height;

   ----------------------------------------------------------------------------

   overriding
   procedure On_Move
     (Object   : in out AWT_Window;
      Monitor  : Standard.AWT.Monitors.Monitor'Class;
      Presence : Standard.AWT.Windows.Monitor_Presence)
   is
      use all type Standard.AWT.Windows.Monitor_Presence;
      use Standard.AWT.Monitors;
   begin
      case Presence is
         when Entered =>
            Print_Monitor (Monitor);
         when Left =>
            Messages.Log (Debug, "Window no longer visible on monitor " & (+Monitor.State.Name));
      end case;
   end On_Move;

   ----------------------------------------------------------------------------

   overriding
   procedure Make_Current
     (Object : AWT_Context;
      Window : in out Orka.Windows.Window'Class) is
   begin
      if Window not in AWT_Window'Class then
         raise Constraint_Error;
      end if;

      AWT_Window (Window).Make_Current (Object.Context);
   end Make_Current;

   overriding
   function Create_Context
     (Version : Orka.Contexts.Context_Version;
      Flags   : Orka.Contexts.Context_Flags := (others => False)) return AWT_Context is
   begin
      if not Standard.AWT.Is_Initialized then
         Standard.AWT.Initialize;
      end if;

      return Create_Context
        (Standard.Wayland.Protocols.Client.AWT.Get_Display (Standard.AWT.Wayland.Get_Display.all),
         Version, Flags);
   end Create_Context;

   overriding
   function Create_Window
     (Context            : Orka.Contexts.Surface_Context'Class;
      Width, Height      : Positive;
      Title              : String  := "";
      Samples            : Natural := 0;
      Visible, Resizable : Boolean := True;
      Transparent        : Boolean := False) return AWT_Window
   is
      package EGL_Configs  renames Standard.EGL.Objects.Configs;
      package EGL_Surfaces renames Standard.EGL.Objects.Surfaces;

      use all type Standard.EGL.Objects.Contexts.Buffer_Kind;
      use type EGL_Configs.Config;

      package SF renames Ada.Strings.Fixed;

      Object : AWT_Context renames AWT_Context (Context);
   begin
      return Result : AWT_Window do
         declare
            Configs : constant EGL_Configs.Config_Array :=
              EGL_Configs.Get_Configs
                (Object.Context.Display,
                 8, 8, 8, (if Transparent then 8 else 0),
                 24, 8, EGL_Configs.Sample_Size (Samples));

            Used_Config : EGL_Configs.Config renames Configs (Configs'First);
         begin
            Messages.Log (Debug, "EGL configs: (" & Trim (Natural'Image (Configs'Length)) & ")");
            Messages.Log (Debug, "    Re Gr Bl Al De St Ms");
            Messages.Log (Debug, "    --------------------");
            for Config of Configs loop
               declare
                  State : constant EGL_Configs.Config_State := Config.State;
               begin
                  Messages.Log (Debug, "  - " &
                    SF.Tail (Trim (State.Red'Image), 2) & " " &
                    SF.Tail (Trim (State.Green'Image), 2) & " " &
                    SF.Tail (Trim (State.Blue'Image), 2) & " " &
                    SF.Tail (Trim (State.Alpha'Image), 2) & " " &
                    SF.Tail (Trim (State.Depth'Image), 2) & " " &
                    SF.Tail (Trim (State.Stencil'Image), 2) & " " &
                    SF.Tail (Trim (State.Samples'Image), 2) &
                    (if Config = Used_Config then " (used)" else ""));
               end;
            end loop;

            Result.Set_EGL_Data (Object.Context, Used_Config, sRGB => True);

            Result.Create_Window ("", Title, Width, Height,
              Visible     => Visible,
              Resizable   => Resizable,
              Decorated   => True,
              Transparent => Transparent);

            Result.Make_Current (Object.Context);

            pragma Assert (Object.Context.Buffer = Back);

            Messages.Log (Debug, "Created AWT window");
            Messages.Log (Debug, "  size:        " &
              Trim (Width'Image) & " × " & Trim (Height'Image));
            Messages.Log (Debug, "  visible:     " & (if Visible then "yes" else "no"));
            Messages.Log (Debug, "  resizable:   " & (if Resizable then "yes" else "no"));
            Messages.Log (Debug, "  transparent: " & (if Transparent then "yes" else "no"));
         end;
      end return;
   end Create_Window;

end Orka.Contexts.EGL.Wayland.AWT;
