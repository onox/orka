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

with Orka.Logging.Default;
with Orka.Strings;
with Orka.Terminals;

with EGL.Objects.Configs;

with Wayland.Protocols.Client.AWT;

package body Orka.Contexts.EGL.Wayland.AWT is

   use all type Orka.Logging.Default_Module;
   use all type Orka.Logging.Severity;
   use Orka.Logging;

   procedure Log is new Orka.Logging.Default.Generic_Log (Window_System);

   procedure Print_Monitor (Monitor : Standard.AWT.Monitors.Monitor'Class) is
      State : constant Standard.AWT.Monitors.Monitor_State := Monitor.State;

      use Standard.AWT.Monitors;
   begin
      Log (Debug, "Window visible on monitor " & (+State.Name));
      Log (Debug, "  offset:     " &
        Trim (State.X'Image) & ", " & Trim (State.Y'Image));
      Log (Debug, "  size:       " &
        Trim (State.Width'Image) & Orka.Strings.Unicode (" × ") & Trim (State.Height'Image));
      Log (Debug, "  refresh: " & Orka.Terminals.Image (State.Refresh));
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

   function Framebuffer_Resized (Object : in out AWT_Window) return Boolean is
      Result : constant Boolean := Object.Resize;
   begin
      Object.Resize := False;
      return Result;
   end Framebuffer_Resized;

   overriding
   procedure On_Configure
     (Object : in out AWT_Window;
      State  : Standard.AWT.Windows.Window_State) is
   begin
      Log (Debug, "Configured window surface of " &
        Trim (State.Width'Image) & Orka.Strings.Unicode (" × ") & Trim (State.Height'Image));
      if State.Margin > 0 then
         Log (Debug, "  margin: " & Trim (State.Margin'Image));
      end if;

      Object.Resize := State.Visible and State.Width > 0 and State.Height > 0;
   end On_Configure;

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
            Log (Debug, "Window not visible on monitor " & (+Monitor.State.Name));
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
     (Context            : aliased Orka.Contexts.Surface_Context'Class;
      Width, Height      : Positive;
      Title              : String  := "";
      Samples            : Natural := 0;
      Visible, Resizable : Boolean := True;
      Transparent        : Boolean := False) return AWT_Window
   is
      package EGL_Configs renames Standard.EGL.Objects.Configs;

      use all type Standard.EGL.Objects.Contexts.Buffer_Kind;

      package SU renames Standard.AWT.SU;

      function Flags return String is
         Result : SU.Unbounded_String;
      begin
         if Visible then
            SU.Append (Result, " visible");
         end if;

         if Resizable then
            SU.Append (Result, " resizable");
         end if;

         if Transparent then
            SU.Append (Result, " transparent");
         end if;

         return Trim (SU.To_String (Result));
      end Flags;

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
            Log (Debug, "Available EGL configs: " &
              Trim (Natural'Image (Configs'Length)));

            Result.Set_EGL_Data (Object.Context, Used_Config, sRGB => False);

            Result.Create_Window ("", Title, Width, Height,
              Visible     => Visible,
              Resizable   => Resizable,
              Decorated   => True,
              Transparent => Transparent);

            Result.Make_Current (Object.Context);

            pragma Assert (Object.Context.Buffer = Back);

            Log (Info,  "Created window of " &
              Trim (Width'Image) & Orka.Strings.Unicode (" × ") & Trim (Height'Image));
            Log (Info,  "  flags:       " & Flags);

            Log (Debug, "  framebuffer:");
            declare
               State : constant EGL_Configs.Config_State := Used_Config.State;
            begin
               Log (Debug, "    colors:  " &
                Trim (State.Red'Image) & " " &
                Trim (State.Green'Image) & " " &
                Trim (State.Blue'Image) & " " &
                Trim (State.Alpha'Image));
               Log (Debug, "    depth:   " & Trim (State.Depth'Image));
               Log (Debug, "    stencil: " & Trim (State.Stencil'Image));
               Log (Debug, "    samples: " & Trim (State.Samples'Image));
            end;
         end;
      end return;
   end Create_Window;

end Orka.Contexts.EGL.Wayland.AWT;
