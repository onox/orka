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

private with AWT.Inputs;
private with AWT.Monitors;
private with AWT.Wayland.Windows;

with AWT.Windows;

package Orka.Contexts.EGL.Wayland.AWT is
   pragma Preelaborate;

   type AWT_Context is limited new Surface_Context with private;

   overriding
   function Create_Context
     (Version : Orka.Contexts.Context_Version;
      Flags   : Orka.Contexts.Context_Flags := (others => False)) return AWT_Context;

   type AWT_Window is limited new Orka.Windows.Window and Standard.AWT.Windows.Window with private;

   overriding
   function Create_Window
     (Context            : Orka.Contexts.Surface_Context'Class;
      Width, Height      : Positive;
      Title              : String  := "";
      Samples            : Natural := 0;
      Visible, Resizable : Boolean := True;
      Transparent        : Boolean := False) return AWT_Window
   with Pre => Context in AWT_Context'Class;

private

   type AWT_Context is limited new Wayland_EGL_Context and Surface_Context with null record;

   overriding
   procedure Make_Current
     (Object : AWT_Context;
      Window : in out Orka.Windows.Window'Class);

   type AWT_Window is limited new Standard.AWT.Wayland.Windows.Wayland_Window
     and Orka.Windows.Window with null record;

   overriding
   function Width (Object : AWT_Window) return Positive;

   overriding
   function Height (Object : AWT_Window) return Positive;

   ----------------------------------------------------------------------------

   overriding
   procedure On_Move
     (Object   : in out AWT_Window;
      Monitor  : Standard.AWT.Monitors.Monitor'Class;
      Presence : Standard.AWT.Windows.Monitor_Presence);

end Orka.Contexts.EGL.Wayland.AWT;
