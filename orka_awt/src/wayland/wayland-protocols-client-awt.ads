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

private with Ada.Unchecked_Conversion;

private with Wayland.API;

with EGL;

package Wayland.Protocols.Client.AWT is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --                          Internal Subprograms                          --
   ----------------------------------------------------------------------------

   function Get_Display
     (Display : Wayland.Protocols.Client.Display) return Standard.EGL.Native_Display_Ptr;
   --  Used by function Create_Context in Orka.Contexts.EGL.AWT to create an
   --  EGL context using the Wayland platform

private

   function Convert is new Ada.Unchecked_Conversion
     (Source => Wayland.API.Display_Ptr, Target => Standard.EGL.Native_Display_Ptr);

   function Get_Display
     (Display : Wayland.Protocols.Client.Display) return Standard.EGL.Native_Display_Ptr
   is (Convert (Display.Proxy));

end Wayland.Protocols.Client.AWT;
