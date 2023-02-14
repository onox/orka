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

with EGL.Objects.Configs;
with EGL.Objects.Displays;
with EGL.Objects.Surfaces;

package Wayland.EGL.AWT is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --                          Internal Subprograms                          --
   ----------------------------------------------------------------------------

   use type Standard.EGL.Objects.Displays.Platform_Kind;

   function Create_Surface
     (Object  : Window;
      Display : Standard.EGL.Objects.Displays.Display;
      Config  : Standard.EGL.Objects.Configs.Config;
      sRGB    : Boolean) return Standard.EGL.Objects.Surfaces.Surface
   with Pre  => Display.Platform = Standard.EGL.Objects.Displays.Wayland
                  and Display.Is_Initialized
                  and Config.Is_Initialized,
        Post => Create_Surface'Result.Is_Initialized;
   --  Used by procedure Create_Window in AWT.Wayland.Windows to
   --  create an EGL surface

private

   function Convert is new Ada.Unchecked_Conversion
     (Source => EGL_API.EGL_Window_Ptr, Target => Standard.EGL.Native_Window_Ptr);

   function Create_Surface
     (Object  : Window;
      Display : Standard.EGL.Objects.Displays.Display;
      Config  : Standard.EGL.Objects.Configs.Config;
      sRGB    : Boolean) return Standard.EGL.Objects.Surfaces.Surface
   is (Standard.EGL.Objects.Surfaces.Create_Surface
         (Display, Config, Convert (Object.Handle), sRGB));

end Wayland.EGL.AWT;
