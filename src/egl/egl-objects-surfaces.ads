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

with EGL.Objects.Configs;
with EGL.Objects.Displays;

package EGL.Objects.Surfaces is
   pragma Preelaborate;

   type Swap_Behavior is (Buffer_Preserved, Buffer_Destroyed);

   type Surface (Platform : Displays.Platform_Kind) is new EGL_Object with private;

   function Create_Surface
     (Display : Displays.Display;
      Config  : Configs.Config;
      Window  : Native_Window_Ptr;
      sRGB    : Boolean) return Surface
   with Pre => Display.Is_Initialized and Config.Is_Initialized;

   function Width (Object : Surface) return Natural
     with Pre => Object.Is_Initialized;

   function Height (Object : Surface) return Natural
     with Pre => Object.Is_Initialized;

   function Behavior (Object : Surface) return Swap_Behavior
     with Pre => Object.Is_Initialized;

   procedure Swap_Buffers (Object : Surface)
     with Pre => Object.Is_Initialized;

private

   for Swap_Behavior use
     (Buffer_Preserved => 16#3094#,
      Buffer_Destroyed => 16#3095#);
   for Swap_Behavior'Size use Int'Size;

   type Surface (Platform : Displays.Platform_Kind) is new EGL_Object with record
      Display : Displays.Display (Platform);
   end record;

   overriding procedure Pre_Finalize (Object : in out Surface);

end EGL.Objects.Surfaces;
