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

with EGL.Objects.Displays;

package EGL.Objects.Configs is
   pragma Preelaborate;

   use type Displays.Display;

   type Sample_Size is range 0 .. 16
     with Static_Predicate => Sample_Size in 0 | 2 | 4 | 8 | 16;

   type Config_State is record
      Red, Green, Blue, Alpha : Natural;
      Depth, Stencil          : Natural;
      Samples                 : Sample_Size;
   end record;

   type Config is new EGL_Object with private;

   function State (Object : Config) return Config_State;

   type Config_Array is array (Positive range <>) of Config;

   function Get_Configs
     (Display                 : Displays.Display;
      Red, Green, Blue, Alpha : Natural;
      Depth, Stencil          : Natural;
      Samples                 : Sample_Size) return Config_Array
   with Pre  => Display.Is_Initialized,
        Post => (for all Config of Get_Configs'Result => Config.Is_Initialized);

private

   type Config is new EGL_Object with record
      State : Config_State;
   end record;

end EGL.Objects.Configs;
