--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2020 onox <denkpadje@gmail.com>
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

with Orka.Contexts;

with EGL.Objects.Displays;
with EGL.Objects.Surfaces;

package EGL.Objects.Contexts is
   pragma Preelaborate;

   subtype Context_Version is Orka.Contexts.Context_Version;
   subtype Context_Flags   is Orka.Contexts.Context_Flags;

   type Buffer_Kind is (None, Back, Front);

   type Context (Platform : Displays.Platform_Kind) is new EGL_Object with private;

   function Create_Context
     (Display : Displays.Display;
      Version : Context_Version;
      Flags   : Context_Flags) return Context
   with Pre  => Display.Is_Initialized,
        Post => Create_Context'Result.Is_Current;

   function Display (Object : Context) return Displays.Display
     with Post => Display'Result.Is_Initialized;

   function Buffer (Object : Context) return Buffer_Kind
     with Pre => Object.Is_Initialized;

   function Is_Current (Object : Context) return Boolean
     with Pre => Object.Is_Initialized;

   procedure Make_Current (Object : Context)
     with Pre  => Object.Is_Initialized,
          Post => Object.Is_Current;

   procedure Make_Current (Object : Context; Surface : Surfaces.Surface)
     with Pre  => Object.Is_Initialized and Surface.Is_Initialized,
          Post => Object.Is_Current;

   procedure Make_Not_Current (Object : Context)
     with Pre  => Object.Is_Initialized and then Object.Is_Current,
          Post => not Object.Is_Current;

   procedure Set_Swap_Interval (Object : Context; Value : Natural)
     with Pre => Object.Is_Initialized and then Object.Is_Current;

   type Client_API is private;

private

   type Client_API is (OpenGL_API);

   for Client_API use
     (OpenGL_API => 16#30A2#);
   for Client_API'Size use Enum'Size;

   for Buffer_Kind use
     (None  => 16#3038#,
      Back  => 16#3084#,
      Front => 16#3085#);

   type Context (Platform : Displays.Platform_Kind) is new EGL_Object with record
      Display : Displays.Display (Platform);
   end record;

   overriding procedure Pre_Finalize (Object : in out Context);

   function Display (Object : Context) return Displays.Display is (Object.Display);

end EGL.Objects.Contexts;
