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

package EGL.Objects.Contexts is
   pragma Preelaborate;

   subtype Context_Version is Orka.Contexts.Context_Version;
   subtype Context_Flags   is Orka.Contexts.Context_Flags;

   type Context (Platform : Displays.Platform_Kind) is new EGL_Object with private;

   function Create_Context
     (Display : Displays.Display;
      Version : Context_Version;
      Flags   : Context_Flags) return Context;

   function Display (Object : Context) return Displays.Display;

   type Client_API is private;

private

   type Client_API is (OpenGL_API);

   for Client_API use
     (OpenGL_API => 16#30A2#);
   for Client_API'Size use Enum'Size;

   type Context (Platform : Displays.Platform_Kind) is new EGL_Object with record
      Display : Displays.Display (Platform);
   end record;

   overriding procedure Pre_Finalize (Object : in out Context);

   function Display (Object : Context) return Displays.Display is (Object.Display);

end EGL.Objects.Contexts;
