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

package Orka.Contexts.EGL.Wayland is

   type Wayland_EGL_Context is limited new Context with private;

   overriding
   function Create_Context
     (Version : Context_Version;
      Flags   : Context_Flags := (others => False)) return Wayland_EGL_Context;
   --  Raise Program_Error due to the missing native Wayland display
   --
   --  This function must be overriden and internally call the function below.

   function Create_Context
     (Window  : Standard.EGL.Native_Display_Ptr;
      Version : Context_Version;
      Flags   : Context_Flags := (others => False)) return Wayland_EGL_Context;
   --  Return a Wayland EGL context

private

   type Wayland_EGL_Context is limited new EGL_Context with record
      Context : Standard.EGL.Objects.Contexts.Context (Standard.EGL.Objects.Displays.Wayland);
   end record;

   overriding
   function Is_Current (Object : Wayland_EGL_Context; Kind : Task_Kind) return Boolean is
     (Object.Context.Is_Current
        (case Kind is
           when Current_Task => Standard.EGL.Objects.Contexts.Current_Task,
           when Any_Task     => Standard.EGL.Objects.Contexts.Any_Task));

   overriding
   procedure Make_Current (Object : Wayland_EGL_Context);

   overriding
   procedure Make_Not_Current (Object : Wayland_EGL_Context);

end Orka.Contexts.EGL.Wayland;
