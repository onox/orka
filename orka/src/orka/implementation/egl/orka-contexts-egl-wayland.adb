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

with EGL.Debug;

package body Orka.Contexts.EGL.Wayland is

   overriding
   function Create_Context
     (Version : Context_Version;
      Flags   : Context_Flags := (others => False)) return Wayland_EGL_Context is
   begin
      return Result : constant Wayland_EGL_Context :=
        (Ada.Finalization.Limited_Controlled with others => <>)
      do
         --  EGL context for Wayland platform requires a pointer to a native
         --  Wayland display
         raise Program_Error;
      end return;
   end Create_Context;

   function Create_Context
     (Window  : Standard.EGL.Native_Display_Ptr;
      Version : Context_Version;
      Flags   : Context_Flags := (others => False)) return Wayland_EGL_Context
   is
      package EGL_Contexts renames Standard.EGL.Objects.Contexts;
      package EGL_Displays renames Standard.EGL.Objects.Displays;
   begin
      Standard.EGL.Debug.Set_Message_Callback (Print_Error'Access);

      declare
         Display : constant EGL_Displays.Display := EGL_Displays.Create_Display (Window);
      begin
         return Result : Wayland_EGL_Context do
            Result.Context := EGL_Contexts.Create_Context
              (Display,
               (Major => Version.Major,
                Minor => Version.Minor),
               (Debug    => Flags.Debug,
                Robust   => Flags.Robust,
                No_Error => Flags.No_Error));

            Result.Context.Make_Current;

            Post_Initialize (Result);
            Print_Debug (Display, Flags);
         end return;
      end;
   end Create_Context;

   overriding
   procedure Make_Current (Object : Wayland_EGL_Context) is
   begin
      Object.Context.Make_Current;
   end Make_Current;

   overriding
   procedure Make_Not_Current (Object : Wayland_EGL_Context) is
   begin
      Object.Context.Make_Not_Current;
   end Make_Not_Current;

end Orka.Contexts.EGL.Wayland;
