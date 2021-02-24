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

with EGL.API;
with EGL.Errors;

package body EGL.Objects.Surfaces is

   Color_Space        : constant Int := 16#309D#;
   Color_Space_sRGB   : constant Int := 16#3089#;
   Color_Space_Linear : constant Int := 16#308A#;

   function Create_Surface
     (Display : Displays.Display;
      Config  : Configs.Config;
      Window  : Native_Window_Ptr;
      sRGB    : Boolean) return Surface
   is
      No_Surface : constant ID_Type := ID_Type (System.Null_Address);

      Attributes : constant Int_Array :=
        (Color_Space,
         (if sRGB then Color_Space_sRGB else Color_Space_Linear),
         None);

      ID : constant ID_Type :=
         API.Create_Platform_Window_Surface.Ref
           (Display.ID, Config.ID, Window, Attributes);
   begin
      if ID = No_Surface then
         Errors.Raise_Exception_On_EGL_Error;
      end if;

      return Result : Surface (Display.Platform) do
         Result.Reference.ID := ID;
         Result.Display      := Display;
      end return;
   end Create_Surface;

   procedure Swap_Buffers (Object : Surface) is
   begin
      if not Boolean (API.Swap_Buffers (Object.Display.ID, Object.ID)) then
         Errors.Raise_Exception_On_EGL_Error;
      end if;
   end Swap_Buffers;

   overriding procedure Pre_Finalize (Object : in out Surface) is
      No_Surface : constant ID_Type := ID_Type (System.Null_Address);
   begin
      pragma Assert (Object.ID /= No_Surface);
      if not Boolean (API.Destroy_Surface (Object.Display.ID, Object.ID)) then
         Errors.Raise_Exception_On_EGL_Error;
      end if;
      Object.Reference.ID := No_Surface;
   end Pre_Finalize;

end EGL.Objects.Surfaces;
