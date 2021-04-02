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

with Ada.Unchecked_Conversion;

with EGL.API;
with EGL.Errors;

package body EGL.Objects.Contexts is

   Major_Version       : constant Int := 16#3098#;
   Minor_Version       : constant Int := 16#30FB#;
   Context_Flags_Bits  : constant Int := 16#30FC#;
   OpenGL_Profile_Mask : constant Int := 16#30FD#;

   --  EGL 1.5
--   OpenGL_Debug                       : constant Int := 16#31B0#;
--   OpenGL_Forward_Compatible          : constant Int := 16#31B1#;
--   OpenGL_Robust                      : constant Int := 16#31B2#;

   OpenGL_No_Error                    : constant Int := 16#31B3#;
   OpenGL_Reset_Notification_Strategy : constant Int := 16#31BD#;

   Context_Release_Behavior : constant Int := 16#2097#;

   Context_Release_Behavior_None  : constant Int := 0;
--   Context_Release_Behavior_Flush : constant Int := 16#2098#;

   No_Reset_Notification : constant Int := 16#31BE#;
   Lose_Context_On_Reset : constant Int := 16#31BF#;

   OpenGL_Core_Profile : constant Int := 16#0000_0001#;

   type Flag_Bits is record
      Debug   : Boolean := False;
      Forward : Boolean := True;
      Robust  : Boolean := False;
   end record;

   for Flag_Bits use record
      Debug   at 0 range 0 .. 0;
      Forward at 0 range 1 .. 1;
      Robust  at 0 range 2 .. 2;
   end record;
   for Flag_Bits'Size use Int'Size;

   function Create_Context
     (Display : Displays.Display;
      Version : Context_Version;
      Flags   : Context_Flags) return Context
   is
      No_Config  : constant ID_Type := ID_Type (System.Null_Address);
      No_Context : constant ID_Type := ID_Type (System.Null_Address);

      function Convert is new Ada.Unchecked_Conversion (Flag_Bits, Int);

      Flags_Mask : constant Flag_Bits :=
        (Debug   => Flags.Debug,
         Forward => True,
         Robust  => Flags.Robust);

      Attributes : constant Int_Array :=
        (Major_Version, Int (Version.Major),
         Minor_Version, Int (Version.Minor),
         Context_Flags_Bits, Convert (Flags_Mask),
         OpenGL_Profile_Mask, OpenGL_Core_Profile,

         --  EGL 1.5
--         OpenGL_Debug, (if Flags.Debug then 1 else 0),
--         OpenGL_Forward_Compatible, 1,
--         OpenGL_Robust, (if Flags.Robust then 1 else 0),

         OpenGL_Reset_Notification_Strategy,
           (if Flags.Robust then Lose_Context_On_Reset else No_Reset_Notification));
         --  Requires EGL_KHR_create_context or EGL 1.5

      No_Error : constant Int_Array := (OpenGL_No_Error, 1);
      No_Flush : constant Int_Array := (Context_Release_Behavior, Context_Release_Behavior_None);

      Extensions : constant String_List := Display.Extensions;
   begin
      if not Boolean (API.Bind_API (OpenGL_API)) then
         Errors.Raise_Exception_On_EGL_Error;
      end if;

      Check_Extension (Extensions, "EGL_KHR_create_context");
      Check_Extension (Extensions, "EGL_KHR_no_config_context");
      Check_Extension (Extensions, "EGL_KHR_surfaceless_context");

      declare
         Can_Have_No_Error : constant Boolean :=
           Flags.No_Error and then Has_Extension (Extensions, "EGL_KHR_create_context_no_error");

         Can_Have_Release_Context : constant Boolean :=
           Has_Extension (Extensions, "EGL_KHR_context_flush_control");

         ID : constant ID_Type :=
           API.Create_Context (Display.ID, No_Config, No_Context,
             Attributes &
             (if Can_Have_No_Error then No_Error else (1 .. 0 => <>)) &
             (if Can_Have_Release_Context then No_Flush else (1 .. 0 => <>)) &
             None);
         --  TODO Support shared context (replace No_Context with Shared_Context.ID)
      begin
         if ID = No_Context then
            Errors.Raise_Exception_On_EGL_Error;
         end if;

         return Result : Context (Display.Platform) do
            Result.Reference.ID := ID;
            Result.Display      := Display;
         end return;
      end;
   end Create_Context;

   function Buffer (Object : Context) return Buffer_Kind is
      Result : Buffer_Kind;
   begin
      if not Boolean (API.Query_Context (Object.Display.ID, Object.ID, Render_Buffer, Result)) then
         Errors.Raise_Exception_On_EGL_Error;
      end if;

      return Result;
   end Buffer;

   function Is_Current (Object : Context; Kind : Task_Kind) return Boolean is
   begin
      case Kind is
         when Current_Task =>
            return API.Get_Current_Context = Object.ID;
         when Any_Task =>
            return Object.Reference.Active;
      end case;
   end Is_Current;

   procedure Make_Current (Object : Context) is
      No_Surface : constant ID_Type := ID_Type (System.Null_Address);
   begin
      if not API.Make_Current (Object.Display.ID, No_Surface, No_Surface, Object.ID) then
         Errors.Raise_Exception_On_EGL_Error;
      end if;
      Object.Reference.Active := True;
   end Make_Current;

   procedure Make_Current (Object : Context; Surface : Surfaces.Surface) is
   begin
      if not API.Make_Current (Object.Display.ID, Surface.ID, Surface.ID, Object.ID) then
         Errors.Raise_Exception_On_EGL_Error;
      end if;
      Object.Reference.Active := True;
   end Make_Current;

   procedure Make_Not_Current (Object : Context) is
      No_Context : constant ID_Type := ID_Type (System.Null_Address);
      No_Surface : constant ID_Type := ID_Type (System.Null_Address);
   begin
      if not API.Make_Current (Object.Display.ID, No_Surface, No_Surface, No_Context) then
         Errors.Raise_Exception_On_EGL_Error;
      end if;
      Object.Reference.Active := False;
   end Make_Not_Current;

   procedure Set_Swap_Interval (Object : Context; Value : Natural) is
   begin
      if not Boolean (API.Swap_Interval (Object.Display.ID, Int (Value))) then
         Errors.Raise_Exception_On_EGL_Error;
      end if;
   end Set_Swap_Interval;

   overriding procedure Pre_Finalize (Object : in out Context) is
      No_Context : constant ID_Type := ID_Type (System.Null_Address);
   begin
      pragma Assert (Object.ID /= No_Context);
      if Object.Is_Current (Current_Task) then
         Object.Make_Not_Current;
      end if;
      if not Boolean (API.Destroy_Context (Object.Display.ID, Object.ID)) then
         Errors.Raise_Exception_On_EGL_Error;
      end if;
      Object.Reference.ID := No_Context;
   end Pre_Finalize;

end EGL.Objects.Contexts;
