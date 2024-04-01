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

with Interfaces.C.Strings;

with Ada.Unchecked_Conversion;

with EGL.API;
with EGL.Errors;

package body EGL.Objects.Displays is

   function Client_Extensions return String_List is
      No_Display : constant ID_Type := ID_Type (System.Null_Address);

      Result : constant C.Strings.chars_ptr := API.Query_String (No_Display, Extensions);

      use all type C.Strings.chars_ptr;
   begin
      if Result = C.Strings.Null_Ptr then
         Errors.Raise_Exception_On_EGL_Error;
         return [];
      else
         return EGL.Extensions (Result);
      end if;
   exception
      when Errors.Invalid_Operation_Error =>
         raise Feature_Not_Supported with "EGL_EXT_client_extensions not supported";
   end Client_Extensions;

   function Extensions (Object : Display) return String_List is
      Result : constant C.Strings.chars_ptr := API.Query_String (Object.ID, Extensions);

      use all type C.Strings.chars_ptr;
   begin
      if Result = C.Strings.Null_Ptr then
         Errors.Raise_Exception_On_EGL_Error;
         return [];
      else
         return EGL.Extensions (Result);
      end if;
   end Extensions;

   function Vendor (Object : Display) return String is
      Result : constant C.Strings.chars_ptr := API.Query_String (Object.ID, Vendor);

      use all type C.Strings.chars_ptr;
   begin
      if Result = C.Strings.Null_Ptr then
         Errors.Raise_Exception_On_EGL_Error;
         return "";
      else
         return C.Strings.Value (Result);
      end if;
   end Vendor;

   function Version (Object : Display) return String is
      Result : constant C.Strings.chars_ptr := API.Query_String (Object.ID, Version);

      use all type C.Strings.chars_ptr;
   begin
      if Result = C.Strings.Null_Ptr then
         Errors.Raise_Exception_On_EGL_Error;
         return "";
      else
         return C.Strings.Value (Result);
      end if;
   end Version;

   -----------------------------------------------------------------------------

   function Create_Display
     (Platform       : Platform_Kind;
      Native_Display : Void_Ptr;
      Device         : Devices.Device) return Display
   is
      No_Display : constant ID_Type := ID_Type (System.Null_Address);

      Attributes : constant Int_Array := [1 => None];

      Major, Minor : Int;
   begin
      Check_Extension (Client_Extensions,
        (case Platform is
           when GBM             => "EGL_MESA_platform_gbm",
           when Displays.Device => "EGL_EXT_platform_device",
           when Wayland         => "EGL_EXT_platform_wayland"));

      return Result : Display (Platform) do
         Result.Reference.ID :=
           API.Get_Platform_Display.Ref (Platform, Native_Display, Attributes);

         if Result.ID = No_Display
           or else not Boolean (API.Initialize_Display (Result.ID, Major, Minor))
         then
            Errors.Raise_Exception_On_EGL_Error;
         end if;

         Result.Device := Device;
      end return;
   end Create_Display;

   function Create_Display (Device : Devices.Device) return Display is
      function Convert is new Ada.Unchecked_Conversion (ID_Type, Void_Ptr);
   begin
      return Create_Display (Displays.Device, Convert (Device.ID), Device);
   end Create_Display;

   function Create_Display (Wayland_Display : Native_Display_Ptr) return Display is
      function Convert is new Ada.Unchecked_Conversion (Native_Display_Ptr, Void_Ptr);
   begin
      return Create_Display (Displays.Wayland, Convert (Wayland_Display), Devices.No_Device);
   end Create_Display;

   function Create_Display (Platform : Platform_Kind) return Display is
      function Convert is new Ada.Unchecked_Conversion (ID_Type, Void_Ptr);

      function Get_First_Device return Devices.Device is
         All_Devices : constant EGL.Objects.Devices.Device_List := EGL.Objects.Devices.Devices;
      begin
         return All_Devices (All_Devices'First);
      end Get_First_Device;

      Native_Display : constant Void_Ptr :=
        (case Platform is
           when GBM | Wayland   => Void_Ptr (System.Null_Address),
           when Displays.Device => Convert (Get_First_Device.ID));
   begin
      return Create_Display (Platform, Native_Display, Devices.No_Device);
   end Create_Display;

   overriding procedure Pre_Finalize (Object : in out Display) is
      No_Display : constant ID_Type := ID_Type (System.Null_Address);
   begin
      pragma Assert (Object.ID /= No_Display);
      if not Boolean (API.Terminate_Display (Object.ID)) then
         Errors.Raise_Exception_On_EGL_Error;
      end if;
      Object.Reference.ID := No_Display;
   end Pre_Finalize;

   function Device (Object : Display) return Devices.Device is
      use type EGL.Objects.Devices.Device;
   begin
      return Result : constant Devices.Device := Devices.Get_Device (Object) do
         if Object.Device /= Devices.No_Device then
            pragma Assert (Object.Device.ID = Result.ID);
         end if;
      end return;
   end Device;

end EGL.Objects.Displays;
