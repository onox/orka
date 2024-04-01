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

with EGL.API;
with EGL.Objects.Displays;
with EGL.Errors;

package body EGL.Objects.Devices is

   function Extensions (Object : Device) return String_List is
   begin
      Check_Extension (Displays.Client_Extensions, "EGL_EXT_device_query");

      declare
         Result : constant C.Strings.chars_ptr :=
           API.Query_Device_String.Ref (Object.ID, Extensions);

         use all type C.Strings.chars_ptr;
      begin
         if Result = C.Strings.Null_Ptr then
            Errors.Raise_Exception_On_EGL_Error;
            return [];
         else
            return EGL.Extensions (Result);
         end if;
      end;
   end Extensions;

   function DRM_Name (Object : Device) return String is
   begin
      Check_Extension (Object.Extensions, "EGL_EXT_device_drm");

      declare
         Result : constant C.Strings.chars_ptr :=
           API.Query_Device_String.Ref (Object.ID, DRM_Device_File);

         use all type C.Strings.chars_ptr;
      begin
         if Result = C.Strings.Null_Ptr then
            Errors.Raise_Exception_On_EGL_Error;
            return "";
         else
            return Trim (Result);
         end if;
      end;
   end DRM_Name;

   function Name (Object : Device) return String is
   begin
      return DRM_Name (Object);
   exception
      when Feature_Not_Supported =>
         return "";
   end Name;

   function Devices return Device_List is
      Max_Devices : constant := 16;

      Devices : ID_Array (1 .. Max_Devices);
      Count   : Int := 0;
   begin
      Check_Extension (Displays.Client_Extensions, "EGL_EXT_device_enumeration");

      if API.Query_Devices.Ref (Max_Devices, Devices, Count) then
         return Result : Device_List (1 .. Natural (Count)) do
            for Index in Result'Range loop
               Result (Index).Reference.ID := Devices (Index);
               pragma Assert (Result (Index).Display = No_Display);
            end loop;
         end return;
      else
         Errors.Raise_Exception_On_EGL_Error;
         return [];
      end if;
   end Devices;

   function Get_Device (Subject : Displays.Display) return Device is
      Device_Ext : constant Int := 16#322C#;

      ID : ID_Type;
   begin
      Check_Extension (Displays.Client_Extensions, "EGL_EXT_device_query");

      if API.Query_Display_Attrib.Ref (Subject.ID, Device_Ext, ID) then
         return Result : Device do
            Result.Reference.ID := ID;

            --  The device needs to keep a reference to the display,
            --  otherwise the display can get finalized and any further
            --  use of the device would then give undefined results
            pragma Assert (Result.Display = No_Display);
            Result.Display := (Ada.Finalization.Controlled with
                                 Reference => EGL_Object (Subject).Reference);
            Result.Display.Reference.Count := Result.Display.Reference.Count + 1;
            pragma Assert (Result.Display /= No_Display);
         end return;
      else
         Errors.Raise_Exception_On_EGL_Error;
         return Result : Device;
      end if;
   end Get_Device;

end EGL.Objects.Devices;
