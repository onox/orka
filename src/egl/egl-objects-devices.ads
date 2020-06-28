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

limited with EGL.Objects.Displays;

package EGL.Objects.Devices is
   pragma Preelaborate;

   type Device is new EGL_Object with private;

   function Extensions (Object : Device) return String_List;
   --  Return a list of EGL device extensions
   --
   --  Requires EGL_EXT_device_query extension

   function Name (Object : Device) return String;
   --  Return the name of a device, or empty string if device has
   --  no name
   --
   --  Requires EGL_EXT_device_drm extension for DRM devices.

   function In_Use (Object : Device) return Boolean;
   --  Return True if the device was retrieved from a display, False otherwise

   No_Device : constant Device;

   type Device_List is array (Natural range <>) of aliased Device;

   function Devices return Device_List;
   --  Return a list of devices
   --
   --  Requires EGL_EXT_device_enumeration extension

   function Get_Device (Subject : Displays.Display) return Device
     with Post => Get_Device'Result.In_Use;
   --  Return the device of a display
   --
   --  Requires EGL_EXT_device_query extension.

private

   type Fake_Display is new EGL_Object with null record;
   --  Contains the Reference of a real Display when a Device is
   --  retrieved via function Get_Device

   No_Fake_Display : constant Fake_Display := (EGL_Object with null record);

   type Device is new EGL_Object with record
      Display : Fake_Display := No_Fake_Display;
      --  Since a device is a property of a display, the display must
      --  not get finalized while the device is still allocated. Otherwise
      --  any further use will give undefined results.
      --
      --  See EGL_EXT_device_query extension.
   end record;

   No_Device : constant Device := (EGL_Object with Display => <>);

   function In_Use (Object : Device) return Boolean is (Object.Display /= No_Fake_Display);

end EGL.Objects.Devices;
