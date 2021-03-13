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

with EGL.Objects.Devices;

package EGL.Objects.Displays is
   pragma Preelaborate;

   type Platform_Kind is (Device, GBM, Wayland);
   --  The following extension is required for a specific platform:
   --
   --  Device:      EGL_EXT_platform_device
   --  Wayland:     EGL_EXT_platform_wayland
   --  GBM:         EGL_MESA_platform_gbm

   type Display (Platform : Platform_Kind) is new EGL_Object with private;
   --  Not_Initialized_Error is raised if a display for the requested
   --  platform could not be initialized

   function Create_Display (Device : Devices.Device) return Display
     with Pre => not Device.In_Use;

   function Create_Display (Wayland_Display : Native_Display_Ptr) return Display;

   function Client_Extensions return String_List;
   --  Return a list of EGL client extensions
   --
   --  Requires EGL 1.5 or EGL_EXT_client_extensions. If an
   --  Invalid_Operation_Error is raised, then the extension is not
   --  supported.

   function Extensions (Object : Display) return String_List;
   --  Return a list of EGL display extensions

   function Vendor  (Object : Display) return String;
   function Version (Object : Display) return String;

   function Device (Object : Display) return Devices.Device;
   --  Return the device on which the display is based
   --
   --  Requires EGL_EXT_device_query extension.

   No_Display : constant Display;

private

   for Platform_Kind use
     (Device      => 16#313F#,
      GBM         => 16#31D7#,
      Wayland     => 16#31D8#);
   for Platform_Kind'Size use Enum'Size;

   type Display (Platform : Platform_Kind) is new EGL_Object with record
      Device : Devices.Device := Devices.No_Device;
   end record;

   overriding procedure Pre_Finalize (Object : in out Display);

   No_Display : constant Display := (EGL_Object with Platform => Device, others => <>);

end EGL.Objects.Displays;
