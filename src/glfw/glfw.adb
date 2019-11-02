--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2013 Felix Krause <contact@flyx.org>
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

with Glfw.API;

with Interfaces.C.Strings;

package body Glfw is

   procedure Init is
      use type Interfaces.C.int;
   begin
      if API.Init = 0 then
         raise Initialization_Exception;
      end if;
   end Init;

   procedure Shutdown is
   begin
      API.Glfw_Terminate;
   end Shutdown;

   procedure Version (Major, Minor, Rev : out Natural) is
      Raw_Major, Raw_Minor, Raw_Rev : C.int;
   begin
      API.Get_Version (Raw_Major, Raw_Minor, Raw_Rev);
      Major := Natural (Raw_Major);
      Minor := Natural (Raw_Minor);
      Rev   := Natural (Raw_Rev);
   end Version;

   function Version_String return String is
   begin
      return Interfaces.C.Strings.Value (API.Get_Version_String);
   end Version_String;

   function Extension_Supported (Name : String) return Boolean is
   begin
      return Boolean (API.Extension_Supported (Interfaces.C.To_C (Name)));
   end Extension_Supported;

end Glfw;
