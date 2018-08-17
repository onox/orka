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

with Interfaces.C;

package Glfw is
   pragma Preelaborate;

   subtype Seconds is Interfaces.C.double;

   subtype Size is Interfaces.C.int range 0 .. Interfaces.C.int'Last;

   Initialization_Exception : exception;
   Operation_Exception      : exception;

   procedure Init;

   -- because terminate is a keyword in Ada
   procedure Shutdown;

   procedure Version (Major, Minor, Rev : out Natural);

   function Version_String return String;

   function Time return Seconds;

   procedure Set_Time (Value : Seconds);

   function Extension_Supported (Name : String) return Boolean;

private
   package C renames Interfaces.C;

   type Bool is new Boolean;

   for Bool use (False => 0, True => 1);
   for Bool'Size use C.int'Size;
   pragma Convention (C, Bool);

end Glfw;
