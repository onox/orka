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

with Interfaces.C.Strings;

with Glfw.API;

package body Glfw.Windows.Clipboard is

   function Get (Object : not null access Window'Class) return String is
      use type Interfaces.C.Strings.chars_ptr;

      Raw : constant Interfaces.C.Strings.chars_ptr
        := API.Get_Clipboard_String (Object.Handle);
   begin
      if Raw = Interfaces.C.Strings.Null_Ptr then
         raise Operation_Exception with "Could not get clipboard string";
      end if;
      return Interfaces.C.Strings.Value (Raw);
   end Get;

   procedure Set (Object : not null access Window'Class; Value : String) is
   begin
      API.Set_Clipboard_String (Object.Handle, Interfaces.C.To_C (Value));
   end Set;

end Glfw.Windows.Clipboard;
