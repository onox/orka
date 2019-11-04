--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2019 onox <denkpadje@gmail.com>
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

package body Glfw.Input.Keys is

   function Key_Name
     (Key  : Input.Keys.Key;
      Code : Input.Keys.Scancode) return String is
   begin
      return Interfaces.C.Strings.Value (API.Get_Key_Name (Key, Code));
   end Key_Name;

   function Key_Code (Key : Input.Keys.Key) return Input.Keys.Scancode renames API.Get_Key_Code;

end Glfw.Input.Keys;
