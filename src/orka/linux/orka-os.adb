--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

package body Orka.OS is

   procedure Set_Task_Name (Name : in String) is
      use Interfaces.C;

      PR_SET_NAME : constant := 15;

      function prctl
        (option : int;
         arg2   : Strings.chars_ptr;
         arg3, arg4, arg5 : unsigned_long := 0) return int
      with Import, Convention => C, External_Name => "prctl";

      C_Name_Str : Strings.chars_ptr := Strings.New_String (Name);
      Result     : int;
   begin
      Result := prctl (PR_SET_NAME, C_Name_Str);
      Strings.Free (C_Name_Str);

      pragma Assert (Result = 0);
   end Set_Task_Name;

end Orka.OS;
