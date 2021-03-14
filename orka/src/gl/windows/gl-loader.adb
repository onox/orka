--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 onox <denkpadje@gmail.com>
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

package body GL.Loader is

   pragma Linker_Options ("-lkernel32", "-lopengl32");

   function Get_Proc_Address (Name : Interfaces.C.char_array) return System.Address is
      use type System.Address;

      function WGL_Get_Proc_Address
        (Name : Interfaces.C.char_array) return System.Address
      with Import, Convention => StdCall, External_Name => "wglGetProcAddress";

      function Old_Get_Proc_Address
        (Handle : System.Address;
         Name   : Interfaces.C.char_array) return System.Address
      with Import, Convention => StdCall, External_Name => "GetProcAddress";

      function Get_Module_Handle
        (Name : Interfaces.C.char_array) return System.Address
      with Import, Convention => StdCall, External_Name => "GetModuleHandleA";

      Result : constant System.Address := WGL_Get_Proc_Address (Name);
   begin
      if Result /= System.Null_Address then
         return Result;
      else
         --  OpenGL 1.1 functions are directly exported in the opengl32 library
         return Old_Get_Proc_Address (Get_Module_Handle ("opengl32"), Name);
      end if;
   end Get_Proc_Address;

end GL.Loader;
