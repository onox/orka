--  Copyright (c) 2016 onox <denkpadje@gmail.com>
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

with System;
with Interfaces.C.Strings;

package GL.Loader is
   pragma Preelaborate;

   function Get_Proc_Address (Name : Interfaces.C.Strings.chars_ptr)
     return System.Address
   with Import, Convention => StdCall, External_Name => "wglGetProcAddress";

end GL.Loader;
