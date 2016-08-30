--  Copyright (c) 2012 Felix Krause <contact@flyx.org>
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

with GL.GLX;

package body GL.API is
   function GL_Subprogram_Reference (Function_Name : String)
                                     return System.Address is
      GL_Function_Name_C : Interfaces.C.Strings.chars_ptr
        := Interfaces.C.Strings.New_String (Function_Name);

      Result : constant System.Address
        := GL.GLX.Get_Proc_Address (GL_Function_Name_C);
   begin
      Interfaces.C.Strings.Free (GL_Function_Name_C);
      return Result;
   end GL_Subprogram_Reference;
end GL.API;
