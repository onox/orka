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

with GL.API.Mac_OS_X;

package body GL.API is
   function GL_Subprogram_Reference (Function_Name : String) return System.Address is
      -- OSX-specific implementation uses CoreFoundation functions
      use GL.API.Mac_OS_X;
   
      package IFC renames Interfaces.C.Strings;
   
      GL_Function_Name_C : IFC.chars_ptr := IFC.New_String (Function_Name);

      Symbol_Name : constant CFStringRef := CFStringCreateWithCString
        (alloc => System.Null_Address, cStr => GL_Function_Name_C,
         encoding => kCFStringEncodingASCII);
      Result : constant System.Address := CFBundleGetFunctionPointerForName
        (bundle => OpenGLFramework,
         functionName => Symbol_Name);
   begin
      CFRelease (Symbol_Name);
      IFC.Free (GL_Function_Name_C);
      return Result;
   end GL_Subprogram_Reference;
end GL.API;
