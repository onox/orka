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

package body GL.API.Mac_OS_X is
   OpenGLFramework_Cached : CFBundleRef;
  
   function OpenGLFramework return CFBundleRef is
      use type System.Address;
   begin
      if OpenGLFramework_Cached = System.Null_Address then
         declare
            OpenGLFramework_ID : constant CFStringRef
              := CFStringCreateWithCString (System.Null_Address,
                                            IFC.New_String ("com.apple.opengl"),
                                            kCFStringEncodingASCII);
         begin
            OpenGLFramework_Cached 
              := CFBundleGetBundleWithIdentifier (OpenGLFramework_ID);
         end;
      end if;
      return OpenGLFramework_Cached;
   end OpenGLFramework;

end GL.API.Mac_OS_X;
