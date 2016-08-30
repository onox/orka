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

with Interfaces.C.Strings;
with System;

private package GL.API.Mac_OS_X is
   pragma Preelaborate;
   
   -- package for MacOSX-specific stuff
      
   package IFC renames Interfaces.C.Strings;
   
   subtype CFStringRef    is System.Address;
   subtype CFBundleRef    is System.Address;
   subtype CFAllocatorRef is System.Address;
   
   type CFStringEncoding is new Integer;
   pragma Convention (C, CFStringEncoding);
   for CFStringEncoding'Size use 32;
   
   kCFStringEncodingASCII : constant CFStringEncoding := 16#0600#;
      
   function CFBundleGetBundleWithIdentifier (bundleID : CFStringRef)
     return CFBundleRef;
   pragma Import (Convention => C, Entity => CFBundleGetBundleWithIdentifier,
                  External_Name => "CFBundleGetBundleWithIdentifier");
   
   function CFStringCreateWithCString (alloc    : CFAllocatorRef;
                                       cStr     : IFC.chars_ptr;
                                       encoding : CFStringEncoding)
                                       return CFStringRef;
   pragma Import (Convention => C, Entity => CFStringCreateWithCString,
                  External_Name => "CFStringCreateWithCString");
   
   function CFBundleGetFunctionPointerForName (bundle : CFBundleRef;
                                               functionName : CFStringRef)
                                              return System.Address;
   pragma Import (Convention => C, Entity => CFBundleGetFunctionPointerForName,
                  External_Name => "CFBundleGetFunctionPointerForName");
   
   procedure CFRelease (cf : System.Address);
   pragma Import (Convention => C, Entity => CFRelease,
                  External_Name => "CFRelease");
      
   function OpenGLFramework return CFBundleRef;
   
   
end GL.API.Mac_OS_X;
