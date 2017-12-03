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

with GL.API;
with GL.Enums.Getter;
with GL.Errors;

package body GL.Context is

   function Extension (Index : Positive) return String is
     (C.Strings.Value (API.Get_String_I (Enums.Getter.Extensions, UInt (Index - 1))));

   function GLSL_Version (Index : Positive) return String is
     (C.Strings.Value (API.Get_String_I (Enums.Getter.Shading_Language_Version, UInt (Index - 1))));

   function Major_Version return Int is
      Result : aliased Int;
   begin
      API.Get_Integer (Enums.Getter.Major_Version, Result'Access);
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Major_Version;

   function Minor_Version return Int is
      Result : aliased Int;
   begin
      API.Get_Integer (Enums.Getter.Minor_Version, Result'Access);
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Minor_Version;

   function Version_String return String is
   begin
      return C.Strings.Value (API.Get_String (Enums.Getter.Version));
   end Version_String;

   function Vendor return String is
   begin
      return C.Strings.Value (API.Get_String (Enums.Getter.Vendor));
   end Vendor;

   function Renderer return String is
   begin
      return C.Strings.Value (API.Get_String (Enums.Getter.Renderer));
   end Renderer;

   function Extensions return String_List is
      use Ada.Strings.Unbounded;
      use type Errors.Error_Code;
      Count : aliased Int;
   begin
      API.Get_Integer (Enums.Getter.Num_Extensions, Count'Access);
      Raise_Exception_On_OpenGL_Error;

      pragma Assert (API.Get_Error = Errors.No_Error);
      --  We are on OpenGL 3

      return List : String_List (1 .. Positive (Count)) do
         for I in List'Range loop
            List (I) := To_Unbounded_String (Extension (I));
         end loop;
      end return;
   end Extensions;

   function Has_Extension (Name : String) return Boolean is
      use type Errors.Error_Code;
      Count : aliased Int;
   begin
      API.Get_Integer (Enums.Getter.Num_Extensions, Count'Access);
      Raise_Exception_On_OpenGL_Error;

      pragma Assert (API.Get_Error = Errors.No_Error);
      --  We are on OpenGL 3

      return (for some I in 1 .. Positive (Count) => Extension (I) = Name);
   end Has_Extension;

   function Primary_Shading_Language_Version return String is
      Result : constant String := C.Strings.Value
        (API.Get_String (Enums.Getter.Shading_Language_Version));
   begin
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Primary_Shading_Language_Version;

   function Supported_Shading_Language_Versions return String_List is
      use Ada.Strings.Unbounded;
      use type Errors.Error_Code;
      Count : aliased Int;
   begin
      API.Get_Integer (Enums.Getter.Num_Shading_Language_Versions, Count'Access);
      if API.Get_Error = Errors.Invalid_Enum then
         raise Feature_Not_Supported_Exception;
      end if;
      return List : String_List (1 .. Positive (Count)) do
         for I in List'Range loop
            List (I) := To_Unbounded_String (GLSL_Version (I));
         end loop;
      end return;
   end Supported_Shading_Language_Versions;

   function Supports_Shading_Language_Version (Name : String) return Boolean is
      Count : aliased Int;
   begin
      API.Get_Integer (Enums.Getter.Num_Shading_Language_Versions, Count'Access);
      Raise_Exception_On_OpenGL_Error;

      return (for some I in 1 .. Positive (Count) => GLSL_Version (I) = Name);
   end Supports_Shading_Language_Version;

end GL.Context;
