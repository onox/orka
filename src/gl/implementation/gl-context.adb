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

with Ada.Unchecked_Conversion;

with GL.API;
with GL.Enums.Getter;
with GL.Errors;
with GL.Types;

package body GL.Context is

   use GL.Types;

   function Extension (Index : Positive) return String is
     (C.Strings.Value (API.Get_String_I.Ref (Enums.Getter.Extensions, UInt (Index - 1))));

   function GLSL_Version (Index : Positive) return String is
     (C.Strings.Value (API.Get_String_I.Ref (Enums.Getter.Shading_Language_Version, UInt (Index - 1))));

   procedure Flush is
   begin
      API.Flush.Ref.all;
   end Flush;

   function Status return Reset_Status is
   begin
      return API.Get_Graphics_Reset_Status.Ref.all;
   end Status;

   function Flags return Context_Flags is
      use type Low_Level.Bitfield;

      function Convert is new Ada.Unchecked_Conversion
        (Low_Level.Bitfield, Context.Context_Flags);

      Raw_Bits : Low_Level.Bitfield := 0;
   begin
      API.Get_Context_Flags.Ref (Enums.Getter.Context_Flags, Raw_Bits);
      return Convert (Raw_Bits and 2#0000000000001111#);
   end Flags;

   function Reset_Notification return Context_Reset_Notification is
      Result : Context_Reset_Notification := No_Reset_Notification;
   begin
      API.Get_Context_Reset_Notification.Ref (Enums.Getter.Context_Reset_Notification, Result);
      return Result;
   end Reset_Notification;

   function Release_Behavior return Context_Release_Behavior is
      Result : Context_Release_Behavior := Flush;
   begin
      API.Get_Context_Release_Behavior.Ref (Enums.Getter.Context_Release_Behavior, Result);
      return Result;
   end Release_Behavior;

   function Major_Version return Natural is
      Result : Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Major_Version, Result);
      return Natural (Result);
   end Major_Version;

   function Minor_Version return Natural is
      Result : Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Minor_Version, Result);
      return Natural (Result);
   end Minor_Version;

   function Version_String return String is
   begin
      return C.Strings.Value (API.Get_String.Ref (Enums.Getter.Version));
   end Version_String;

   function Vendor return String is
   begin
      return C.Strings.Value (API.Get_String.Ref (Enums.Getter.Vendor));
   end Vendor;

   function Renderer return String is
   begin
      return C.Strings.Value (API.Get_String.Ref (Enums.Getter.Renderer));
   end Renderer;

   function Extensions return String_List is
      use Ada.Strings.Unbounded;
      use type Errors.Error_Code;
      Count : Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Num_Extensions, Count);

      pragma Assert (API.Get_Error.Ref.all = Errors.No_Error);
      --  We are on OpenGL 3

      return List : String_List (1 .. Positive (Count)) do
         for I in List'Range loop
            List (I) := To_Unbounded_String (Extension (I));
         end loop;
      end return;
   end Extensions;

   function Has_Extension
     (Extensions : String_List;
      Name       : String) return Boolean
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      return (for some Extension of Extensions => Extension = Name);
   end Has_Extension;

   function Primary_Shading_Language_Version return String is
      Result : constant String := C.Strings.Value
        (API.Get_String.Ref (Enums.Getter.Shading_Language_Version));
   begin
      return Result;
   end Primary_Shading_Language_Version;

   function Supported_Shading_Language_Versions return String_List is
      use Ada.Strings.Unbounded;
      use type Errors.Error_Code;

      Count : Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Num_Shading_Language_Versions, Count);

      if API.Get_Error.Ref.all = Errors.Invalid_Enum then
         raise Feature_Not_Supported_Exception;
      end if;

      return List : String_List (1 .. Positive (Count)) do
         for I in List'Range loop
            List (I) := To_Unbounded_String (GLSL_Version (I));
         end loop;
      end return;
   end Supported_Shading_Language_Versions;

   function Supports_Shading_Language_Version
     (Versions : String_List;
      Name     : String) return Boolean
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      return (for some Version of Versions => Version = Name);
   end Supports_Shading_Language_Version;

end GL.Context;
