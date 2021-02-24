--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2020 onox <denkpadje@gmail.com>
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

private with Interfaces.C.Strings;

with Interfaces.C;

with System;

with Ada.Strings.Unbounded;

package EGL is
   pragma Preelaborate;

   package C renames Interfaces.C;

   type Bool is new Boolean;

   subtype Int is C.int;

   subtype Enum is C.unsigned;

   subtype Attrib is C.ptrdiff_t;

   type ID_Type is private;

   function None return Int;
   function None return Attrib;

   type Enum_Array is array (Natural range <>) of Enum
     with Convention => C;

   type Attribute_Array is array (Natural range <>) of Attrib
     with Convention => C;

   type Int_Array is array (Natural range <>) of Int
     with Convention => C;

   package SU renames Ada.Strings.Unbounded;

   type String_List is array (Positive range <>) of SU.Unbounded_String;

   function Has_Extension (Extensions : String_List; Name : String) return Boolean;
   --  Return True if the extension with the given name can be found in
   --  the list of extensions, False otherwise

   procedure Check_Extension (Extensions : String_List; Name : String);
   --  Raise Feature_Not_Supported if the extension with the given
   --  name cannot be found in the list of extensions

   Feature_Not_Supported : exception;
   --  Raised when a function that is not available is called

   type Native_Window_Ptr is private;

private

   type Native_Window is limited null record;

   type Native_Window_Ptr is access all Native_Window
     with Convention => C;

   function None return Int is (16#3038#);
   function None return Attrib is (16#3038#);

   type Void_Ptr is new System.Address;

   type ID_Type is new Void_Ptr;

   type ID_Array is array (Natural range <>) of ID_Type
     with Convention => C;

   for Bool use (False => 0, True => 1);
   for Bool'Size use C.unsigned'Size;

   function Trim (Value : C.Strings.chars_ptr) return String;

   function Extensions (Value : C.Strings.chars_ptr) return String_List;

   type Display_Query_Param is (Vendor, Version, Extensions, Client_APIs);

   for Display_Query_Param use
     (Vendor      => 16#3053#,
      Version     => 16#3054#,
      Extensions  => 16#3055#,
      Client_APIs => 16#308D#);
   for Display_Query_Param'Size use Int'Size;

   type Context_Query_Param is (Render_Buffer);

   for Context_Query_Param use
     (Render_Buffer => 16#3086#);
   for Context_Query_Param'Size use Int'Size;

   type Surface_Query_Param is (Height, Width, Swap_Behavior);

   for Surface_Query_Param use
     (Height        => 16#3056#,
      Width         => 16#3057#,
      Swap_Behavior => 16#3093#);
   for Surface_Query_Param'Size use Int'Size;

   type Device_Query_Param is (Extensions, DRM_Device_File);

   for Device_Query_Param use
     (Extensions      => 16#3055#,
      DRM_Device_File => 16#3233#);
   for Device_Query_Param'Size use Int'Size;

end EGL;
