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

with EGL.API;
with EGL.Objects.Displays;

package body EGL.Debug is

   Current_Callback : Callback_Reference := null;

   procedure Debug_Callback
     (Error     : Errors.Error_Code;
      C_Command : C.Strings.chars_ptr;
      Kind      : Severity;
      Label     : System.Address;
      Object    : System.Address;
      C_Message : C.Strings.chars_ptr)
   with Convention => C;

   procedure Debug_Callback
     (Error     : Errors.Error_Code;
      C_Command : C.Strings.chars_ptr;
      Kind      : Severity;
      Label     : System.Address;
      Object    : System.Address;
      C_Message : C.Strings.chars_ptr)
   is
      pragma Unreferenced (Label);
      pragma Unreferenced (Object);

      Command : constant String := C.Strings.Value (C_Command);
      Message : constant String := C.Strings.Value (C_Message);
   begin
      if Current_Callback /= null then
         Current_Callback (Error, Kind, Command, Message);
      end if;
   end Debug_Callback;

   procedure Set_Message_Callback (Callback : Callback_Reference) is
      Critical : constant Attrib := 16#33B9#;
      Error    : constant Attrib := 16#33BA#;
      Warning  : constant Attrib := 16#33BB#;
      Info     : constant Attrib := 16#33BC#;

      Attributes : constant Attribute_Array :=
        [Critical, 1, Error, 1, Warning, 1, Info, 1, None];

      use type EGL.Errors.Error_Code;
   begin
      Check_Extension (EGL.Objects.Displays.Client_Extensions, "EGL_KHR_debug");

      if API.Debug_Message_Control.Ref ((if Callback /= null then
                                           Debug_Callback'Address
                                         else
                                           System.Null_Address), Attributes) = EGL.Errors.Success
      then
         Current_Callback := Callback;
      else
         Errors.Raise_Exception_On_EGL_Error;
      end if;
   end Set_Message_Callback;

end EGL.Debug;
