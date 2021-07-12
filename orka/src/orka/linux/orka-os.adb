--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

with Ada.Characters.Latin_1;

package body Orka.OS is

   procedure Set_Task_Name (Name : in String) is
      use Interfaces.C;

      PR_SET_NAME : constant := 15;

      function prctl
        (option : int;
         arg2   : Strings.chars_ptr;
         arg3, arg4, arg5 : unsigned_long := 0) return int
      with Import, Convention => C, External_Name => "prctl";

      C_Name_Str : Strings.chars_ptr := Strings.New_String (Name);
      Result     : int;
   begin
      Result := prctl (PR_SET_NAME, C_Name_Str);
      Strings.Free (C_Name_Str);

      pragma Assert (Result = 0);
   end Set_Task_Name;

   ----------------------------------------------------------------------------

   type Clock_Kind is (Realtime, Monotonic);

   for Clock_Kind use
     (Realtime  => 0,
      Monotonic => 1);
   for Clock_Kind'Size use Interfaces.C.int'Size;

   type Timespec is record
      Seconds     : aliased Interfaces.C.long;
      Nanoseconds : aliased Interfaces.C.long;
   end record
     with Convention => C;

   function C_Clock_Gettime
     (Kind : Clock_Kind;
      Time : access Timespec) return Interfaces.C.int
   with Import, Convention => C, External_Name => "clock_gettime";

   function Monotonic_Clock return Duration is
      use type Interfaces.C.int;

      Value  : aliased Timespec;
      Result : Interfaces.C.int;
   begin
      Result := C_Clock_Gettime (Monotonic, Value'Access);

      pragma Assert (Result = 0);
      --  Makes compiler happy and can be optimized away (unlike raise Program_Error)

      return Duration (Value.Seconds) + Duration (Value.Nanoseconds) / 1e9;
   end Monotonic_Clock;

   function Monotonic_Clock return Time is (Time (Duration'(Monotonic_Clock)));

   ----------------------------------------------------------------------------

   subtype Size_Type is Interfaces.C.unsigned_long;

   procedure C_Fwrite
     (Value : String;
      Size  : Size_Type;
      Count : Size_Type;
      File  : System.Address)
   with Import, Convention => C, External_Name => "fwrite";

   File_Standard_Output : constant System.Address
     with Import, Convention => C, External_Name => "stdout";

   File_Standard_Error : constant System.Address
     with Import, Convention => C, External_Name => "stderr";

   procedure Put_Line (Value : String; Kind : File_Kind := Standard_Output) is
      package L1 renames Ada.Characters.Latin_1;

      C_Value : constant String := Value & L1.LF;
   begin
      C_Fwrite (C_Value, 1, C_Value'Length,
        (case Kind is
           when Standard_Output => File_Standard_Output,
           when Standard_Error  => File_Standard_Error));
   end Put_Line;

end Orka.OS;
