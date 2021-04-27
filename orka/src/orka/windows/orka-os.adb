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

package body Orka.OS is

   procedure Set_Task_Name (Name : in String) is
   begin
      null;
   end Set_Task_Name;

   function Monotonic_Clock return Duration is
   begin
      raise Program_Error with "BUG: Monotonic_Clock not implemented yet on Windows";
      return 0.0;
   end Monotonic_Clock;

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
