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

with Ada.Unchecked_Conversion;

with EGL.API;

package body EGL.Loading is

   generic
      type Function_Reference is private;
   function Load (Function_Name : String) return Function_Reference;

   function Load (Function_Name : String) return Function_Reference is
      function As_Function_Reference is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => Function_Reference);
   begin
      return As_Function_Reference (API.Get_Proc_Address (C.To_C (Function_Name)));
   end Load;

   package body Function_With_2_Params is
      function Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type) return Return_Type
      is
         function Load_Function is new Load (Function_Reference);
      begin
         Ref := Load_Function (Function_Name);
         return Ref (Param1, Param2);
      end Init;
   end Function_With_2_Params;

   package body Function_With_3_Params is
      function Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type) return Return_Type
      is
         function Load_Function is new Load (Function_Reference);
      begin
         Ref := Load_Function (Function_Name);
         return Ref (Param1, Param2, Param3);
      end Init;
   end Function_With_3_Params;

   package body Array_Getter_With_3_Params is
      function Init
        (Param1 : Param1_Type;
         Values : in out Array_Type;
         Size   : in out Size_Type) return Return_Type
      is
         function Load_Function is new Load (Function_Reference);
      begin
         Ref := Load_Function (Function_Name);
         return Ref (Param1, Values, Size);
      end Init;
   end Array_Getter_With_3_Params;

   package body Getter_With_3_Params is
      function Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : out Param3_Type) return Return_Type
      is
         function Load_Function is new Load (Function_Reference);
      begin
         Ref := Load_Function (Function_Name);
         return Ref (Param1, Param2, Param3);
      end Init;
   end Getter_With_3_Params;

end EGL.Loading;
