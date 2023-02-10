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

with Ada.Unchecked_Conversion;

package body GL.Runtime_Loading is

   generic
      type Function_Reference is private;
   function Load (Function_Name : String) return Function_Reference;

   function Load (Function_Name : String) return Function_Reference is
      function As_Function_Reference is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => Function_Reference);
   begin
      return As_Function_Reference (Raw_Subprogram_Reference (Function_Name));
   end Load;

   package body Function_Without_Params is
      function Init return Return_Type is
         function Load_Function is new Load (Function_Reference);
      begin
         Ref := Load_Function (Function_Name);
         return Ref.all;
      end Init;
   end Function_Without_Params;

   package body Function_With_1_Param is
      function Init (Param1 : Param1_Type) return Return_Type is
         function Load_Function is new Load (Function_Reference);
      begin
         Ref := Load_Function (Function_Name);
         return Ref (Param1);
      end Init;
   end Function_With_1_Param;

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

   package body Function_With_4_Params is
      function Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type) return Return_Type
      is
         function Load_Function is new Load (Function_Reference);
      begin
         Ref := Load_Function (Function_Name);
         return Ref (Param1, Param2, Param3, Param4);
      end Init;
   end Function_With_4_Params;

   package body Function_With_8_Params is
      function Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type;
         Param6 : Param6_Type;
         Param7 : Param7_Type;
         Param8 : Param8_Type) return Return_Type
      is
         function Load_Function is new Load (Function_Reference);
      begin
         Ref := Load_Function (Function_Name);
         return Ref (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8);
      end Init;
   end Function_With_8_Params;

   package body Array_Getter_With_5_Params is
      type Procedure_Reference is not null access procedure
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Max    : Types.Size;
         Size   : in out Types.Size;
         Values : in out Array_Type)
      with Convention => StdCall;

      procedure Internal_Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Max    : Types.Size;
         Size   : in out Types.Size;
         Values : in out Array_Type)
      with Convention => StdCall;

      Internal_Ref : Procedure_Reference := Internal_Init'Access;

      procedure Internal_Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Max    : Types.Size;
         Size   : in out Types.Size;
         Values : in out Array_Type)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Internal_Ref := Load_Procedure (Procedure_Name);
         Internal_Ref (Param1, Param2, Max, Size, Values);
      end Internal_Init;

      function Ref
        (Param1   : Param1_Type;
         Param2   : Param2_Type;
         Max_Size : Types.Size) return Array_Type
      is
         Actual_Size : Types.Size := 0;

         Result : Array_Type (1 .. Max_Size);
      begin
         Internal_Ref (Param1, Param2, Max_Size, Actual_Size, Result);
         return (if Actual_Size /= Max_Size then Result (1 .. Actual_Size) else Result);
      end Ref;
   end Array_Getter_With_5_Params;

   package body Array_Getter_With_8_Params is
      type Procedure_Reference is not null access procedure
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type;
         Max    : Types.Size;
         Size   : in out Types.Size;
         Values : in out Array_Type)
      with Convention => StdCall;

      procedure Internal_Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type;
         Max    : Types.Size;
         Size   : in out Types.Size;
         Values : in out Array_Type)
      with Convention => StdCall;

      Internal_Ref : Procedure_Reference := Internal_Init'Access;

      procedure Internal_Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type;
         Max    : Types.Size;
         Size   : in out Types.Size;
         Values : in out Array_Type)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Internal_Ref := Load_Procedure (Procedure_Name);
         Internal_Ref (Param1, Param2, Param3, Param4, Param5, Max, Size, Values);
      end Internal_Init;

      function Ref
        (Param1   : Param1_Type;
         Param2   : Param2_Type;
         Param3   : Param3_Type;
         Param4   : Param4_Type;
         Param5   : Param5_Type;
         Max_Size : Types.Size) return Array_Type
      is
         Actual_Size : Types.Size := 0;

         Result : Array_Type (1 .. Max_Size);
      begin
         Internal_Ref (Param1, Param2, Param3, Param4, Param5, Max_Size, Actual_Size, Result);
         return (if Actual_Size /= Max_Size then Result (1 .. Actual_Size) else Result);
      end Ref;
   end Array_Getter_With_8_Params;

   package body Procedure_Without_Params is
      procedure Init is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref.all;
      end Init;
   end Procedure_Without_Params;

   package body Procedure_With_1_Param is
      procedure Init (Param1 : Param1_Type) is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref (Param1);
      end Init;
   end Procedure_With_1_Param;

   package body Procedure_With_2_Params is
      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref (Param1, Param2);
      end Init;
   end Procedure_With_2_Params;

   package body Procedure_With_3_Params is
      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref (Param1, Param2, Param3);
      end Init;
   end Procedure_With_3_Params;

   package body Procedure_With_4_Params is
      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref (Param1, Param2, Param3, Param4);
      end Init;
   end Procedure_With_4_Params;

   package body Procedure_With_5_Params is
      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref (Param1, Param2, Param3, Param4, Param5);
      end Init;
   end Procedure_With_5_Params;

   package body Procedure_With_6_Params is
      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type;
         Param6 : Param6_Type)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref (Param1, Param2, Param3, Param4, Param5, Param6);
      end Init;
   end Procedure_With_6_Params;

   package body Procedure_With_7_Params is
      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type;
         Param6 : Param6_Type;
         Param7 : Param7_Type)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref (Param1, Param2, Param3, Param4, Param5, Param6, Param7);
      end Init;
   end Procedure_With_7_Params;

   package body Procedure_With_8_Params is
      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type;
         Param6 : Param6_Type;
         Param7 : Param7_Type;
         Param8 : Param8_Type)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref (Param1, Param2, Param3, Param4, Param5, Param6, Param7,
              Param8);
      end Init;
   end Procedure_With_8_Params;

   package body Procedure_With_9_Params is
      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type;
         Param6 : Param6_Type;
         Param7 : Param7_Type;
         Param8 : Param8_Type;
         Param9 : Param9_Type)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref (Param1, Param2, Param3, Param4, Param5, Param6, Param7,
              Param8, Param9);
      end Init;
   end Procedure_With_9_Params;

   package body Procedure_With_10_Params is
      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type;
         Param6 : Param6_Type;
         Param7 : Param7_Type;
         Param8 : Param8_Type;
         Param9 : Param9_Type;
         Param10 : Param10_Type)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref (Param1, Param2, Param3, Param4, Param5, Param6, Param7,
              Param8, Param9, Param10);
      end Init;
   end Procedure_With_10_Params;

   package body Procedure_With_11_Params is
      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type;
         Param6 : Param6_Type;
         Param7 : Param7_Type;
         Param8 : Param8_Type;
         Param9 : Param9_Type;
         Param10 : Param10_Type;
         Param11 : Param11_Type)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref (Param1, Param2, Param3, Param4, Param5, Param6, Param7,
              Param8, Param9, Param10, Param11);
      end Init;
   end Procedure_With_11_Params;

   package body Procedure_With_12_Params is
      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type;
         Param6 : Param6_Type;
         Param7 : Param7_Type;
         Param8 : Param8_Type;
         Param9 : Param9_Type;
         Param10 : Param10_Type;
         Param11 : Param11_Type;
         Param12 : Param12_Type)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref (Param1, Param2, Param3, Param4, Param5, Param6, Param7,
              Param8, Param9, Param10, Param11, Param12);
      end Init;
   end Procedure_With_12_Params;

   package body Procedure_With_15_Params is
      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type;
         Param6 : Param6_Type;
         Param7 : Param7_Type;
         Param8 : Param8_Type;
         Param9 : Param9_Type;
         Param10 : Param10_Type;
         Param11 : Param11_Type;
         Param12 : Param12_Type;
         Param13 : Param13_Type;
         Param14 : Param14_Type;
         Param15 : Param15_Type)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref (Param1, Param2, Param3, Param4, Param5, Param6, Param7,
              Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15);
      end Init;
   end Procedure_With_15_Params;

   package body Array_Proc_With_2_Params is
      procedure Init
        (Param1 : Size_Type;
         Param2 : Array_Type)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref (Param1, Param2);
      end Init;
   end Array_Proc_With_2_Params;

   package body Array_Proc_With_3_Params is
      procedure Init
        (Param1 : Param1_Type;
         Param2 : Size_Type;
         Param3 : Array_Type)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref (Param1, Param2, Param3);
      end Init;
   end Array_Proc_With_3_Params;

   package body Getter_With_2_Params is
      procedure Init
        (Param1 : Param1_Type;
         Value  : in out Value_Type)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref (Param1, Value);
      end Init;
   end Getter_With_2_Params;

   package body Getter_With_3_Params is
      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Value  : in out Value_Type)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref (Param1, Param2, Value);
      end Init;
   end Getter_With_3_Params;

   package body Getter_With_4_Params is
      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Value  : in out Value_Type)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref (Param1, Param2, Param3, Value);
      end Init;
   end Getter_With_4_Params;

   package body Getter_With_5_Params is
      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Value  : in out Value_Type)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref (Param1, Param2, Param3, Param4, Value);
      end Init;
   end Getter_With_5_Params;

   package body String_Getter_With_4_Params is
      procedure Init
        (Param1      : Param1_Type;
         Buffer_Size : Size_Type;
         Length      : out Size_Type;
         Value       : in out String)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref (Param1, Buffer_Size, Length, Value);
      end Init;
   end String_Getter_With_4_Params;

   package body String_Getter_With_5_Params is
      procedure Init
        (Param1      : Param1_Type;
         Param2      : Param2_Type;
         Buffer_Size : Size_Type;
         Length      : out Size_Type;
         Value       : in out String)
      is
         function Load_Procedure is new Load (Procedure_Reference);
      begin
         Ref := Load_Procedure (Procedure_Name);
         Ref (Param1, Param2, Buffer_Size, Length, Value);
      end Init;
   end String_Getter_With_5_Params;

end GL.Runtime_Loading;
