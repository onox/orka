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

with System;

with GL.Types;

private generic
   with function Raw_Subprogram_Reference (Name : String) return System.Address;
package GL.Runtime_Loading is
   pragma Preelaborate;

   --  This package loads the raw API function pointers at runtime. Some GL
   --  implementations may return a pointer even when the feature is not
   --  supported. Therefore, the only reliable way on all platforms to find
   --  out whether a feature is supported is by using the GL.Context package.

   generic
      Function_Name : String;
      type Return_Type is private;
   package Function_Without_Params is
      type Function_Reference is not null access function return Return_Type
        with Convention => StdCall;

      function Init return Return_Type
        with Convention => StdCall;

      Ref : Function_Reference := Init'Access;
   end Function_Without_Params;

   generic
      Function_Name : String;
      type Param1_Type (<>) is private;
      type Return_Type is private;
   package Function_With_1_Param is
      type Function_Reference is not null access function
        (Param1 : Param1_Type) return Return_Type
      with Convention => StdCall;

      function Init (Param1 : Param1_Type) return Return_Type
        with Convention => StdCall;

      Ref : Function_Reference := Init'Access;
   end Function_With_1_Param;

   generic
      Function_Name : String;
      type Param1_Type (<>) is private;
      type Param2_Type (<>) is private;
      type Return_Type is private;
   package Function_With_2_Params is
      type Function_Reference is not null access function
        (Param1 : Param1_Type;
         Param2 : Param2_Type) return Return_Type
      with Convention => StdCall;

      function Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type) return Return_Type
      with Convention => StdCall;

      Ref : Function_Reference := Init'Access;
   end Function_With_2_Params;

   generic
      Function_Name : String;
      type Param1_Type (<>) is private;
      type Param2_Type (<>) is private;
      type Param3_Type (<>) is private;
      type Return_Type is private;
   package Function_With_3_Params is
      type Function_Reference is not null access function
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type) return Return_Type
      with Convention => StdCall;

      function Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type) return Return_Type
      with Convention => StdCall;

      Ref : Function_Reference := Init'Access;
   end Function_With_3_Params;

   generic
      Function_Name : String;
      type Param1_Type (<>) is private;
      type Param2_Type (<>) is private;
      type Param3_Type (<>) is private;
      type Param4_Type (<>) is private;
      type Return_Type is private;
   package Function_With_4_Params is
      type Function_Reference is not null access function
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type) return Return_Type
      with Convention => StdCall;

      function Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type) return Return_Type
      with Convention => StdCall;

      Ref : Function_Reference := Init'Access;
   end Function_With_4_Params;

   generic
      Function_Name : String;
      type Param1_Type (<>) is private;
      type Param2_Type (<>) is private;
      type Param3_Type (<>) is private;
      type Param4_Type (<>) is private;
      type Param5_Type (<>) is private;
      type Param6_Type (<>) is private;
      type Param7_Type (<>) is private;
      type Param8_Type (<>) is private;
      type Return_Type is private;
   package Function_With_8_Params is
      type Function_Reference is not null access function
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type;
         Param6 : Param6_Type;
         Param7 : Param7_Type;
         Param8 : Param8_Type) return Return_Type
      with Convention => StdCall;

      function Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type;
         Param6 : Param6_Type;
         Param7 : Param7_Type;
         Param8 : Param8_Type) return Return_Type
      with Convention => StdCall;

      Ref : Function_Reference := Init'Access;
   end Function_With_8_Params;

   generic
      Procedure_Name : String;
   package Procedure_Without_Params is
      type Procedure_Reference is not null access procedure
        with Convention => StdCall;

      procedure Init
        with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end Procedure_Without_Params;

   generic
      Procedure_Name : String;
      type Param1_Type (<>) is private;
   package Procedure_With_1_Param is
      type Procedure_Reference is not null access procedure
        (Param1 : Param1_Type)
      with Convention => StdCall;

      procedure Init (Param1 : Param1_Type)
        with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end Procedure_With_1_Param;

   generic
      Procedure_Name : String;
      type Param1_Type (<>) is private;
      type Param2_Type (<>) is private;
   package Procedure_With_2_Params is
      type Procedure_Reference is not null access procedure
        (Param1 : Param1_Type;
         Param2 : Param2_Type)
      with Convention => StdCall;

      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type)
      with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end Procedure_With_2_Params;

   generic
      Procedure_Name : String;
      type Param1_Type (<>) is private;
      type Param2_Type (<>) is private;
      type Param3_Type (<>) is private;
   package Procedure_With_3_Params is
      type Procedure_Reference is not null access procedure
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type)
      with Convention => StdCall;

      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type)
      with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end Procedure_With_3_Params;

   generic
      Procedure_Name : String;
      type Param1_Type (<>) is private;
      type Param2_Type (<>) is private;
      type Param3_Type (<>) is private;
      type Param4_Type (<>) is private;
   package Procedure_With_4_Params is
      type Procedure_Reference is not null access procedure
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type)
      with Convention => StdCall;

      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type)
      with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end Procedure_With_4_Params;

   generic
      Procedure_Name : String;
      type Param1_Type (<>) is private;
      type Param2_Type (<>) is private;
      type Param3_Type (<>) is private;
      type Param4_Type (<>) is private;
      type Param5_Type (<>) is private;
   package Procedure_With_5_Params is
      type Procedure_Reference is not null access procedure
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type)
      with Convention => StdCall;

      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type)
      with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end Procedure_With_5_Params;

   generic
      Procedure_Name : String;
      type Param1_Type (<>) is private;
      type Param2_Type (<>) is private;
      type Param3_Type (<>) is private;
      type Param4_Type (<>) is private;
      type Param5_Type (<>) is private;
      type Param6_Type (<>) is private;
   package Procedure_With_6_Params is
      type Procedure_Reference is not null access procedure
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type;
         Param6 : Param6_Type)
      with Convention => StdCall;

      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type;
         Param6 : Param6_Type)
      with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end Procedure_With_6_Params;

   generic
      Procedure_Name : String;
      type Param1_Type (<>) is private;
      type Param2_Type (<>) is private;
      type Param3_Type (<>) is private;
      type Param4_Type (<>) is private;
      type Param5_Type (<>) is private;
      type Param6_Type (<>) is private;
      type Param7_Type (<>) is private;
   package Procedure_With_7_Params is
      type Procedure_Reference is not null access procedure
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type;
         Param6 : Param6_Type;
         Param7 : Param7_Type)
      with Convention => StdCall;

      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type;
         Param6 : Param6_Type;
         Param7 : Param7_Type)
      with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end Procedure_With_7_Params;

   generic
      Procedure_Name : String;
      type Param1_Type (<>) is private;
      type Param2_Type (<>) is private;
      type Param3_Type (<>) is private;
      type Param4_Type (<>) is private;
      type Param5_Type (<>) is private;
      type Param6_Type (<>) is private;
      type Param7_Type (<>) is private;
      type Param8_Type (<>) is private;
   package Procedure_With_8_Params is
      type Procedure_Reference is not null access procedure
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type;
         Param6 : Param6_Type;
         Param7 : Param7_Type;
         Param8 : Param8_Type)
      with Convention => StdCall;

      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type;
         Param6 : Param6_Type;
         Param7 : Param7_Type;
         Param8 : Param8_Type)
      with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end Procedure_With_8_Params;

   generic
      Procedure_Name : String;
      type Param1_Type (<>) is private;
      type Param2_Type (<>) is private;
      type Param3_Type (<>) is private;
      type Param4_Type (<>) is private;
      type Param5_Type (<>) is private;
      type Param6_Type (<>) is private;
      type Param7_Type (<>) is private;
      type Param8_Type (<>) is private;
      type Param9_Type (<>) is private;
   package Procedure_With_9_Params is
      type Procedure_Reference is not null access procedure
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Param5 : Param5_Type;
         Param6 : Param6_Type;
         Param7 : Param7_Type;
         Param8 : Param8_Type;
         Param9 : Param9_Type)
      with Convention => StdCall;

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
      with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end Procedure_With_9_Params;

   generic
      Procedure_Name : String;
      type Param1_Type (<>) is private;
      type Param2_Type (<>) is private;
      type Param3_Type (<>) is private;
      type Param4_Type (<>) is private;
      type Param5_Type (<>) is private;
      type Param6_Type (<>) is private;
      type Param7_Type (<>) is private;
      type Param8_Type (<>) is private;
      type Param9_Type (<>) is private;
      type Param10_Type (<>) is private;
   package Procedure_With_10_Params is
      type Procedure_Reference is not null access procedure
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
      with Convention => StdCall;

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
      with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end Procedure_With_10_Params;

   generic
      Procedure_Name : String;
      type Param1_Type (<>) is private;
      type Param2_Type (<>) is private;
      type Param3_Type (<>) is private;
      type Param4_Type (<>) is private;
      type Param5_Type (<>) is private;
      type Param6_Type (<>) is private;
      type Param7_Type (<>) is private;
      type Param8_Type (<>) is private;
      type Param9_Type (<>) is private;
      type Param10_Type (<>) is private;
      type Param11_Type (<>) is private;
   package Procedure_With_11_Params is
      type Procedure_Reference is not null access procedure
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
      with Convention => StdCall;

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
      with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end Procedure_With_11_Params;

   generic
      Procedure_Name : String;
      type Param1_Type (<>) is private;
      type Param2_Type (<>) is private;
      type Param3_Type (<>) is private;
      type Param4_Type (<>) is private;
      type Param5_Type (<>) is private;
      type Param6_Type (<>) is private;
      type Param7_Type (<>) is private;
      type Param8_Type (<>) is private;
      type Param9_Type (<>) is private;
      type Param10_Type (<>) is private;
      type Param11_Type (<>) is private;
      type Param12_Type (<>) is private;
   package Procedure_With_12_Params is
      type Procedure_Reference is not null access procedure
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
      with Convention => StdCall;

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
      with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end Procedure_With_12_Params;

   generic
      Procedure_Name : String;
      type Param1_Type (<>) is private;
      type Param2_Type (<>) is private;
      type Param3_Type (<>) is private;
      type Param4_Type (<>) is private;
      type Param5_Type (<>) is private;
      type Param6_Type (<>) is private;
      type Param7_Type (<>) is private;
      type Param8_Type (<>) is private;
      type Param9_Type (<>) is private;
      type Param10_Type (<>) is private;
      type Param11_Type (<>) is private;
      type Param12_Type (<>) is private;
      type Param13_Type (<>) is private;
      type Param14_Type (<>) is private;
      type Param15_Type (<>) is private;
   package Procedure_With_15_Params is
      type Procedure_Reference is not null access procedure
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
      with Convention => StdCall;

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
      with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end Procedure_With_15_Params;

   generic
      Procedure_Name : String;
      type Param1_Type (<>) is private;
      type Param2_Type (<>) is private;
      type Element_Type is private;
      type Array_Type is array (Types.Size range <>) of Element_Type;
   package Array_Getter_With_5_Params is
      function Ref
        (Param1   : Param1_Type;
         Param2   : Param2_Type;
         Max_Size : Types.Size) return Array_Type;
   end Array_Getter_With_5_Params;

   generic
      Procedure_Name : String;
      type Param1_Type (<>) is private;
      type Param2_Type (<>) is private;
      type Param3_Type (<>) is private;
      type Param4_Type (<>) is private;
      type Param5_Type (<>) is private;
      type Element_Type is private;
      type Array_Type is array (Types.Size range <>) of Element_Type;
   package Array_Getter_With_8_Params is
      function Ref
        (Param1   : Param1_Type;
         Param2   : Param2_Type;
         Param3   : Param3_Type;
         Param4   : Param4_Type;
         Param5   : Param5_Type;
         Max_Size : Types.Size) return Array_Type;
   end Array_Getter_With_8_Params;

   generic
      Procedure_Name : String;
      type Size_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Positive range <>) of Element_Type;
   package Array_Proc_With_2_Params is
      type Procedure_Reference is not null access procedure
        (Param1 : Size_Type;
         Param2 : Array_Type)
      with Convention => StdCall;

      procedure Init
        (Param1 : Size_Type;
         Param2 : Array_Type)
      with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end Array_Proc_With_2_Params;

   generic
      Procedure_Name : String;
      type Param1_Type (<>) is private;
      type Size_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Positive range <>) of Element_Type;
   package Array_Proc_With_3_Params is
      type Procedure_Reference is not null access procedure
        (Param1 : Param1_Type;
         Param2 : Size_Type;
         Param3 : Array_Type)
      with Convention => StdCall;

      procedure Init
        (Param1 : Param1_Type;
         Param2 : Size_Type;
         Param3 : Array_Type)
      with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end Array_Proc_With_3_Params;

   generic
      Procedure_Name : String;
      type Param1_Type is private;
      type Value_Type  (<>) is private;
   package Getter_With_2_Params is
      type Procedure_Reference is not null access procedure
        (Param1 : Param1_Type;
         Value  : in out Value_Type)
      with Convention => StdCall;

      procedure Init
        (Param1 : Param1_Type;
         Value  : in out Value_Type)
      with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end Getter_With_2_Params;

   generic
      Procedure_Name : String;
      type Param1_Type is private;
      type Param2_Type is private;
      type Value_Type  (<>) is private;
   package Getter_With_3_Params is
      type Procedure_Reference is not null access procedure
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Value  : in out Value_Type)
      with Convention => StdCall;

      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Value  : in out Value_Type)
      with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end Getter_With_3_Params;

   generic
      Procedure_Name : String;
      type Param1_Type is private;
      type Param2_Type is private;
      type Param3_Type is private;
      type Value_Type  (<>) is private;
   package Getter_With_4_Params is
      type Procedure_Reference is not null access procedure
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Value  : in out Value_Type)
      with Convention => StdCall;

      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Value  : in out Value_Type)
      with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end Getter_With_4_Params;

   generic
      Procedure_Name : String;
      type Param1_Type is private;
      type Param2_Type is private;
      type Param3_Type is private;
      type Param4_Type is private;
      type Value_Type  (<>) is private;
   package Getter_With_5_Params is
      type Procedure_Reference is not null access procedure
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Value : in out Value_Type)
      with Convention => StdCall;

      procedure Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Param4 : Param4_Type;
         Value  : in out Value_Type)
      with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end Getter_With_5_Params;

   generic
      Procedure_Name : String;
      type Size_Type is (<>);
      type Param1_Type is private;
   package String_Getter_With_4_Params is
      type Procedure_Reference is not null access procedure
        (Param1      : Param1_Type;
         Buffer_Size : Size_Type;
         Length      : out Size_Type;
         Value       : in out String)
      with Convention => StdCall;

      procedure Init
        (Param1      : Param1_Type;
         Buffer_Size : Size_Type;
         Length      : out Size_Type;
         Value       : in out String)
      with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end String_Getter_With_4_Params;

   generic
      Procedure_Name : String;
      type Size_Type is (<>);
      type Param1_Type is private;
      type Param2_Type is private;
   package String_Getter_With_5_Params is
      type Procedure_Reference is not null access procedure
        (Param1      : Param1_Type;
         Param2      : Param2_Type;
         Buffer_Size : Size_Type;
         Length      : out Size_Type;
         Value       : in out String)
      with Convention => StdCall;

      procedure Init
        (Param1      : Param1_Type;
         Param2      : Param2_Type;
         Buffer_Size : Size_Type;
         Length      : out Size_Type;
         Value       : in out String)
      with Convention => StdCall;

      Ref : Procedure_Reference := Init'Access;
   end String_Getter_With_5_Params;

end GL.Runtime_Loading;
