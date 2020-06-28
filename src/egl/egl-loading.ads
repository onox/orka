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

private package EGL.Loading is
   pragma Preelaborate;

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
      type Element_Type is private;
      type Array_Type is array (Natural range <>) of Element_Type;
      type Size_Type (<>) is private;
      type Return_Type is private;
   package Array_Getter_With_3_Params is
      type Function_Reference is not null access function
        (Param1 : Param1_Type;
         Values : in out Array_Type;
         Size   : in out Size_Type) return Return_Type
      with Convention => StdCall;

      function Init
        (Param1 : Param1_Type;
         Values : in out Array_Type;
         Size   : in out Size_Type) return Return_Type
      with Convention => StdCall;

      Ref : Function_Reference := Init'Access;
   end Array_Getter_With_3_Params;

   generic
      Function_Name : String;
      type Param1_Type (<>) is private;
      type Param2_Type (<>) is private;
      type Param3_Type (<>) is private;
      type Return_Type is private;
   package Getter_With_3_Params is
      type Function_Reference is not null access function
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : out Param3_Type) return Return_Type
      with Convention => StdCall;

      function Init
        (Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : out Param3_Type) return Return_Type
      with Convention => StdCall;

      Ref : Function_Reference := Init'Access;
   end Getter_With_3_Params;

end EGL.Loading;
