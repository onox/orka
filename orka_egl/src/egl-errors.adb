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

package body EGL.Errors is

   procedure Raise_Exception_On_EGL_Error is
      Error : constant Error_Code := API.Get_Error;
   begin
      case Error is
         when Success =>
            null;
         when Not_Initialized =>
            raise Not_Initialized_Error with Error'Image;
         when Context_Lost =>
            raise Context_Lost_Error with Error'Image;
         when Bad_Access | Bad_Alloc =>
            raise Invalid_Operation_Error with Error'Image;
         when Bad_Attribute =>
            raise Internal_Error with Error'Image;
         when Bad_Config | Bad_Context | Bad_Current_Surface | Bad_Surface
           | Bad_Display | Bad_Device | Bad_Native_Pixmap | Bad_Native_Window =>
            raise Invalid_Operation_Error with Error'Image;
         when Bad_Match | Bad_Parameter =>
            raise Invalid_Value_Error with Error'Image;
      end case;
   exception
      when Constraint_Error => raise Internal_Error;
   end Raise_Exception_On_EGL_Error;

end EGL.Errors;
