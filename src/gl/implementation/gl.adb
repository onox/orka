--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2012 Felix Krause <contact@flyx.org>
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

with GL.API;
with GL.Errors;

package body GL is
   
   procedure Flush renames API.Flush;

   procedure Finish renames API.Finish;

   procedure Raise_Exception_On_OpenGL_Error is
   begin
      case Errors.Error_Flag is
         when Errors.Invalid_Operation => raise Errors.Invalid_Operation_Error;
         when Errors.Invalid_Value => raise Errors.Invalid_Value_Error;
         when Errors.Invalid_Framebuffer_Operation =>
            raise Errors.Invalid_Framebuffer_Operation_Error;
         when Errors.Out_Of_Memory => raise Errors.Out_Of_Memory_Error;
         when Errors.Stack_Overflow => raise Errors.Stack_Overflow_Error;
         when Errors.Stack_Underflow => raise Errors.Stack_Underflow_Error;
         when Errors.Invalid_Enum => raise Errors.Internal_Error;
         when Errors.No_Error => null;
      end case;
   exception
      when Constraint_Error => raise Errors.Internal_Error;
   end Raise_Exception_On_OpenGL_Error;

end GL;
