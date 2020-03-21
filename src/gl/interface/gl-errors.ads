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

private with GL.Low_Level;

package GL.Errors is
   -- not Pure because Error_Flag can change with each call
   pragma Preelaborate;

   type Error_Code is (No_Error, Invalid_Enum, Invalid_Value, Invalid_Operation,
                       Stack_Overflow, Stack_Underflow, Out_Of_Memory,
                       Invalid_Framebuffer_Operation);

   Invalid_Operation_Error             : exception;
   Out_Of_Memory_Error                 : exception;
   Invalid_Value_Error                 : exception;
   Stack_Overflow_Error                : exception;
   Stack_Underflow_Error               : exception;
   Invalid_Framebuffer_Operation_Error : exception;

   Internal_Error                      : exception;

   procedure Raise_Exception_On_OpenGL_Error
     with Inline;

private

   for Error_Code use (No_Error          => 0,
                       Invalid_Enum      => 16#0500#,
                       Invalid_Value     => 16#0501#,
                       Invalid_Operation => 16#0502#,
                       Stack_Overflow    => 16#0503#,
                       Stack_Underflow   => 16#0504#,
                       Out_Of_Memory     => 16#0505#,
                       Invalid_Framebuffer_Operation => 16#0506#);
   for Error_Code'Size use Low_Level.Enum'Size;

end GL.Errors;
