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

package EGL.Errors is
   pragma Preelaborate;
   --  Not Pure because Error_Flag can change with each call

   type Error_Code is
     (Success,
      Not_Initialized,
      Bad_Access,
      Bad_Alloc,
      Bad_Attribute,
      Bad_Config,
      Bad_Context,
      Bad_Current_Surface,
      Bad_Display,
      Bad_Match,
      Bad_Native_Pixmap,
      Bad_Native_Window,
      Bad_Parameter,
      Bad_Surface,
      Context_Lost,
      Bad_Device);

   Context_Lost_Error      : exception;
   Not_Initialized_Error   : exception;

   Invalid_Operation_Error : exception;
   Invalid_Value_Error     : exception;

   Internal_Error : exception;

   procedure Raise_Exception_On_EGL_Error
     with Inline;

private

   for Error_Code use
     (Success             => 16#3000#,
      Not_Initialized     => 16#3001#,
      Bad_Access          => 16#3002#,
      Bad_Alloc           => 16#3003#,
      Bad_Attribute       => 16#3004#,
      Bad_Config          => 16#3005#,
      Bad_Context         => 16#3006#,
      Bad_Current_Surface => 16#3007#,
      Bad_Display         => 16#3008#,
      Bad_Match           => 16#3009#,
      Bad_Native_Pixmap   => 16#300A#,
      Bad_Native_Window   => 16#300B#,
      Bad_Parameter       => 16#300C#,
      Bad_Surface         => 16#300D#,
      Context_Lost        => 16#300E#,
      Bad_Device          => 16#322B#);
   for Error_Code'Size use Enum'Size;

end EGL.Errors;
