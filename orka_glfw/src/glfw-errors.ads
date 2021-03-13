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

package Glfw.Errors is
   pragma Preelaborate;

   type Kind is (Not_Initialized,
                 No_Current_Context,
                 Invalid_Enum,
                 Invalid_Value,
                 Out_Of_Memory,
                 API_Unavailable,
                 Version_Unavailable,
                 Platform_Error,
                 Format_Unavailable);

   type Callback is access procedure (Error : Kind; Description : String);

   procedure Set_Callback (Handler : Callback);

private

   for Kind use (Not_Initialized     => 16#00010001#,
                 No_Current_Context  => 16#00010002#,
                 Invalid_Enum        => 16#00010003#,
                 Invalid_Value       => 16#00010004#,
                 Out_Of_Memory       => 16#00010005#,
                 API_Unavailable     => 16#00010006#,
                 Version_Unavailable => 16#00010007#,
                 Platform_Error      => 16#00010008#,
                 Format_Unavailable  => 16#00010009#);
   for Kind'Size use Interfaces.C.int'Size;

end Glfw.Errors;
