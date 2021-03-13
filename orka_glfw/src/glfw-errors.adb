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

with Interfaces.C.Strings;

with Glfw.API;

package body Glfw.Errors is

   Cur_Callback : Callback := null;

   procedure Raw_Handler (Code : Kind;
                          Description : Interfaces.C.Strings.chars_ptr);
   pragma Convention (C, Raw_Handler);

   procedure Raw_Handler (Code : Kind;
                          Description : Interfaces.C.Strings.chars_ptr) is
   begin
      if Cur_Callback /= null then
         Cur_Callback.all (Code, Interfaces.C.Strings.Value (Description));
      end if;
   end Raw_Handler;

   procedure Set_Callback (Handler : Callback) is
      Previous : API.Error_Callback;

      use type API.Error_Callback;
   begin
      Cur_Callback := Handler;
      if Handler = null then
         Previous := API.Set_Error_Callback (null);
      else
         Previous := API.Set_Error_Callback (Raw_Handler'Access);
      end if;
      pragma Assert (Previous in null | Raw_Handler'Access);
   end Set_Callback;

end Glfw.Errors;
