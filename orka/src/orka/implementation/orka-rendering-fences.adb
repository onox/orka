--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

with GL.Debug;

package body Orka.Rendering.Fences is

   use GL.Debug;
   procedure Log is new GL.Debug.Log (Third_Party, Performance);

   function Create_Buffer_Fence return Buffer_Fence is
   begin
      return Result : Buffer_Fence do
         Result.Index := Index_Type'First;
      end return;
   end Create_Buffer_Fence;

   procedure Prepare_Index (Object : in out Buffer_Fence; Status : out Fence_Status) is
      use GL.Fences;
   begin
      if not Object.Fences (Object.Index).Initialized then
         Status := Not_Initialized;
         return;
      end if;

      case Object.Fences (Object.Index).Client_Wait (Maximum_Wait) is
         when Condition_Satisfied =>
            Log (Medium, "Fence not already signalled");
            Status := Signaled;
         when Timeout_Expired | Wait_Failed =>
            Log (High, "Fence timed out or failed");
            Status := Not_Signaled;
         when Already_Signaled =>
            Status := Signaled;
      end case;
   end Prepare_Index;

   procedure Advance_Index (Object : in out Buffer_Fence) is
   begin
      Object.Fences (Object.Index).Set_Fence;
      Object.Index := Object.Index + 1;
   end Advance_Index;

end Orka.Rendering.Fences;
