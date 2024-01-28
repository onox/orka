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

with Orka.Logging.Default;

package body Orka.Rendering.Fences is

   use all type Orka.Logging.Default_Module;
   use all type Orka.Logging.Severity;

   procedure Log is new Orka.Logging.Default.Generic_Log (Renderer);

   function Create_Buffer_Fence
     (Regions      : Positive;
      Maximum_Wait : Duration := 0.010) return Buffer_Fence is
   begin
      return Result : Buffer_Fence (Regions => Regions) do
         Result.Index := 0;
         Result.Maximum_Wait := Maximum_Wait;
      end return;
   end Create_Buffer_Fence;

   procedure Prepare_Index (Object : in out Buffer_Fence; Status : out Fence_Status) is
      use GL.Fences;
   begin
      if not Object.Fences (Object.Index + 1).Initialized then
         Status := Not_Initialized;
         return;
      end if;

      case Object.Fences (Object.Index + 1).Client_Wait (Object.Maximum_Wait) is
         when Condition_Satisfied =>
            Log (Warning, "Fence not already signalled");
            Status := Signaled;
         when Timeout_Expired | Wait_Failed =>
            Log (Error, "Fence timed out or failed");
            Status := Not_Signaled;
         when Already_Signaled =>
            Status := Signaled;
      end case;
   end Prepare_Index;

   procedure Advance_Index (Object : in out Buffer_Fence) is
   begin
      Object.Fences (Object.Index + 1).Set_Fence;
      Object.Index := (Object.Index + 1) mod Object.Regions;
   end Advance_Index;

end Orka.Rendering.Fences;
