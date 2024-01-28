--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2023 onox <denkpadje@gmail.com>
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

package body Orka.Rendering.Samplers is

   function Create_Sampler (State : Sampler_State) return Sampler is
   begin
      return Result : Sampler do
         Result.Sampler.Set_X_Wrapping (State.Wrapping (X));
         Result.Sampler.Set_Y_Wrapping (State.Wrapping (Y));
         Result.Sampler.Set_Z_Wrapping (State.Wrapping (Z));

         Result.Sampler.Set_Minifying_Filter (State.Minifying_Filter);
         Result.Sampler.Set_Magnifying_Filter (State.Magnifying_Filter);

         Result.Sampler.Set_Minimum_LoD (State.Minimum_LoD);
         Result.Sampler.Set_Maximum_LoD (State.Maximum_LoD);

         Result.Sampler.Set_LoD_Bias (State.LoD_Bias);
         Result.Sampler.Set_Max_Anisotropy (State.Max_Anisotropy);

         Result.Sampler.Set_Border_Color (State.Border_Color);
         Result.Sampler.Set_Compare_X_To_Texture (State.Compare_X_To_Texture);
         Result.Sampler.Set_Compare_Function (State.Current_Compare_Function);
      end return;
   end Create_Sampler;

   procedure Bind (Object : Sampler; Index : Natural) is
   begin
      Object.Sampler.Bind (Unsigned_32 (Index));
   end Bind;

end Orka.Rendering.Samplers;
