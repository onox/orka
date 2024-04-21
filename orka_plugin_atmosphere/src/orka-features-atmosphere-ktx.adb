--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2019 onox <denkpadje@gmail.com>
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

with Orka.Resources.Textures.KTX;

package body Orka.Features.Atmosphere.KTX is

   function Load_Textures
     (Data     : Model_Data;
      Location : Orka.Resources.Locations.Location_Ptr) return Precomputed_Textures is
   begin
      return Result : Precomputed_Textures :=
        (Sampler            => Create_Sampler,
         Combine_Scattering => Data.Combine_Scattering_Textures,
         others             => <>)
      do
         Result.Irradiance_Texture := Orka.Resources.Textures.KTX.Read_Texture
           (Location, "irradiance.ktx");

         Result.Transmittance_Texture := Orka.Resources.Textures.KTX.Read_Texture
           (Location, "transmittance.ktx");

         Result.Scattering_Texture := Orka.Resources.Textures.KTX.Read_Texture
           (Location, "scattering.ktx");

         if not Result.Combine_Scattering then
            Result.Optional_Single_Mie_Scattering_Texture
              := Orka.Resources.Textures.KTX.Read_Texture (Location, "single_mie_scattering.ktx");
         end if;
      end return;
   end Load_Textures;

   procedure Save_Textures
     (Object   : Precomputed_Textures;
      Location : Orka.Resources.Locations.Writable_Location_Ptr) is
   begin
      Orka.Resources.Textures.KTX.Write_Texture
        (Object.Transmittance_Texture, Location, "transmittance.ktx");
      Orka.Resources.Textures.KTX.Write_Texture
        (Object.Scattering_Texture, Location, "scattering.ktx");
      Orka.Resources.Textures.KTX.Write_Texture
        (Object.Irradiance_Texture, Location, "irradiance.ktx");

      if not Object.Combine_Scattering then
         Orka.Resources.Textures.KTX.Write_Texture
           (Object.Optional_Single_Mie_Scattering_Texture, Location, "single_mie_scattering.ktx");
      end if;
   end Save_Textures;

end Orka.Features.Atmosphere.KTX;
