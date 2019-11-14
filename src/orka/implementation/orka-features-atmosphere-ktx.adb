--  SPDX-License-Identifier: BSD-3-Clause
--
--  Copyright (c) 2019 onox <denkpadje@gmail.com>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--  1. Redistributions of source code must retain the above copyright
--     notice, this list of conditions and the following disclaimer.
--  2. Redistributions in binary form must reproduce the above copyright
--     notice, this list of conditions and the following disclaimer in the
--     documentation and/or other materials provided with the distribution.
--  3. Neither the name of the copyright holders nor the names of its
--     contributors may be used to endorse or promote products derived from
--     this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
--  THE POSSIBILITY OF SUCH DAMAGE.

with Orka.Resources.Textures.KTX;

package body Orka.Features.Atmosphere.KTX is

   function Load_Textures
     (Data     : Model_Data;
      Location : Resources.Locations.Location_Ptr) return Precomputed_Textures is
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
      Location : Resources.Locations.Writable_Location_Ptr) is
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
