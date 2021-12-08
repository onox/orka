--  SPDX-License-Identifier: BSD-3-Clause
--
--  Copyright (c) 2017 Eric Bruneton
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

with Ada.Numerics.Generic_Elementary_Functions;

package body Orka.Features.Atmosphere.Earth is

   Half_Precision    : constant Boolean   := True;
   Combined_Textures : constant Boolean   := True;

   --  Values from "Reference Solar Spectral Irradiance: ASTM G-173", ETR column
   --  (see http://rredc.nrel.gov/solar/spectra/am1.5/ASTMG173/ASTMG173.html),
   --  summed and averaged in each bin (e.g. the value for 360nm is the average
   --  of the ASTM G-173 values for all wavelengths between 360 and 370 nm).
   --  Values in W.m^-2
   Solar_Irradiances : constant Float_64_Array
     := (1.11776, 1.14259, 1.01249, 1.14716, 1.72765, 1.73054, 1.6887, 1.61253,
         1.91198, 2.03474, 2.02042, 2.02212, 1.93377, 1.95809, 1.91686, 1.8298,
         1.8685, 1.8931, 1.85149, 1.8504, 1.8341, 1.8345, 1.8147, 1.78158, 1.7533,
         1.6965, 1.68194, 1.64654, 1.6048, 1.52143, 1.55622, 1.5113, 1.474, 1.4482,
         1.41018, 1.36775, 1.34188, 1.31429, 1.28303, 1.26758, 1.2367, 1.2082,
         1.18737, 1.14683, 1.12362, 1.1058, 1.07124, 1.04992);

   --  Values from http://www.iup.uni-bremen.de/gruppen/molspec/databases/
   --  referencespectra/o3spectra2011/index.html for 233K, summed and averaged in
   --  each bin (e.g. the value for 360nm is the average of the original values
   --  for all wavelengths between 360 and 370nm). Values in m^2
   Ozone_Cross_Section : constant Float_64_Array
     := (1.18e-27, 2.182e-28, 2.818e-28, 6.636e-28, 1.527e-27, 2.763e-27, 5.52e-27,
         8.451e-27, 1.582e-26, 2.316e-26, 3.669e-26, 4.924e-26, 7.752e-26, 9.016e-26,
         1.48e-25, 1.602e-25, 2.139e-25, 2.755e-25, 3.091e-25, 3.5e-25, 4.266e-25,
         4.672e-25, 4.398e-25, 4.701e-25, 5.019e-25, 4.305e-25, 3.74e-25, 3.215e-25,
         2.662e-25, 2.238e-25, 1.852e-25, 1.473e-25, 1.209e-25, 9.423e-26, 7.455e-26,
         6.566e-26, 5.105e-26, 4.15e-26, 4.228e-26, 3.237e-26, 2.451e-26, 2.801e-26,
         2.534e-26, 1.624e-26, 1.465e-26, 2.078e-26, 1.383e-26, 7.105e-27);

   K_Dobson_Unit : constant Float_64 := 2.687e20;
   --  From https://en.wikipedia.org/wiki/Dobson_unit, in molecules.m^-2

   K_Max_Ozone_Number_Density : constant Float_64 := 300.0 * K_Dobson_Unit / 15000.0;
   --  Maximum number density of ozone molecules, in m^-3 (computed so at to get
   --  300 Dobson units of ozone - for this we divide 300 DU by the integral of
   --  the ozone density profile defined below, which is equal to 15 km)

   K_Rayleigh                     : constant Float_64 := 1.24062e-6;
   K_Rayleigh_Scale_Height        : constant Float_64 := 8000.0;
   K_Mie_Scale_Height             : constant Float_64 := 1200.0;
   K_Mie_Angstrom_Alpha           : constant Float_64 := 0.0;
   K_Mie_Angstrom_Beta            : constant Float_64 := 5.328e-3;
   K_Mie_Single_Scattering_Albedo : constant Float_64 := 0.9;
   K_Mie_Phase_Function_G         : constant Float_64 := 0.8;
   K_Ground_Albedo                : constant Float_64 := 0.1;

   Rayleigh_Layer : constant Density_Profile_Layer
     := (0.0, 1.0, -1.0 / K_Rayleigh_Scale_Height, 0.0, 0.0);
   Mie_Layer : constant Density_Profile_Layer
     := (0.0, 1.0, -1.0 / K_Mie_Scale_Height, 0.0, 0.0);

   --  Density profile increasing linearly from 0 to 1 between 10 and 25 km, and
   --  decreasing linearly from 1 to 0 between 25 and 40 km. This is an approximate
   --  profile from http://www.kln.ac.lk/science/Chemistry/Teaching_Resources/
   --  Documents/Introduction%20to%20atmospheric%20chemistry.pdf (page 10)
   Absorption_Layer_1 : constant Density_Profile_Layer
     := (25000.0, 0.0, 0.0, 1.0 / 15000.0, -2.0 / 3.0);
   Absorption_Layer_2 : constant Density_Profile_Layer
     := (0.0, 0.0, 0.0, -1.0 / 15000.0, 8.0 / 3.0);

   function Data (Luminance : Luminance_Type) return Model_Data is
      package EF is new Ada.Numerics.Generic_Elementary_Functions (Float_64);

      function To_Radians (Value : Float_64) return Float_64 is
        (Value / 180.0 * Ada.Numerics.Pi);

      Wavelengths, Solar_Irradiance, Ground_Albedo : Double_Vectors.Vector;
      Rayleigh_Scattering, Mie_Scattering          : Double_Vectors.Vector;
      Mie_Extinction, Absorption_Extinction        : Double_Vectors.Vector;

      Absorption_Density, Rayleigh_Density, Mie_Density : Density_Vectors.Vector;

      D_Lambda : constant Float_64 := 10.0;
      L : Float_64 := K_Lambda_Min;
   begin
      while L <= K_Lambda_Max loop
         declare
            use EF;

            Lambda : constant Float_64 := L * 1.0e-3;
            Mie    : constant Float_64
              := K_Mie_Angstrom_Beta / K_Mie_Scale_Height * Lambda ** (-K_Mie_Angstrom_Alpha);
            Sample_Index : constant Size := Size (L - K_Lambda_Min) / 10;
         begin
            Wavelengths.Append (L);
            Solar_Irradiance.Append (Solar_Irradiances (Sample_Index));
            Ground_Albedo.Append (K_Ground_Albedo);

            Rayleigh_Scattering.Append (K_Rayleigh * Lambda ** (-4.0));
            Mie_Scattering.Append (Mie * K_Mie_Single_Scattering_Albedo);
            Mie_Extinction.Append (Mie);

            --  Ozone, 0.0 if no ozone
            Absorption_Extinction.Append
              (K_Max_Ozone_Number_Density * Ozone_Cross_Section (Sample_Index));
         end;

         L := L + D_Lambda;
      end loop;

      Absorption_Density.Append (Absorption_Layer_1);
      Absorption_Density.Append (Absorption_Layer_2);

      Rayleigh_Density.Append (Rayleigh_Layer);
      Mie_Density.Append (Mie_Layer);

      return
        (Luminance                   => Luminance,
         Samples                     => Wavelengths.Length,
         Wavelengths                 => Wavelengths,
         Solar_Irradiance            => Solar_Irradiance,
         Sun_Angular_Radius          => 0.00935 / 2.0,
         Bottom_Radius               => 6360000.0,
         Top_Radius                  => 6420000.0,
         Rayleigh_Density            => Rayleigh_Density,
         Rayleigh_Scattering         => Rayleigh_Scattering,
         Mie_Density                 => Mie_Density,
         Mie_Scattering              => Mie_Scattering,
         Mie_Extinction              => Mie_Extinction,
         Mie_Phase_Function_G        => K_Mie_Phase_Function_G,
         Absorption_Density          => Absorption_Density,
         Absorption_Extinction       => Absorption_Extinction,
         Ground_Albedo               => Ground_Albedo,
         Max_Sun_Zenith_Angle        => To_Radians (if Half_Precision then 102.0 else 120.0),
         Length_Unit_In_Meters       => 1000.0,
         Num_Precomputed_Wavelengths => (if Luminance = Precomputed then 15 else 3),
         Combine_Scattering_Textures => Combined_Textures,
         Half_Precision              => Half_Precision);
   end Data;

end Orka.Features.Atmosphere.Earth;
