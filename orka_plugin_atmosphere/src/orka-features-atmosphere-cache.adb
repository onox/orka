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

with Orka.Features.Atmosphere.KTX;
with Orka.Logging.Default;

package body Orka.Features.Atmosphere.Cache is

   use all type Orka.Logging.Default_Module;
   use all type Orka.Logging.Severity;

   procedure Log is new Orka.Logging.Default.Generic_Log (Other);

   function Create_Atmosphere
     (Data             : aliased Model_Data;
      Location_Shaders : Resources.Locations.Location_Ptr;
      Location_Cache   : Resources.Locations.Writable_Location_Ptr;
      Parameters       : Rendering.Model_Parameters := (others => <>)) return Cached_Atmosphere is
   begin
      if not Location_Cache.Exists ("irradiance.ktx") or
         not Location_Cache.Exists ("scattering.ktx") or
         not Location_Cache.Exists ("transmittance.ktx")
      then
         Log (Info, "Precomputing atmosphere... (this may take a while)");
         declare
            Atmosphere_Model : constant Model := Create_Model (Data, Location_Shaders);
            Textures : constant Precomputed_Textures := Atmosphere_Model.Compute_Textures;
         begin
            Log (Info, "Precomputed textures for atmosphere");
            KTX.Save_Textures (Textures, Location_Cache);
            Log (Success, "Saved textures for atmosphere");
         end;
      end if;

      return
        (Rendering.Create_Atmosphere (Data, Location_Shaders, Parameters) with
         Textures =>
           KTX.Load_Textures (Data, Orka.Resources.Locations.Location_Ptr (Location_Cache)));
   end Create_Atmosphere;

   overriding
   function Create_Atmosphere
     (Data       : aliased Model_Data;
      Location   : Resources.Locations.Location_Ptr;
      Parameters : Rendering.Model_Parameters := (others => <>)) return Cached_Atmosphere is
   begin
      --  This function just exists to satisfy the compiler
      raise Program_Error with "Use the other constructor";
      return (Rendering.Atmosphere'(Rendering.Create_Atmosphere (Data, Location, Parameters)) with Textures => <>);
   end Create_Atmosphere;

   overriding procedure Render (Object : Cached_Atmosphere) is
   begin
      Bind_Textures (Object.Textures);
      Rendering.Atmosphere (Object).Render;
   end Render;

end Orka.Features.Atmosphere.Cache;
