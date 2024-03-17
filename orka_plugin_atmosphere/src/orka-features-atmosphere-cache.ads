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

with Orka.Features.Atmosphere.Rendering;

package Orka.Features.Atmosphere.Cache is

   type Cached_Atmosphere is limited new Rendering.Atmosphere with private;

   function Create_Atmosphere
     (Data             : aliased Model_Data;
      Location_Shaders : Resources.Locations.Location_Ptr;
      Location_Cache   : Resources.Locations.Writable_Location_Ptr;
      Parameters       : Rendering.Model_Parameters := (others => <>)) return Cached_Atmosphere;

   overriding
   function Create_Atmosphere
     (Data       : aliased Model_Data;
      Location   : Resources.Locations.Location_Ptr;
      Parameters : Rendering.Model_Parameters := (others => <>)) return Cached_Atmosphere;

private

   type Cached_Atmosphere is limited new Rendering.Atmosphere with record
      Textures : Precomputed_Textures;
   end record;

   overriding procedure Render (Object : Cached_Atmosphere);

end Orka.Features.Atmosphere.Cache;
