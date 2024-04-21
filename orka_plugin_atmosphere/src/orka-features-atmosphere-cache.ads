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

with Orka.Resources.Locations;

package Orka.Features.Atmosphere.Cache is
   pragma Preelaborate;

   function Get_Textures
     (Context        : aliased Orka.Contexts.Context'Class;
      Data           : aliased Model_Data;
      Location_Cache : Orka.Resources.Locations.Writable_Location_Ptr) return Precomputed_Textures;

end Orka.Features.Atmosphere.Cache;
