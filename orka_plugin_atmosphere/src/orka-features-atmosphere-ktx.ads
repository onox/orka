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

package Orka.Features.Atmosphere.KTX is
   pragma Preelaborate;

   function Load_Textures
     (Data     : Model_Data;
      Location : Resources.Locations.Location_Ptr) return Precomputed_Textures;

   procedure Save_Textures
     (Object   : Precomputed_Textures;
      Location : Resources.Locations.Writable_Location_Ptr);

end Orka.Features.Atmosphere.KTX;
