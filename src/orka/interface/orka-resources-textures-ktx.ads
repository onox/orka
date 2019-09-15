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

with GL.Objects.Textures;

with Orka.Jobs;

with Orka.Resources.Loaders;
with Orka.Resources.Locations;
with Orka.Resources.Managers;

package Orka.Resources.Textures.KTX is

   function Create_Loader
     (Manager : Managers.Manager_Ptr) return Loaders.Loader_Ptr;

   procedure Write_Texture
     (Texture  : GL.Objects.Textures.Texture;
      Location : Locations.Writable_Location_Ptr;
      Path     : String);

end Orka.Resources.Textures.KTX;
