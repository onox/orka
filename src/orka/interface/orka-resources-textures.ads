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

with Ada.Containers.Indefinite_Holders;

with GL.Objects.Textures;

package Orka.Resources.Textures is
   pragma Preelaborate;

   type Texture is limited new Resource with private;

   function Element (Object : Texture) return GL.Objects.Textures.Texture;

   type Texture_Ptr is not null access all Texture;

   Texture_Load_Error : exception renames Resource_Load_Error;

private

   package Texture_Holder is new Ada.Containers.Indefinite_Holders
     (Element_Type => GL.Objects.Textures.Texture,
      "=" => GL.Objects.Textures."=");

   type Texture is limited new Resource with record
      Texture : Texture_Holder.Holder;
   end record;

end Orka.Resources.Textures;
