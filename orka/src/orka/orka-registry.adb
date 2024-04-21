--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2024 onox <denkpadje@gmail.com>
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

with Ada.Containers.Indefinite_Ordered_Maps;

package body Orka.Registry is

   use type Orka.Resources.Locations.Location_Ptr;

   package Location_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => String,
      Element_Type => Orka.Resources.Locations.Location_Ptr);

   Locations : Location_Maps.Map;

   procedure Add
     (Namespace : String;
      Location  : Orka.Resources.Locations.Location_Ptr) is
   begin
      Locations.Include (Namespace, Location);
   end Add;

   function Location (Namespace : String) return Orka.Resources.Locations.Location_Ptr is
   begin
      return Locations.Element (Namespace);
   end Location;

end Orka.Registry;
