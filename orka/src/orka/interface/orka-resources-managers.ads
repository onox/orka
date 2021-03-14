--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 onox <denkpadje@gmail.com>
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package Orka.Resources.Managers is
   pragma Preelaborate;

   type Manager is tagged limited private;

   function Contains (Object : Manager; Name : String) return Boolean;

   function Resource (Object : Manager; Name : String) return Resource_Ptr;

   procedure Add_Resource
     (Object   : in out Manager;
      Name     : String;
      Resource : Resource_Ptr);

   procedure Remove_Resource (Object : in out Manager; Name : String);

   type Manager_Ptr is not null access Manager;

   function Create_Manager return Manager_Ptr;

private

   package Resource_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Resource_Ptr,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   type Manager is tagged limited record
      Resources : Resource_Maps.Map;
   end record;

end Orka.Resources.Managers;
