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

package Orka.Resources.Locations.Directories is

   type Directory_Location is limited new Location with private;

   overriding
   function Read_Data
     (Object : Directory_Location;
      Path   : String) return not null Byte_Array_Access;

   function Create_Location (Path : String) return Location_Ptr;

private

   type Directory_Location is limited new Location with record
      Full_Path : SU.Unbounded_String;
   end record;

end Orka.Resources.Locations.Directories;
