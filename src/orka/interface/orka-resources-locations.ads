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

with Ada.IO_Exceptions;

package Orka.Resources.Locations is
   pragma Preelaborate;

   type Location is limited interface;

   function Exists (Object : Location; Path : String) return Boolean is abstract;

   function Read_Data
     (Object : Location;
      Path   : String) return not null Byte_Array_Access is abstract;
   --  Return the data of the file at the given path in the location if
   --  Path identifies an existing file, otherwise the exception Name_Error
   --  is raised
   --
   --  If the file at the given path could not be read for any reason
   --  then any kind of error is raised.

   type Location_Access is access Location'Class;

   subtype Location_Ptr is not null Location_Access;

   Name_Error : exception renames Ada.IO_Exceptions.Name_Error;

   Path_Separator : constant Character
     with Import, Convention => C, External_Name => "__gnat_dir_separator";

end Orka.Resources.Locations;
