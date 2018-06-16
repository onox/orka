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

with Ada.Directories;

package body Orka.Resources.Locations.Directories is

   overriding
   function Read_Data
     (Object : Directory_Location;
      Path   : String) return not null Byte_Array_Access
   is
      Path_Separator : constant Character
        with Import, Convention => C, External_Name => "__gnat_dir_separator";

      Directory : String renames SU.To_String (Object.Full_Path);
      Full_Path : constant String := Directory & Path_Separator & Path;

      use Ada.Directories;
   begin
      if not Exists (Full_Path) then
         raise Name_Error with "File '" & Full_Path & "' not found";
      end if;

      if Kind (Full_Path) /= Ordinary_File then
         raise Name_Error with "Path '" & Full_Path & "' is not a regular file";
      end if;

      declare
         File : constant Byte_Array_File'Class := Open_File (Full_Path);
      begin
         return File.Read_File;
      end;
   end Read_Data;

   function Create_Location (Path : String) return Location_Ptr is
      use Ada.Directories;

      Full_Path : constant String := Full_Name (Path);
   begin
      if not Exists (Full_Path) then
         raise Name_Error with "Directory '" & Full_Path & "' not found";
      end if;

      if Kind (Full_Path) /= Directory then
         raise Name_Error with "Path '" & Full_Path & "' is not a directory";
      end if;

      return new Directory_Location'(Full_Path => SU.To_Unbounded_String (Full_Path));
   end Create_Location;

end Orka.Resources.Locations.Directories;
