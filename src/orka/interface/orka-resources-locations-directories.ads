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

private with Ada.Finalization;
private with Ada.Streams.Stream_IO;

package Orka.Resources.Locations.Directories is

   type Directory_Location is limited new Location with private;

   overriding
   function Exists (Object : Directory_Location; Path : String) return Boolean;

   overriding
   function Read_Data
     (Object : Directory_Location;
      Path   : String) return Byte_Array_Pointers.Pointer;

   type Writable_Directory_Location is limited new Writable_Location with private;

   overriding
   procedure Write_Data
     (Object : Writable_Directory_Location;
      Path   : String;
      Data   : Byte_Array);

   function Create_Location (Path : String) return Location_Ptr;
   --  Return a location that is represented by a directory

   function Create_Location (Path : String) return Writable_Location_Ptr;
   --  Return a writable location that is represented by a directory

private

   type Directory_Location is limited new Location with record
      Full_Path : SU.Unbounded_String;
   end record;

   type Writable_Directory_Location is limited
     new Directory_Location and Writable_Location with null record;

   type Byte_Array_File is limited new Ada.Finalization.Limited_Controlled with record
      File : Ada.Streams.Stream_IO.File_Type;
      Finalized : Boolean;
   end record;

   overriding
   procedure Finalize (Object : in out Byte_Array_File);

end Orka.Resources.Locations.Directories;
