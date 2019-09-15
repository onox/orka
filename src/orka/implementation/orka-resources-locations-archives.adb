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

with Ada.Directories;

with DCF.Unzip.Streams;

package body Orka.Resources.Locations.Archives is

   type Byte_Stream_Writer
     (Bytes : Byte_Array_Access)
   is new Ada.Streams.Root_Stream_Type with record
      Index : Ada.Streams.Stream_Element_Offset := Bytes'First;
   end record;

   overriding procedure Read
     (Stream : in out Byte_Stream_Writer;
      Item   :    out Ada.Streams.Stream_Element_Array;
      Last   :    out Ada.Streams.Stream_Element_Offset) is null;

   overriding procedure Write
     (Stream : in out Byte_Stream_Writer;
      Item   : in     Ada.Streams.Stream_Element_Array)
   is
      use type Ada.Streams.Stream_Element_Offset;
   begin
      Stream.Bytes (Stream.Index .. Stream.Index + Item'Length - 1) := Item;
      Stream.Index := Stream.Index + Item'Length;
   end Write;

   -----------------------------------------------------------------------------

   overriding
   function Exists (Object : Archive_Location; Path : String) return Boolean is
   begin
      return Object.Archive.Exists (Path);
   end Exists;

   overriding
   function Read_Data
     (Object : Archive_Location;
      Path   : String) return Byte_Array_Pointers.Pointer is
   begin
      if Path (Path'Last) = '/' then
         raise Name_Error with "Path '" & Path & "' is not a regular file";
      end if;

      if not Object.Exists (Path) then
         raise Name_Error with "File '" & Path & "' not found in archive " & Object.Archive.Name;
      end if;

      declare
         Pointer : Byte_Array_Pointers.Pointer;

         procedure Extract_File (File : DCF.Zip.Archived_File) is
            subtype File_Byte_Array
              is Byte_Array (1 .. Ada.Streams.Stream_Element_Offset (File.Uncompressed_Size));

            Raw_Contents : Byte_Array_Access := new File_Byte_Array;
         begin
            declare
               Stream_Writer : Byte_Stream_Writer (Raw_Contents);
            begin
               DCF.Unzip.Streams.Extract
                 (Destination      => Stream_Writer,
                  Archive_Info     => Object.Archive,
                  File             => File,
                  Verify_Integrity => False);  -- Integrity can be verified offline
               Pointer.Set (Raw_Contents);
            end;
         exception
            when others =>
               Free (Raw_Contents);
               raise;
         end Extract_File;

         procedure Extract_One_File is new DCF.Zip.Traverse_One_File (Extract_File);
      begin
         Extract_One_File (Object.Archive, Path);
         return Pointer;
      end;
   end Read_Data;

   function Create_Location (Path : String) return Location_Ptr is
   begin
      if not Ada.Directories.Exists (Path) then
         raise Name_Error with "Archive '" & Path & "' not found";
      end if;

      return Result : constant Location_Ptr := new Archive_Location'
        (Stream => DCF.Streams.Open (Path), Archive => <>)
      do
         declare
            Location : Archive_Location renames Archive_Location (Result.all);
         begin
            DCF.Zip.Load (Location.Archive, Location.Stream);
         end;
      end return;
   end Create_Location;

end Orka.Resources.Locations.Archives;
