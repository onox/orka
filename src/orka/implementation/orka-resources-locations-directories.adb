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

with Ada.Directories;

package body Orka.Resources.Locations.Directories is

   use Ada.Streams;

   function Open_File (File_Name : String) return Byte_Array_File is
   begin
      return Result : Byte_Array_File
        := (Ada.Finalization.Limited_Controlled with File => <>, Finalized => False) do
         Stream_IO.Open (Result.File, Stream_IO.In_File, File_Name);
      end return;
   end Open_File;

   overriding
   procedure Finalize (Object : in out Byte_Array_File) is
   begin
      if not Object.Finalized then
         if Stream_IO.Is_Open (Object.File) then
            Stream_IO.Close (Object.File);
         end if;
         Object.Finalized := True;
      end if;
   end Finalize;

   function Read_File (Object : Byte_Array_File) return not null Byte_Array_Access is
      File_Stream : Stream_IO.Stream_Access;
      File_Size   : constant Integer := Integer (Stream_IO.Size (Object.File));

      subtype File_Byte_Array is Byte_Array (1 .. Stream_Element_Offset (File_Size));
      Raw_Contents : Byte_Array_Access := new File_Byte_Array;
   begin
      File_Stream := Stream_IO.Stream (Object.File);
      File_Byte_Array'Read (File_Stream, Raw_Contents.all);
      return Raw_Contents;
   exception
      when others =>
         Free (Raw_Contents);
         raise;
   end Read_File;

   -----------------------------------------------------------------------------

   function Create_File (File_Name : String) return Byte_Array_File is
   begin
      return Result : Byte_Array_File
        := (Ada.Finalization.Limited_Controlled with File => <>, Finalized => False) do
         Stream_IO.Create (Result.File, Stream_IO.Out_File, File_Name);
      end return;
   end Create_File;

   procedure Write_Data (Object : Byte_Array_File; Data : Byte_Array) is
      File_Stream : Stream_IO.Stream_Access;
      File_Size   : constant Integer := Data'Length;

      subtype File_Byte_Array is Byte_Array (1 .. Stream_Element_Offset (File_Size));
   begin
      File_Stream := Stream_IO.Stream (Object.File);
      File_Byte_Array'Write (File_Stream, Data);
   end Write_Data;

   -----------------------------------------------------------------------------

   overriding
   function Exists (Object : Directory_Location; Path : String) return Boolean is
      Directory : String renames SU.To_String (Object.Full_Path);
      Full_Path : constant String := Directory & Path_Separator & Path;
   begin
      return Ada.Directories.Exists (Full_Path);
   end Exists;

   overriding
   function Read_Data
     (Object : Directory_Location;
      Path   : String) return Byte_Array_Pointers.Pointer
   is
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
         File : constant Byte_Array_File := Open_File (Full_Path);
         Pointer : Byte_Array_Pointers.Pointer;
      begin
         Pointer.Set (File.Read_File);
         return Pointer;
      end;
   end Read_Data;

   overriding
   procedure Write_Data
     (Object : Writable_Directory_Location;
      Path   : String;
      Data   : Byte_Array)
   is
      Directory : String renames SU.To_String (Object.Full_Path);
      Full_Path : constant String := Directory & Path_Separator & Path;

      use Ada.Directories;
   begin
      if Exists (Full_Path) then
         raise Name_Error with "File '" & Full_Path & "' already exists";
      end if;

      declare
         File : constant Byte_Array_File := Create_File (Full_Path);
      begin
         File.Write_Data (Data);
      end;
   end Write_Data;

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

   function Create_Location (Path : String) return Writable_Location_Ptr is
      use Ada.Directories;

      Full_Path : constant String := Full_Name (Path);
   begin
      if not Exists (Full_Path) then
         raise Name_Error with "Directory '" & Full_Path & "' not found";
      end if;

      if Kind (Full_Path) /= Directory then
         raise Name_Error with "Path '" & Full_Path & "' is not a directory";
      end if;

      --  The pointer is stored in a variable to make the compiler happy
      return Result : constant Writable_Location_Ptr
        := new Writable_Directory_Location'(Full_Path => SU.To_Unbounded_String (Full_Path)) do
         null;
      end return;
   end Create_Location;

end Orka.Resources.Locations.Directories;
