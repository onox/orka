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

package body Orka.Resources is

   use Ada.Streams;

   function Open_File (File_Name : String) return Byte_Array_File'Class is
   begin
      return Result : Byte_Array_File'Class
        := Byte_Array_File'(Ada.Finalization.Limited_Controlled
        with File => <>, Finalized => False) do
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

   function Read_File (Object : in out Byte_Array_File) return Byte_Array_Access is
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
         Free_Byte_Array (Raw_Contents);
         raise;
   end Read_File;

end Orka.Resources;
