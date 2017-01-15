--  Copyright (c) 2016 onox <denkpadje@gmail.com>
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

with Ada.Streams.Stream_IO;

with Orka.Base64;

package body Orka.glTF.Buffers is

   function Load_Data_From_File (File_Name : String) return Stream_Element_Array_Access is
      File        : Stream_IO.File_Type;
      File_Stream : Stream_IO.Stream_Access;
   begin
      Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, File_Name);
      --  TODO Handle relative and absolute paths

      declare
         File_Size : constant Integer := Integer (Stream_IO.Size (File));
         subtype File_Stream_Array is Stream_Element_Array (1 .. Stream_Element_Offset (File_Size));
         Raw_Contents : Stream_Element_Array_Access := new File_Stream_Array;
      begin
         File_Stream := Stream_IO.Stream (File);
         File_Stream_Array'Read (File_Stream, Raw_Contents.all);

         Stream_IO.Close (File);

         return Raw_Contents;
      exception
         when others =>
            Free_Stream_Array (Raw_Contents);
            raise;
      end;
   exception
      when others =>
         if Stream_IO.Is_Open (File) then
            Stream_IO.Close (File);
         end if;
         raise;
   end Load_Data_From_File;

   function Load_Data (URI : String) return Stream_Element_Array_Access is
   begin
      if Base64.Base64_Encoded (URI) then
         return new Stream_Element_Array'(Base64.Decode (URI (Base64.Data_Prefix'Last + 1 .. URI'Last)));
      else
         return Load_Data_From_File (URI);
      end if;
   end Load_Data;

   function Create_Buffer (URI : String; Length : Natural) return Buffer is
   begin
      return Result : Buffer do
         Result.Data := Load_Data (URI);
         Result.Kind := Array_Buffer;
      end return;
   end Create_Buffer;

   function Create_Buffer_View
     (Buffer         : not null Stream_Element_Array_Access;
      Offset, Length : Natural;
      Kind           : Buffer_Kind) return Buffer_View is
   begin
      return Result : Buffer_View do
         Result.Buffer := Buffer;
         Result.Offset := Offset;
         Result.Length := Length;
         Result.Target := Kind;
      end return;
   end Create_Buffer_View;

   function Elements (View : Buffer_View) return Stream_Element_Array_Access is
      Offset : constant Stream_Element_Offset := Stream_Element_Offset (View.Offset);
      Length : constant Stream_Element_Offset := Stream_Element_Offset (View.Length);
   begin
      return new Stream_Element_Array'(View.Buffer (Offset + 1 .. Offset + Length));
   end Elements;

end Orka.glTF.Buffers;
