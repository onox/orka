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

with Ada.Streams;

with Orka.Base64;

package body Orka.glTF.Buffers is

   function Load_Data_From_File (File_Name : String) return Byte_Array_Access is
      File : Byte_Array_File'Class := Open_File (File_Name);
      --  TODO Handle relative and absolute paths
   begin
      return File.Read_File;
   end Load_Data_From_File;

   function Load_Data (URI : String) return Byte_Array_Access is
   begin
      if Base64.Base64_Encoded (URI) then
         return new Byte_Array'(Base64.Decode (URI (Base64.Data_Prefix'Last + 1 .. URI'Last)));
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
     (Buffer         : not null Byte_Array_Access;
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

   function Elements (View : Buffer_View) return Byte_Array_Access is
      use Ada.Streams;

      Offset : constant Stream_Element_Offset := Stream_Element_Offset (View.Offset);
      Length : constant Stream_Element_Offset := Stream_Element_Offset (View.Length);
   begin
      return new Byte_Array'(View.Buffer (Offset + 1 .. Offset + Length));
   end Elements;

end Orka.glTF.Buffers;
