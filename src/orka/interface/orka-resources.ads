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

with Ada.Finalization;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;

package Orka.Resources is
   pragma Preelaborate;

   subtype Byte_Array is Ada.Streams.Stream_Element_Array;

   type Byte_Array_Access is access Byte_Array;

   procedure Free_Byte_Array is new Ada.Unchecked_Deallocation
     (Object => Byte_Array, Name => Byte_Array_Access);

   type Byte_Array_File is limited new Ada.Finalization.Limited_Controlled with private;

   function Read_File (Object : in out Byte_Array_File) return Byte_Array_Access;

   function Open_File (File_Name : String) return Byte_Array_File'Class;

   Resource_Load_Error : exception;

private

   type Byte_Array_File is limited new Ada.Finalization.Limited_Controlled with record
      File : Ada.Streams.Stream_IO.File_Type;
      Finalized : Boolean;
   end record;

   overriding
   procedure Finalize (Object : in out Byte_Array_File);

end Orka.Resources;
