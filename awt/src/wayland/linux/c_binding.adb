--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 - 2019 Joakim Strandberg <joakim@mequinox.se>
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

package body C_Binding is

   subtype Size_Type is Interfaces.C.unsigned_long;
   subtype SSize_Type is Interfaces.C.long;

   use type SSize_Type;

   function C_Close
     (File_Descriptor : Interfaces.C.int) return Interfaces.C.int
   with Import, Convention => C, External_Name => "close";

   function C_Read
     (File_Descriptor : Interfaces.C.int;
      Buffer          : in out Ada.Streams.Stream_Element_Array;
      Count           : Size_Type) return SSize_Type
   with Import, Convention => C, External_Name => "read";

   function C_Write
     (File_Descriptor : Interfaces.C.int;
      Buffer          : Ada.Streams.Stream_Element_Array;
      Count           : Size_Type) return SSize_Type
   with Import, Convention => C, External_Name => "write";

   procedure Close (This : in out File) is
      Result : Interfaces.C.int;
   begin
      Result := C_Close (Interfaces.C.int (This.File_Descriptor));
      pragma Assert (Result /= -1);
      This.Open := False;
   end Close;

   procedure Write
     (This : File;
      Bytes : Ada.Streams.Stream_Element_Array)
   is
      Result : SSize_Type;
   begin
      Result :=
        C_Write
          (File_Descriptor => Interfaces.C.int (This.File_Descriptor),
           Buffer          => Bytes,
           Count           => Bytes'Length);
      pragma Assert (Result /= -1);
   end Write;

   function Read
     (This : File;
      Bytes : in out Ada.Streams.Stream_Element_Array) return Read_Result
   is
      Result : constant SSize_Type
        := C_Read (Interfaces.C.int (This.File_Descriptor), Bytes, Bytes'Length);
   begin
      case Result is
         when SSize_Type'First .. -1 =>
            return (Kind_Id => Read_Failure);
         when 0 =>
            return (Kind_Id => End_Of_File_Reached);
         when 1 .. SSize_Type'Last =>
            return (Kind_Id       => Read_Success,
                    Element_Count =>
                       Ada.Streams.Stream_Element_Count (Result));
      end case;
   end Read;

end C_Binding;
