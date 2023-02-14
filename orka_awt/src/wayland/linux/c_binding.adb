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

with Ada.Characters.Latin_1;

package body C_Binding is

   subtype Size_Type is Interfaces.C.unsigned_long;
   subtype SSize_Type is Interfaces.C.long;

   use type SSize_Type;

   function C_Open
     (Path_Name : String;
      Flags     : Access_Flag) return Interfaces.C.int
   with Import, Convention => C, External_Name => "open";

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

   function Open (Path : String; Flags : Access_Flag) return File is
      package L1 renames Ada.Characters.Latin_1;

      Result : Interfaces.C.int;
   begin
      Result := C_Open (Path & L1.NUL, Flags);
      return (File_Descriptor => Wayland.File_Descriptor (Result), Open => Result /= -1);
   end Open;

   procedure Close (Object : in out File) is
      Result : constant Interfaces.C.int
        := C_Close (Interfaces.C.int (Object.File_Descriptor));
   begin
      if Result /= -1 then
         Object.Open := False;
      end if;
   end Close;

   function Write
     (Object : File;
      Bytes  : Ada.Streams.Stream_Element_Array) return Result
   is
      Count : constant SSize_Type
        := C_Write
             (File_Descriptor => Interfaces.C.int (Object.File_Descriptor),
              Buffer          => Bytes,
              Count           => Bytes'Length);
   begin
      case Count is
         when SSize_Type'First .. -1 =>
            return (Kind => Failure);
         when 0 =>
            return (Kind => EOF);
         when 1 .. SSize_Type'Last =>
            return (Kind  => Success,
                    Count => Ada.Streams.Stream_Element_Count (Count));
      end case;
   end Write;

   function Read
     (Object : File;
      Bytes  : in out Ada.Streams.Stream_Element_Array) return Result
   is
      Count : constant SSize_Type
        := C_Read (Interfaces.C.int (Object.File_Descriptor), Bytes, Bytes'Length);
   begin
      case Count is
         when SSize_Type'First .. -1 =>
            return (Kind => Failure);
         when 0 =>
            return (Kind => EOF);
         when 1 .. SSize_Type'Last =>
            return (Kind  => Success,
                    Count => Ada.Streams.Stream_Element_Count (Count));
      end case;
   end Read;

end C_Binding;
