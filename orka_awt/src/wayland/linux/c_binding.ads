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

private with Interfaces.C;

with Ada.Streams;

with Wayland;

package C_Binding is
   pragma Preelaborate;

   type File (File_Descriptor : Wayland.File_Descriptor) is private;

   type Result_Kind is (Success, EOF, Failure);

   type Result (Kind : Result_Kind) is record
      case Kind is
         when Success =>
            Count : Ada.Streams.Stream_Element_Count;
         when EOF | Failure =>
            null;
      end case;
   end record;

   type Access_Flag is (Read_Only, Write_Only, Read_Write);

   function Open (Path : String; Flags : Access_Flag) return File
     with Pre => Path'Length > 0;

   function Read
     (Object : File;
      Bytes  : in out Ada.Streams.Stream_Element_Array) return Result
   with Pre => Is_Open (Object);

   function Write
     (Object : File;
      Bytes  : Ada.Streams.Stream_Element_Array) return Result
   with Pre => Is_Open (Object);

   procedure Close (Object : in out File)
     with Pre  =>     Is_Open (Object),
          Post => not Is_Open (Object);

   function Is_Open (Object : File) return Boolean;

   ----------------------------------------------------------------------------

   type Entry_Kind is
     (Unknown, FIFO, Character_Device, Directory, Block_Device, Regular_File, Link, Socket);

private

   use type Interfaces.C.int;

   type File (File_Descriptor : Wayland.File_Descriptor) is record
      Open : Boolean := True;
   end record;

   function Is_Open (Object : File) return Boolean is (Object.Open);

   ----------------------------------------------------------------------------

   for Entry_Kind use
     (Unknown          => 0,
      FIFO             => 1,
      Character_Device => 2,
      Directory        => 4,
      Block_Device     => 6,
      Regular_File     => 8,
      Link             => 10,
      Socket           => 12);
   for Entry_Kind'Size use Interfaces.C.unsigned_char'Size;

   for Access_Flag use
     (Read_Only  => 0,
      Write_Only => 1,
      Read_Write => 2);
   for Access_Flag'Size use Interfaces.C.int'Size;

end C_Binding;
