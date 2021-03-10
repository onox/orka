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

   type File (File_Descriptor : Wayland.File_Descriptor) is limited private;

   type Read_Result_Kind_Id is
     (Read_Success,
      End_Of_File_Reached,
      Read_Failure);

   type Read_Result (Kind_Id : Read_Result_Kind_Id) is record
      case Kind_Id is
         when Read_Success =>
            Element_Count : Ada.Streams.Stream_Element_Count;
         when End_Of_File_Reached =>
            null;
         when Read_Failure =>
            null;
      end case;
   end record;

   function Read
     (This  : File;
      Bytes : in out Ada.Streams.Stream_Element_Array) return Read_Result
   with Pre => Is_Open (This);

   procedure Write
     (This  : File;
      Bytes : Ada.Streams.Stream_Element_Array)
   with Pre => Is_Open (This);

   procedure Close (This : in out File)
     with Pre  =>     Is_Open (This),
          Post => not Is_Open (This);

   function Is_Open (This : File) return Boolean;

private

   use type Interfaces.C.int;

   type File (File_Descriptor : Wayland.File_Descriptor) is limited record
      Open : Boolean := True;
   end record;

   function Is_Open (This : File) return Boolean is (This.Open);

end C_Binding;
