--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2021 onox <denkpadje@gmail.com>
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

with Wayland;

with AWT.Inputs;

private with C_Binding;

private package AWT.OS is
   pragma Preelaborate;

   subtype Unsigned_32 is Standard.Wayland.Unsigned_32;

   function Code_To_Button (Code : Unsigned_32) return AWT.Inputs.Pointer_Button;

   function Code_To_Button (Code : Unsigned_32) return AWT.Inputs.Keyboard_Button;

   type Pipe is record
      Read, Write : Standard.Wayland.File_Descriptor;
   end record;

   procedure Create_Pipe (Result : out Pipe);

   type File (File_Descriptor : Standard.Wayland.File_Descriptor) is tagged limited private;

   function Read (Object : File) return Ada.Streams.Stream_Element_Array;

   procedure Write (Object : in out File; Value : String);

   type Access_Flag is (Read, Write, Read_Write);

   function Open (Path : String; Flags : Access_Flag := Read) return File;

   procedure Close (Object : in out File);

   function Is_Open (Object : File) return Boolean;

   ----------------------------------------------------------------------------

   package Paths is
      subtype Path is String;

      function "/" (Left, Right : String) return String is (Left & "/" & Right);
   end Paths;

   ----------------------------------------------------------------------------

   type Entry_Kind is
     (Directory, Regular_File, Symbolic_Link, Device_File, Named_Pipe, Socket);

   type Directory_Entry_Type is record
      Kind : Entry_Kind;
      Name : SU.Unbounded_String;
   end record;

   type Directory_Entry_Array is array (Natural range <>) of Directory_Entry_Type;

   type Filter_Type is array (Entry_Kind) of Boolean;

   function Scan_Directory
     (Path   : String;
      Filter : Filter_Type := (others => True)) return Directory_Entry_Array;
   --  Return an array of directory entries found in the given path that
   --  match the given filter
   --
   --  Task safety: this function is not reentrant.

private

   type File (File_Descriptor : Standard.Wayland.File_Descriptor) is tagged limited record
      Handle : C_Binding.File (File_Descriptor);
   end record;

end AWT.OS;
