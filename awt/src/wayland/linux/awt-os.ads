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

   function Read (Object : in out File) return Ada.Streams.Stream_Element_Array;

   procedure Write (Object : in out File; Value : String);

   procedure Close (Object : in out File);

private

   type File (File_Descriptor : Standard.Wayland.File_Descriptor) is tagged limited record
      Handle : C_Binding.File (File_Descriptor);
   end record;

end AWT.OS;
