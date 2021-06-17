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

with Interfaces.C.Pointers;

with Ada.Characters.Latin_1;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body AWT.OS is

   use all type AWT.Inputs.Pointer_Button;
   use all type AWT.Inputs.Keyboard_Button;

   function Code_To_Button (Code : Unsigned_32) return AWT.Inputs.Pointer_Button is
     (case Code is
        when 272 => Left,
        when 273 => Right,
        when 274 => Middle,
        when others => Unknown);

   function Code_To_Button (Code : Unsigned_32) return AWT.Inputs.Keyboard_Button is
     (case Code is
        when 1 => Key_Escape,
        when 2 => Key_1,
        when 3 => Key_2,
        when 4 => Key_3,
        when 5 => Key_4,
        when 6 => Key_5,
        when 7 => Key_6,
        when 8 => Key_7,
        when 9 => Key_8,
        when 10 => Key_9,
        when 11 => Key_0,
        when 12 => Key_Minus,
        when 13 => Key_Equal,
        when 14 => Key_Backspace,
        when 15 => Key_Tab,
        when 16 => Key_Q,
        when 17 => Key_W,
        when 18 => Key_E,
        when 19 => Key_R,
        when 20 => Key_T,
        when 21 => Key_Y,
        when 22 => Key_U,
        when 23 => Key_I,
        when 24 => Key_O,
        when 25 => Key_P,
        when 26 => Key_Left_Bracket,
        when 27 => Key_Right_Bracket,
        when 28 => Key_Enter,
        when 29 => Key_Left_Ctrl,
        when 30 => Key_A,
        when 31 => Key_S,
        when 32 => Key_D,
        when 33 => Key_F,
        when 34 => Key_G,
        when 35 => Key_H,
        when 36 => Key_J,
        when 37 => Key_K,
        when 38 => Key_L,
        when 39 => Key_Semicolon,
        when 40 => Key_Apostrophe,
        when 41 => Key_Backtick,
        when 42 => Key_Left_Shift,
        when 43 => Key_Backslash,
        when 44 => Key_Z,
        when 45 => Key_X,
        when 46 => Key_C,
        when 47 => Key_V,
        when 48 => Key_B,
        when 49 => Key_N,
        when 50 => Key_M,
        when 51 => Key_Comma,
        when 52 => Key_Period,
        when 53 => Key_Slash,
        when 54 => Key_Right_Shift,
        when 55 => Key_Numpad_Asterisk,
        when 56 => Key_Left_Alt,
        when 57 => Key_Space,
        when 58 => Key_Caps_Lock,
        when 59 => Key_F1,
        when 60 => Key_F2,
        when 61 => Key_F3,
        when 62 => Key_F4,
        when 63 => Key_F5,
        when 64 => Key_F6,
        when 65 => Key_F7,
        when 66 => Key_F8,
        when 67 => Key_F9,
        when 68 => Key_F10,
        when 69 => Key_Num_Lock,
        when 70 => Key_Scroll_Lock,
        when 71 => Key_Numpad_7,
        when 72 => Key_Numpad_8,
        when 73 => Key_Numpad_9,
        when 74 => Key_Numpad_Minus,
        when 75 => Key_Numpad_4,
        when 76 => Key_Numpad_5,
        when 77 => Key_Numpad_6,
        when 78 => Key_Numpad_Plus,
        when 79 => Key_Numpad_1,
        when 80 => Key_Numpad_2,
        when 81 => Key_Numpad_3,
        when 82 => Key_Numpad_0,
        when 83 => Key_Numpad_Period,
        --  84 .. 86
        when 87 => Key_F11,
        when 88 => Key_F12,
        --  89 .. 95
        when 96 => Key_Numpad_Enter,
        when 97 => Key_Right_Ctrl,
        when 98 => Key_Numpad_Slash,
        when 99 => Key_Print_Screen,
        when 100 => Key_Right_Alt,
        --  101
        when 102 => Key_Home,
        when 103 => Key_Arrow_Up,
        when 104 => Key_Page_Up,
        when 105 => Key_Arrow_Left,
        when 106 => Key_Arrow_Right,
        when 107 => Key_End,
        when 108 => Key_Arrow_Down,
        when 109 => Key_Page_Down,
        when 110 => Key_Insert,
        when 111 => Key_Delete,
        --  112 .. 118
        when 119 => Key_Pause,
        --  120 .. 124
        when 125 => Key_Left_Logo,
        when 126 => Key_Right_Logo,
        when others => Key_Unknown);

   procedure Create_Pipe (Result : out Pipe) is
      type File_Descriptor_Array is array (1 .. 2) of Standard.Wayland.File_Descriptor
        with Convention => C;

      function C_Pipe
        (File_Descriptors : in out File_Descriptor_Array) return Integer
      with Import, Convention => C, External_Name => "pipe";

      File_Descriptors : File_Descriptor_Array;
   begin
      if C_Pipe (File_Descriptors) = 0 then
         Result := (Read => File_Descriptors (1), Write => File_Descriptors (2));
      else
         raise Constraint_Error;
      end if;
   end Create_Pipe;

   function Read (Object : File) return Ada.Streams.Stream_Element_Array is
      Content : Ada.Streams.Stream_Element_Array (1 .. 1024);

      Result : constant C_Binding.Result := C_Binding.Read (Object.Handle, Content);

      use all type C_Binding.Result_Kind;
   begin
      case Result.Kind is
         when Success =>
            return Content (1 .. Result.Count);
         when EOF =>
            return Content (1 .. 0);
         when Failure =>
            raise Constraint_Error;
      end case;
   end Read;

   procedure Write (Object : in out File; Value : String) is
      subtype Byte_Array is Ada.Streams.Stream_Element_Array (1 .. Value'Length);

      function Convert is new Ada.Unchecked_Conversion
        (Source => String, Target => Byte_Array);

      Result : constant C_Binding.Result := C_Binding.Write (Object.Handle, Convert (Value));

      use all type C_Binding.Result_Kind;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      if Result.Kind /= Success or else Result.Count < Value'Length then
         raise Constraint_Error;
      end if;
   end Write;

   function Open (Path : String; Flags : Access_Flag := Read) return File is
      Result : constant C_Binding.File := C_Binding.Open
        (Path, (case Flags is
                  when Read       => C_Binding.Read_Only,
                  when Write      => C_Binding.Write_Only,
                  when Read_Write => C_Binding.Read_Write));
   begin
      return (File_Descriptor => Result.File_Descriptor, Handle => Result);
   end Open;

   procedure Close (Object : in out File) is
   begin
      C_Binding.Close (Object.Handle);
   end Close;

   function Is_Open (Object : File) return Boolean is (C_Binding.Is_Open (Object.Handle));

   ----------------------------------------------------------------------------

   use Interfaces.C;

   subtype Name_C_String is char_array (1 .. 256);

   type Dir_Entry is record
      Inode  : unsigned_long;
      Offset : long;
      Length : unsigned_short;
      Kind   : C_Binding.Entry_Kind;
      Name   : Name_C_String;
   end record
     with Convention => C_Pass_By_Copy;

   type Dir_Entry_Access is access Dir_Entry;

   type Dir_Entry_Array is array (Natural range <>) of aliased Dir_Entry_Access
     with Convention => C;

   package Dir_Entry_Pointers is new Interfaces.C.Pointers
     (Natural, Dir_Entry_Access, Dir_Entry_Array, null);

   function C_Scandir
     (Path    : String;
      Names   : access Dir_Entry_Pointers.Pointer;
      Filter  : access function (Node : Dir_Entry_Access) return int;
      Compare : access function (Left, Right : access Dir_Entry_Access) return int)
   return int
     with Import, Convention => C, External_Name => "scandir";

   function Convert (Kind : C_Binding.Entry_Kind) return Entry_Kind is
     (case Kind is
        when C_Binding.Unknown          => raise Program_Error,
        when C_Binding.Directory        => Directory,
        when C_Binding.Regular_File     => Regular_File,
        when C_Binding.Character_Device => Device_File,
        when C_Binding.Block_Device     => Device_File,
        when C_Binding.Link             => Symbolic_Link,
        when C_Binding.FIFO             => Named_Pipe,
        when C_Binding.Socket           => Socket);

   Directory_Filter : Filter_Type;

   function Is_Match_Filter (Node : Dir_Entry_Access) return int
     with Convention => C;

   function Is_Match_Filter (Node : Dir_Entry_Access) return int is
   begin
      return (if Directory_Filter (Convert (Node.Kind)) then 1 else 0);
   exception
      when Program_Error =>
         return 0;
   end Is_Match_Filter;

   function Scan_Directory
     (Path   : String;
      Filter : Filter_Type := (others => True)) return Directory_Entry_Array
   is
      package L1 renames Ada.Characters.Latin_1;

      procedure Free is new Ada.Unchecked_Deallocation
        (Dir_Entry, Dir_Entry_Access);

      procedure Free is new Ada.Unchecked_Deallocation
        (Dir_Entry_Access, Dir_Entry_Pointers.Pointer);

      Names : aliased Dir_Entry_Pointers.Pointer;

      Count : int;

      use type Dir_Entry_Pointers.Pointer;
   begin
      --  This global is a bit unfortunate; an 'Access of a nested subprogram
      --  with a foreign language convention requires a trampoline, which is
      --  forbidden because of the No_Implicit_Dynamic_Code restriction.
      Directory_Filter := Filter;
      Count := C_Scandir (Path & L1.NUL, Names'Access, Is_Match_Filter'Access, null);

      if Count = -1 then
         raise Constraint_Error;
      end if;

      if Names = null then
         return Result : Directory_Entry_Array (1 .. 0);
      end if;

      declare
         Entries : constant Dir_Entry_Array :=
           Dir_Entry_Pointers.Value (Names, ptrdiff_t (Count));
      begin
         return Result : Directory_Entry_Array (Entries'Range) do
            for Index in Entries'Range loop
               declare
                  Node : Dir_Entry_Access := Entries (Index);
               begin
                  Result (Index).Kind := Convert (Node.all.Kind);
                  Result (Index).Name := SU.To_Unbounded_String (To_Ada (Node.all.Name));

                  Free (Node);
               end;
            end loop;

            Free (Names);
         end return;
      end;
   end Scan_Directory;

end AWT.OS;
