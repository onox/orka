--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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
with Ada.Strings.Fixed;

with Orka.OS;
with Orka.Strings;

package body Orka.Terminals is

   Style_Codes : constant array (Style) of Natural :=
     (Default   => 0,
      Bold      => 1,
      Dark      => 2,
      Italic    => 3,
      Underline => 4,
      Blink     => 5,
      Reversed  => 7,
      Cross_Out => 9);

   type Color_Kind is (FG, BG, None);

   type Color_Code      is range 0 .. 255;
   type Color_Component is range 0 .. 5;

   type Color_Index is (R, G, B);

   type Color_RGB is array (Color_Index) of Color_Component;

   Colors : constant array (Color) of Color_RGB :=
     (Default => (0, 0, 0),
      Grey    => (2, 2, 2),
      Red     => (4, 0, 0),
      Green   => (1, 3, 0),
      Yellow  => (4, 3, 0),
      Blue    => (0, 1, 4),
      Magenta => (4, 0, 3),
      Cyan    => (0, 4, 3),
      White   => (4, 4, 4));

   package L1 renames Ada.Characters.Latin_1;
   package SF renames Ada.Strings.Fixed;

   Reset : constant String := L1.ESC & "[0m";

   function Sequence (Code : Natural) return String is
      Image : constant String := SF.Trim (Code'Image, Ada.Strings.Both);
   begin
      return (if Code /= 0 then L1.ESC & "[" & Image & "m" else "");
   end Sequence;

   function Sequence (Kind : Color_Kind; Color : Color_RGB) return String is
      Code : constant Color_Code :=
        Color_Code (16 + 36 * Color_Code (Color (R)) +
                          6 * Color_Code (Color (G)) +
                              Color_Code (Color (B)));

      Image : constant String := SF.Trim (Code'Image, Ada.Strings.Both);
   begin
      return
        (case Kind is
           when FG   => L1.ESC & "[38;5;" & Image & "m",
           when BG   => L1.ESC & "[48;5;" & Image & "m",
           when None => "");
   end Sequence;

   function Colorize
     (Text                   : String;
      Foreground, Background : Color := Default;
      Attribute              : Style := Default) return String
   is
      FG_Kind : constant Color_Kind := (if Foreground /= Default then FG else None);
      BG_Kind : constant Color_Kind := (if Background /= Default then BG else None);

      FG : constant String := Sequence (FG_Kind, Colors (Foreground));
      BG : constant String := Sequence (BG_Kind, Colors (Background));
      ST : constant String := Sequence (Style_Codes (Attribute));
   begin
      return Reset & FG & BG & ST & Text & Reset;
   end Colorize;

   -----------------------------------------------------------------------------

   Time_Zero : Duration := 0.0;

   procedure Set_Time_Zero (Time : Duration) is
   begin
      Time_Zero := Time;
   end Set_Time_Zero;

   Day_In_Seconds : constant := 24.0 * 60.0 * 60.0;

   function Time_Image return String is
      Seconds_Since_Zero : Duration := Orka.OS.Monotonic_Clock - Time_Zero;
      Days_Since_Zero    : Natural := 0;
   begin
      while Seconds_Since_Zero > Day_In_Seconds loop
         Seconds_Since_Zero := Seconds_Since_Zero - Day_In_Seconds;
         Days_Since_Zero    := Days_Since_Zero + 1;
      end loop;

      declare
         Seconds_Rounded : constant Natural :=
           Natural (Duration'Max (0.0, Seconds_Since_Zero - 0.5));

         Hour       : constant Natural := Seconds_Rounded / 3600;
         Minute     : constant Natural := (Seconds_Rounded mod 3600) / 60;
         Second     : constant Natural := Seconds_Rounded mod 60;

         Sub_Second : constant Duration :=
           Seconds_Since_Zero - Duration (Hour * 3600 + Minute * 60 + Second);

         --  Remove first character (space) from ' hhmmss' image and then pad it to six digits
         Image1 : constant String := Natural'Image
           ((Days_Since_Zero * 24 + Hour) * 1e4 + Minute * 1e2 + Second);
         Image2 : constant String := SF.Tail (Image1 (2 .. Image1'Last), 6, '0');

         --  Insert ':' characters to get 'hh:mm:ss'
         Image3 : constant String := SF.Insert (Image2, 5, ":");
         Image4 : constant String := SF.Insert (Image3, 3, ":");

         --  Take image without first character (space) and then pad it to six digits
         Image5 : constant String := Natural'Image (Natural (Sub_Second * 1e6));
         Image6 : constant String := SF.Tail (Image5 (2 .. Image5'Last), 6, '0');
      begin
         return Image4 & "." & Image6;
      end;
   end Time_Image;

   function Format (Value : Duration; Fore, Aft : Positive) return String is
      package SF renames Ada.Strings.Fixed;

      Aft_Shift : constant Positive := 10 ** Aft;
      New_Value : constant Duration := Duration (Integer (Value * Aft_Shift)) / Aft_Shift;

      S1 : constant String := SF.Trim (New_Value'Image, Ada.Strings.Both);

      Index_Decimal : constant Natural := SF.Index (S1, ".");

      --  Following code assumes that Aft >= 1 (If Aft = 0 then Aft must
      --  be decremented to remove the decimal point)
      S2 : constant String := S1 (S1'First .. Natural'Min (S1'Last, Index_Decimal + Aft));
      S3 : constant String := SF.Tail (S2, Natural'Max (S2'Length, Fore + 1 + Aft), ' ');
   begin
      return S3;
   end Format;

   type String_Access is not null access String;

   Suffices : constant array (1 .. 3) of String_Access
     := (new String'("s"),
         new String'("ms"),
         new String'("us"));

   function Image (Value : Duration) return String is
      Number : Duration := Value;

      Last_Suffix : constant String_Access := Suffices (Suffices'Last);
      Suffix : String_Access := Suffices (Suffices'First);
   begin
      for S of Suffices loop
         Suffix := S;
         exit when Number >= 1.0 or else Number <= -1.0 or else S = Last_Suffix;
         Number := Number * 1e3;
      end loop;

      begin
         return Format (Number, Fore => 5, Aft => 3) & " " & Suffix.all;
      exception
         when others =>
            return Number'Image & " " & Suffix.all;
      end;
   end Image;

   function Trim (Value : String) return String renames Orka.Strings.Trim;

   function Strip_Line_Term (Value : String) return String renames Orka.Strings.Strip_Line_Term;

end Orka.Terminals;
