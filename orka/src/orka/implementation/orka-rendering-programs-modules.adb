--  SPDX-License-Identifier: Apache-2.0
--
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

with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;

with GL.Debug;

with Orka.Strings;
with Orka.Terminals;

package body Orka.Rendering.Programs.Modules is

   use GL.Debug;
   package Messages is new GL.Debug.Messages (Third_Party, Other);

   function Trim_Image (Value : Integer) return String is
     (Orka.Strings.Trim (Integer'Image (Value)));

   procedure Log_Error_With_Source (Text, Info_Log, Message : String) is
      package SF renames Ada.Strings.Fixed;
      package L1 renames Ada.Characters.Latin_1;

      Extra_Rows          : constant := 2;
      Line_Number_Padding : constant := 2;

      Separator : constant String := " | ";

      use Orka.Strings;
      use SF;

      use all type Orka.Terminals.Color;
      use all type Orka.Terminals.Style;
   begin
      declare
         Log_Parts : constant Orka.Strings.String_List := Split (Info_Log, ":", 3);

         Message_Parts : constant String_List := Split (Trim (+Log_Parts (3)), ": ", 2);

         Message_Kind_Color : constant Orka.Terminals.Color :=
           (if +Message_Parts (1) = "error" then
              Red
            elsif +Message_Parts (2) = "warning" then
              Yellow
            elsif +Message_Parts (2) = "note" then
              Cyan
            else
              Default);

         Message_Kind : constant String :=
           Orka.Terminals.Colorize (+Message_Parts (1) & ":", Foreground => Message_Kind_Color);
         Message_Value : constant String :=
           Orka.Terminals.Colorize (+Message_Parts (2), Attribute => Bold);

         -------------------------------------------------------------------------

         Lines : constant Orka.Strings.String_List := Orka.Strings.Split (Text, "" & L1.LF);

         Error_Row : constant Positive :=
           Positive'Value (+Orka.Strings.Split (+Log_Parts (2), "(", 2) (1));
         First_Row : constant Positive := Positive'Max (Lines'First, Error_Row - Extra_Rows);
         Last_Row  : constant Positive := Positive'Min (Lines'Last, Error_Row + Extra_Rows);

         Line_Digits : constant Positive := Trim (Last_Row'Image)'Length + Line_Number_Padding;
      begin
         Messages.Log (High, Message);

         for Row_Index in First_Row .. Last_Row loop
            declare
               Row_Image : constant String :=
                 SF.Tail (Trim (Row_Index'Image), Line_Digits);
               Row_Image_Colorized : constant String :=
                  Orka.Terminals.Colorize (Row_Image, Attribute => Dark);

               Line_Image : constant String := +Lines (Row_Index);

               First_Index_Line : constant Natural :=
                 SF.Index_Non_Blank (Line_Image, Going => Ada.Strings.Forward);
               Last_Index_Line : constant Natural :=
                 SF.Index_Non_Blank (Line_Image, Going => Ada.Strings.Backward);

               Error_Indicator : constant String :=
                 Orka.Terminals.Colorize
                   (Natural'Max (0, First_Index_Line - 1) * " " &
                    (Last_Index_Line - First_Index_Line + 1) * "^",
                    Foreground => Green,
                    Attribute  => Bold);

               Prefix_Image : constant String :=
                 (Row_Image'Length + Separator'Length) * " ";
            begin
               Messages.Log (High, Row_Image_Colorized  & Separator & Line_Image);
               if Row_Index = Error_Row then
                  Messages.Log (High, Prefix_Image  & Error_Indicator);
                  Messages.Log (High, Prefix_Image & ">>> " & Message_Kind & " " & Message_Value);
               end if;
            end;
         end loop;
      end;
   exception
      when others =>
         --  Continue if parsing Info_Log fails
         null;
   end Log_Error_With_Source;

   procedure Load_And_Compile
     (Object      : in out Module;
      Shader_Kind : GL.Objects.Shaders.Shader_Type;
      Location    : Resources.Locations.Location_Ptr;
      Path        : String) is
   begin
      if Path /= "" then
         pragma Assert (Object.Shaders (Shader_Kind).Is_Empty);
         declare
            Shader : GL.Objects.Shaders.Shader (Kind => Shader_Kind);
            Source : constant Resources.Byte_Array_Pointers.Pointer
              := Location.Read_Data (Path);

            Text : String renames Resources.Convert (Source.Get);
         begin
            Shader.Set_Source (Text);

            Shader.Compile;
            if not Shader.Compile_Status then
               declare
                  Log : constant String := Shader.Info_Log;
               begin
                  Log_Error_With_Source (Text, Log, "Compiling shader " & Path & " failed:");

                  raise Shader_Compile_Error with Path & ":" & Log;
               end;
            end if;
            Messages.Log (Notification, "Compiled shader " & Path);
            Messages.Log (Notification, "  size: " & Trim_Image (Orka.Strings.Lines (Text)) &
              " lines (" & Trim_Image (Source.Get.Value'Length) & " bytes)");
            Messages.Log (Notification, "  kind: " & Shader_Kind'Image);

            Object.Shaders (Shader_Kind).Replace_Element (Shader);
         end;
      end if;
   end Load_And_Compile;

   procedure Set_And_Compile
     (Object      : in out Module;
      Shader_Kind : GL.Objects.Shaders.Shader_Type;
      Source      : String) is
   begin
      if Source /= "" then
         pragma Assert (Object.Shaders (Shader_Kind).Is_Empty);
         declare
            Shader : GL.Objects.Shaders.Shader (Kind => Shader_Kind);
         begin
            Shader.Set_Source (Source);

            Shader.Compile;
            if not Shader.Compile_Status then
               declare
                  Log : constant String := Shader.Info_Log;
               begin
                  Log_Error_With_Source (Source, Log,
                    "Compiling " & Shader_Kind'Image & " shader failed:");

                  raise Shader_Compile_Error with Shader_Kind'Image & ":" & Log;
               end;
            end if;
            Messages.Log (Notification, "Compiled string with " &
              Trim_Image (Source'Length) & " characters");
            Messages.Log (Notification, "  size: " &
              Trim_Image (Orka.Strings.Lines (Source)) & " lines");
            Messages.Log (Notification, "  kind: " & Shader_Kind'Image);

            Object.Shaders (Shader_Kind).Replace_Element (Shader);
         end;
      end if;
   end Set_And_Compile;

   function Create_Module_From_Sources (VS, TCS, TES, GS, FS, CS : String := "")
     return Module
   is
      use GL.Objects.Shaders;
   begin
      return Result : Module do
         Set_And_Compile (Result, Vertex_Shader, VS);
         Set_And_Compile (Result, Tess_Control_Shader, TCS);
         Set_And_Compile (Result, Tess_Evaluation_Shader, TES);
         Set_And_Compile (Result, Geometry_Shader, GS);
         Set_And_Compile (Result, Fragment_Shader, FS);
         Set_And_Compile (Result, Compute_Shader, CS);
      end return;
   end Create_Module_From_Sources;

   function Create_Module
     (Location : Resources.Locations.Location_Ptr;
      VS, TCS, TES, GS, FS, CS : String := "") return Module
   is
      use GL.Objects.Shaders;
   begin
      return Result : Module do
         Load_And_Compile (Result, Vertex_Shader, Location, VS);
         Load_And_Compile (Result, Tess_Control_Shader, Location, TCS);
         Load_And_Compile (Result, Tess_Evaluation_Shader, Location, TES);
         Load_And_Compile (Result, Geometry_Shader, Location, GS);
         Load_And_Compile (Result, Fragment_Shader, Location, FS);
         Load_And_Compile (Result, Compute_Shader, Location, CS);
      end return;
   end Create_Module;

   procedure Attach_Shaders (Modules : Module_Array; Program : in out Programs.Program) is
      use GL.Objects.Shaders;

      procedure Attach (Subject : Module; Stage : GL.Objects.Shaders.Shader_Type) is
         Holder : Shader_Holder.Holder renames Subject.Shaders (Stage);
      begin
         if not Holder.Is_Empty then
            Program.GL_Program.Attach (Holder.Element);
            Program.Stages (Stage) := True;
         end if;
      end Attach;
   begin
      for Module of Modules loop
         Attach (Module, Vertex_Shader);
         Attach (Module, Tess_Control_Shader);
         Attach (Module, Tess_Evaluation_Shader);
         Attach (Module, Geometry_Shader);
         Attach (Module, Fragment_Shader);
         Attach (Module, Compute_Shader);
      end loop;
   end Attach_Shaders;

   procedure Detach_Shaders (Modules : Module_Array; Program : Programs.Program) is
      use GL.Objects.Shaders;

      procedure Detach (Holder : Shader_Holder.Holder) is
      begin
         if not Holder.Is_Empty then
            Program.GL_Program.Detach (Holder.Element);
         end if;
      end Detach;
   begin
      for Module of Modules loop
         Detach (Module.Shaders (Vertex_Shader));
         Detach (Module.Shaders (Tess_Control_Shader));
         Detach (Module.Shaders (Tess_Evaluation_Shader));
         Detach (Module.Shaders (Geometry_Shader));
         Detach (Module.Shaders (Fragment_Shader));
         Detach (Module.Shaders (Compute_Shader));
      end loop;
   end Detach_Shaders;

end Orka.Rendering.Programs.Modules;
