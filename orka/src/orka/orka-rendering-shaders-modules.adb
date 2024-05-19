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

with Orka.Logging.Default;
with Orka.Registry;
with Orka.Resources;
with Orka.Strings;
with Orka.Terminals;

package body Orka.Rendering.Shaders.Modules is

   package Shaders renames GL.Objects.Shaders;

   function Get_Type (Kind : Shader_Kind) return Shaders.Shader_Type is
     (case Kind is
        when Vertex_Shader          => Shaders.Vertex_Shader,
        when Tess_Control_Shader    => Shaders.Tess_Control_Shader,
        when Tess_Evaluation_Shader => Shaders.Tess_Evaluation_Shader,
        when Geometry_Shader        => Shaders.Geometry_Shader,
        when Fragment_Shader        => Shaders.Fragment_Shader,
        when Compute_Shader         => Shaders.Compute_Shader);

   function Get_Kind (Kind : Shaders.Shader_Type) return Shader_Kind is
     (case Kind is
        when Shaders.Vertex_Shader          => Vertex_Shader,
        when Shaders.Tess_Control_Shader    => Tess_Control_Shader,
        when Shaders.Tess_Evaluation_Shader => Tess_Evaluation_Shader,
        when Shaders.Geometry_Shader        => Geometry_Shader,
        when Shaders.Fragment_Shader        => Fragment_Shader,
        when Shaders.Compute_Shader         => Compute_Shader);

   function Kind (Object : Shader_Module) return Shader_Kind is (Get_Kind (Object.Shader.Constant_Reference.Kind));

   use all type Orka.Logging.Default_Module;
   use all type Orka.Logging.Severity;

   procedure Log is new Orka.Logging.Default.Generic_Log (Renderer);

   function Trim_Image (Value : Integer) return String is
     (Orka.Strings.Trim (Integer'Image (Value)));

   package L1 renames Ada.Characters.Latin_1;

   use Orka.Strings;

   type Row_Data is record
      Line, Column   : Positive;
      Severity, Text : SU.Unbounded_String;
   end record;

   function Get_Row_Data (Info_Log : String) return Row_Data is
      Log_Parts     : constant Orka.Strings.String_List := Split (Info_Log, ":", 3);
      Message_Parts : constant String_List := Split (Trim (+Log_Parts (3)), ": ", 2);

      Line : constant Positive :=
        Positive'Value (+Split (+Log_Parts (2), "(", 2) (1));
      Column : constant Positive :=
        Positive'Value (+Split (+Split (+Log_Parts (2), "(", 2) (2), ")", 2) (1));
   begin
      return (Line => Line, Column => Column, Severity => Message_Parts (1), Text => Message_Parts (2));
   exception
      when others =>
         --  Continue if parsing Info_Log fails
         return (Line => Positive'Last, others => <>);
   end Get_Row_Data;

   procedure Log_Error_With_Source (Text : String; Data : Row_Data; Print_Prefix, Print_Suffix : Boolean) is
      package SF renames Ada.Strings.Fixed;

      Extra_Rows          : constant := 1;
      Line_Number_Padding : constant := 2;

      Separator : constant String := " | ";

      use SF;

      use all type Orka.Terminals.Color;
      use all type Orka.Terminals.Style;
   begin
      if Data.Line = Positive'Last then
         return;
      end if;

      declare
         Message_Kind_Color : constant Orka.Terminals.Color :=
           (if +Data.Severity = "error" then
              Red
            elsif +Data.Severity = "warning" then
              Yellow
            elsif +Data.Severity = "note" then
              Cyan
            else
              Default);

         Log_Severity : constant Orka.Logging.Severity :=
           (if +Data.Severity = "error" then
              Error
            elsif +Data.Severity = "warning" then
              Warning
            elsif +Data.Severity = "note" then
              Info
            else
              Error);

         Message_Kind : constant String :=
           Orka.Terminals.Colorize (+Data.Severity & ":", Foreground => Message_Kind_Color);
         Message_Value : constant String :=
           Orka.Terminals.Colorize (+Data.Text, Attribute => Bold);

         -------------------------------------------------------------------------

         Lines : constant Orka.Strings.String_List := Split (Text, "" & L1.LF);

         First_Row : constant Positive := Positive'Max (Lines'First, Data.Line - (if Print_Prefix then Extra_Rows else 0));
         Last_Row  : constant Positive := Positive'Min (Lines'Last, Data.Line + (if Print_Suffix then Extra_Rows else 0));

         Line_Digits : constant Positive := Trim (Last_Row'Image)'Length + Line_Number_Padding;
      begin
         for Row_Index in First_Row .. Last_Row loop
            declare
               Row_Image : constant String :=
                 SF.Tail (Trim (Row_Index'Image), Line_Digits);
               Row_Image_Colorized : constant String :=
                  Orka.Terminals.Colorize (Row_Image, Attribute => Dark);

               Line_Image : constant String := +Lines (Row_Index);
               Line_Image_Colorized : constant String :=
                  Orka.Terminals.Colorize (Line_Image, Attribute => (if Row_Index = Data.Line then Default else Dark));

               First_Index_Line : constant Natural :=
                 SF.Index_Non_Blank (Line_Image, Going => Ada.Strings.Forward);

               Error_Indicator : constant String :=
                 Orka.Terminals.Colorize
                   ((Data.Column - 1 + Natural'Max (0, First_Index_Line - 2)) * " " & "^",
                    Foreground => Green,
                    Attribute  => Bold);

               Prefix_Image : constant String :=
                 (Row_Image'Length + Separator'Length) * " ";
            begin
               if (Print_Prefix and Row_Index <= Data.Line) or (Print_Suffix and Row_Index > Data.Line) then
                  Log (Log_Severity, Row_Image_Colorized  & Separator & Line_Image_Colorized);
               end if;
               if Row_Index = Data.Line then
                  if Print_Prefix then
                     Log (Log_Severity, Prefix_Image  & Error_Indicator);
                  end if;
                  Log (Log_Severity, Prefix_Image & ">>> " & Message_Kind & " " & Message_Value);
               end if;
            end;
         end loop;
      end;
   end Log_Error_With_Source;

   use all type GL.Objects.Shaders.Shader_Type;

   function Image (Kind : GL.Objects.Shaders.Shader_Type) return String is
     (case Kind is
        when Vertex_Shader          => "vertex shader",
        when Fragment_Shader        => "fragment shader",
        when Geometry_Shader        => "geometry shader",
        when Tess_Evaluation_Shader => "tessellation evaluation shader",
        when Tess_Control_Shader    => "tessellation control shader",
        when Compute_Shader         => "compute shader");

   procedure Print_Log (Text, Shader_Log : String) is
      Log_Parts : constant Orka.Strings.String_List := Split (Shader_Log, "" & L1.LF);

      Parts : array (Log_Parts'Range) of Row_Data;
   begin
      for Index in Log_Parts'Range loop
         Parts (Index) := Get_Row_Data (+Log_Parts (Index));
      end loop;

      for Index in Parts'Range loop
         Log_Error_With_Source
            (Text,
            Parts (Index),
            Index = Parts'First
            or else Parts (Index - 1).Line /= Parts (Index).Line
            or else Parts (Index - 1).Column /= Parts (Index).Column,
            Index = Parts'Last
            or else Parts (Index).Line /= Parts (Index + 1).Line
            or else Parts (Index).Column /= Parts (Index + 1).Column);
      end loop;
   end Print_Log;

   procedure Load_And_Compile
     (Object      : in out Shader_Holders.Holder;
      Shader_Kind : GL.Objects.Shaders.Shader_Type;
      Path        : String)
   is
      Path_Parts : constant String_List := Orka.Strings.Split (Path, Separator => ":", Maximum => 2);

      Namespace : constant String := +Path_Parts (1);
      File      : constant String := +Path_Parts (2);

      Shader : GL.Objects.Shaders.Shader (Kind => Shader_Kind);
      Source : constant Orka.Resources.Byte_Array_Pointers.Pointer :=
        Orka.Registry.Location (Namespace).Read_Data (File);

      Text : String renames Orka.Resources.Convert (Source.Get);
   begin
      Shader.Set_Source (Text);

      Shader.Compile;
      if not Shader.Compile_Status then
         declare
            Shader_Log : constant String := Shader.Info_Log;
         begin
            Log (Error, "Compiling shader " & Path & " failed:");
            Print_Log (Text, Shader_Log);

            raise Shader_Compile_Error with Path & ":" & Shader_Log;
         end;
      end if;
      Log (Info, "Compiled " & Image (Shader_Kind) & " " & Path & " (" & Trim_Image (Orka.Strings.Lines (Text)) & " lines)");

      Object.Replace_Element (Shader);
   end Load_And_Compile;

   procedure Set_And_Compile
     (Object      : in out Shader_Holders.Holder;
      Shader_Kind : GL.Objects.Shaders.Shader_Type;
      Source      : String) is
   begin
      if Source /= "" then
         pragma Assert (Object.Is_Empty);
         declare
            Shader : GL.Objects.Shaders.Shader (Kind => Shader_Kind);
         begin
            Shader.Set_Source (Source);

            Shader.Compile;
            if not Shader.Compile_Status then
               declare
                  Shader_Log : constant String := Shader.Info_Log;
               begin
                  Log (Error, "Compiling " & Shader_Kind'Image & " shader failed:");
                  Print_Log (Source, Shader_Log);

                  raise Shader_Compile_Error with Shader_Kind'Image & ":" & Shader_Log;
               end;
            end if;
            Log (Info, "Compiled " & Image (Shader_Kind) & " text (" &
              Trim_Image (Source'Length) & " characters)");

            Object.Replace_Element (Shader);
         end;
      end if;
   end Set_And_Compile;

   function Create_Module
     (Kind : Shader_Kind;
      Path : String) return Shader_Module is
   begin
      if Path = "" then
         raise Constraint_Error;
      end if;

      return Result : Shader_Module do
         Load_And_Compile (Result.Shader, Get_Type (Kind), Path);
      end return;
   end Create_Module;

   function Create_Modules
     (Kind  : Shader_Kind;
      Paths : String_Array) return Shader_Module_Array
   is [for Path of Paths => Create_Module (Kind, Path.all)];

   function Create_Module_From_Source
     (Kind : Shader_Kind;
      Text : String) return Shader_Module is
   begin
      return Result : Shader_Module do
         Set_And_Compile (Result.Shader, Get_Type (Kind), Text);
      end return;
   end Create_Module_From_Source;

   procedure Attach_Shaders (Modules : Shader_Module_Array; Shader : Orka.Rendering.Shaders.Shader) is
   begin
      for Module of Modules loop
         if not Module.Shader.Is_Empty then
            Shader.GL_Program.Attach (Module.Shader.Element);
         end if;
      end loop;
   end Attach_Shaders;

   procedure Detach_Shaders (Modules : Shader_Module_Array; Shader : Orka.Rendering.Shaders.Shader) is
   begin
      for Module of Modules loop
         if not Module.Shader.Is_Empty then
            Shader.GL_Program.Detach (Module.Shader.Element);
         end if;
      end loop;
   end Detach_Shaders;

end Orka.Rendering.Shaders.Modules;
