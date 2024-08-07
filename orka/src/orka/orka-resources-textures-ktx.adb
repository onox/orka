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

with System;

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with GL.Barriers;
with GL.Pixels.Extensions;
with GL.Types.Pointers;
with GL.Objects.Textures;

with Orka.Jobs;
with Orka.KTX;
with Orka.Logging.Default;
with Orka.OS;
with Orka.Strings;

package body Orka.Resources.Textures.KTX is

   package PE renames GL.Pixels.Extensions;

   use all type Orka.Logging.Default_Module;
   use all type Orka.Logging.Severity;

   procedure Log is new Orka.Logging.Default.Generic_Log (Resource_Loader);

   function Trim_Image (Value : Integer) return String is
     (Orka.Strings.Trim (Integer'Image (Value)));

   type KTX_Load_Job is new Jobs.Abstract_Job and Jobs.GPU_Job with record
      Data    : Loaders.Resource_Data;
      Manager : Managers.Manager_Ptr;
   end record;

   overriding
   procedure Execute
     (Object  : KTX_Load_Job;
      Context : Jobs.Execution_Context'Class);

   -----------------------------------------------------------------------------

   function Read_Texture
     (Bytes : Byte_Array_Pointers.Constant_Reference;
      Path  : String;
      Start : Time) return Rendering.Textures.Texture
   is
      T1 : Time renames Start;
      T2 : constant Time := Orka.OS.Monotonic_Clock;

      use Ada.Streams;
      use all type Rendering.Textures.LE.Texture_Kind;

      T3, T4, T5, T6 : Time;
   begin
      if not Orka.KTX.Valid_Identifier (Bytes) then
         raise Texture_Load_Error with Path & " is not a KTX file";
      end if;

      declare
         Header : constant Orka.KTX.Header := Orka.KTX.Get_Header (Bytes);
         Levels_In_File : constant Size := Size'Max (1, Header.Mipmap_Levels);
         Levels : Size := Header.Mipmap_Levels;

         Width  : constant Size := Header.Width;
         Height :          Size := Header.Height;
         Depth  :          Size := Header.Depth;
      begin
         T3 := Orka.OS.Monotonic_Clock;

         --  Allocate storage
         case Header.Kind is
            when Texture_1D =>
               if Header.Compressed then
                  raise Texture_Load_Error with Path & " has unknown 1D compressed format";
               end if;
               Height := 1;
               Depth  := 1;
            when Texture_1D_Array =>
               Height := Header.Array_Elements;
               Depth := 1;
            when Texture_2D =>
               Depth := 1;
            when Texture_2D_Array =>
               Depth := Header.Array_Elements;
            when Texture_3D =>
               null;
            when Texture_Cube_Map | Texture_Cube_Map_Array =>
               --  Texture_Cube_Map uses 2D storage, but 3D load operation
               --  according to table 8.15 of the OpenGL specification

               --  For a cube map, depth is the number of faces, for
               --  a cube map array, depth is the number of layer-faces
               Depth := Size'Max (1, Header.Array_Elements) * 6;
            when others =>
               raise Program_Error;
         end case;

         --  Allocate enough space to generate full mipmap pyramid below
         if Levels = 0 and not Header.Compressed then
            Levels := Size (Rendering.Textures.Levels ([Width, Height, Depth]));
         end if;

         declare
            Description : constant Rendering.Textures.Texture_Description :=
              (if Header.Compressed then
                 (Kind    => Header.Kind,
                  Compressed => True,
                  Compressed_Format  => Header.Compressed_Format,
                  Size    => [Width, Height, Depth],
                  Levels  => Integer (Levels),
                  Samples => 1)
               else
                 (Kind    => Header.Kind,
                  Compressed => False,
                  Format  => Header.Internal_Format,
                  Size    => [Width, Height, Depth],
                  Levels  => Integer (Levels),
                  Samples => 1));

            Texture : constant Rendering.Textures.Texture := Rendering.Textures.Create_Texture (Description);
         begin
            T4 := Orka.OS.Monotonic_Clock;

            --  TODO Handle KTXorientation key value pair
            declare
               procedure Iterate (Position : Orka.KTX.String_Maps.Cursor) is
               begin
                  Log (Warning, "Metadata: " & Orka.KTX.String_Maps.Key (Position) &
                    " = " & Orka.KTX.String_Maps.Element (Position));
               end Iterate;
            begin
               Orka.KTX.Get_Key_Value_Map (Bytes, Header.Bytes_Key_Value).Iterate (Iterate'Access);
            end;

            --  Upload texture data
            declare
               Image_Size_Index : Stream_Element_Offset
                 := Orka.KTX.Get_Data_Offset (Bytes, Header.Bytes_Key_Value);

               Cube_Map : constant Boolean := Header.Kind = Texture_Cube_Map;
            begin
               for Level in 0 .. Levels_In_File - 1 loop
                  declare
                     Face_Size : constant Natural := Orka.KTX.Get_Length (Bytes, Image_Size_Index);
                     --  If not Cube_Map, then Face_Size is the size of the whole level

                     Cube_Padding : constant Natural := 3 - ((Face_Size + 3) mod 4);
                     Image_Size   : constant Natural
                       := (if Cube_Map then (Face_Size + Cube_Padding) * 6 else Face_Size);
                     --  If Cube_Map then Levels = 1 so no need to add it to the expression

                     --  Compute size of the whole mipmap level
                     Mip_Padding : constant Natural := 3 - ((Image_Size + 3) mod 4);
                     Mipmap_Size : constant Natural := 4 + Image_Size + Mip_Padding;

                     Offset : constant Stream_Element_Offset := Image_Size_Index + 4;
                     pragma Assert
                       (Offset + Stream_Element_Offset (Image_Size) - 1 <= Bytes.Value'Last);
                     Image_Data : constant System.Address := Bytes (Offset)'Address;

                     Level_Size : constant Size_3D := Texture.Size (Level);

                     Level_Width  : constant Size := Level_Size (X);
                     Level_Height : constant Size := Level_Size (Y);
                     Level_Depth  : constant Size := Level_Size (Z);
                  begin
                     if Header.Compressed then
                        Texture.GL_Texture.Load_From_Data
                          (Level, 0, 0, 0, Level_Width, Level_Height, Level_Depth,
                           Header.Compressed_Format, Integer_32 (Image_Size), Image_Data);
                     else
                        declare
                           Original_Alignment : constant GL.Pixels.Alignment :=
                             GL.Pixels.Unpack_Alignment;
                           Is_Packed  : constant Boolean := Header.Data_Type in PE.Packed_Data_Type;
                        begin
                           GL.Pixels.Set_Unpack_Alignment (if Is_Packed then GL.Pixels.Bytes else GL.Pixels.Words);
                           Texture.GL_Texture.Load_From_Data (Level, 0, 0, 0,
                             Level_Width, Level_Height, Level_Depth,
                             Header.Format, Header.Data_Type, Image_Data);
                           GL.Pixels.Set_Unpack_Alignment (Original_Alignment);
                        end;
                     end if;

                     Image_Size_Index := Image_Size_Index + Stream_Element_Offset (Mipmap_Size);
                  end;
               end loop;
            end;
            T5 := Orka.OS.Monotonic_Clock;

            --  Generate a full mipmap pyramid if Mipmap_Levels = 0
            if Header.Mipmap_Levels = 0 and not Header.Compressed then
               Texture.GL_Texture.Generate_Mipmap;
            end if;
            T6 := Orka.OS.Monotonic_Clock;

            Log (Info, "Loaded texture " & Path & " in " &
              Logging.Trim (Logging.Image (T6 - T1)));
            Log (Info, "  dims:   " &
               Logging.Trim (Width'Image) & Strings.Unicode (" × ") &
               Logging.Trim (Height'Image) & Strings.Unicode (" × ") &
               Logging.Trim (Depth'Image) &
               ", mipmap levels:" & Levels'Image);
            Log (Info, "  size:   " & Trim_Image (Bytes.Value'Length) & " bytes");
            Log (Info, "  kind:   " & Header.Kind'Image);
            if Header.Compressed then
               Log (Info, "  format: " & Header.Compressed_Format'Image);
            else
               declare
                  Is_Packed  : constant Boolean := Header.Data_Type in PE.Packed_Data_Type;
                  Components : constant Positive := Integer (PE.Components (Header.Format));
               begin
                  Log (Info, "  format: " & Header.Internal_Format'Image &
                    (if Is_Packed then
                       " (packed with " & Trim_Image (Components) & " components)"
                     else
                       " (" & Trim_Image (Components) &
                       "x " & Header.Data_Type'Image & ")"));
               end;
            end if;

            Log (Debug, "  timing:");
            Log (Debug, "    reading file:   " & Logging.Image (T2 - T1));
            Log (Debug, "    parsing header: " & Logging.Image (T3 - T2));
            Log (Debug, "    storage:        " & Logging.Image (T4 - T3));
            Log (Debug, "    buffers:        " & Logging.Image (T5 - T4));
            if Header.Mipmap_Levels = 0 then
               Log (Debug, "    generating mipmap:" & Logging.Image (T6 - T5));
            end if;

            return Texture;
         end;
      end;
   exception
      when Error : Orka.KTX.Invalid_Enum_Error =>
         declare
            Message : constant String := Ada.Exceptions.Exception_Message (Error);
         begin
            raise Texture_Load_Error with Path & " has " & Message;
         end;
   end Read_Texture;

   function Read_Texture
     (Location : Locations.Location_Ptr;
      Path     : String) return Rendering.Textures.Texture
   is
      Start_Time : constant Time := Orka.OS.Monotonic_Clock;
   begin
      return Read_Texture (Location.Read_Data (Path).Get, Path, Start_Time);
   end Read_Texture;

   overriding
   procedure Execute
     (Object  : KTX_Load_Job;
      Context : Jobs.Execution_Context'Class)
   is
      Bytes : constant Byte_Array_Pointers.Constant_Reference := Object.Data.Bytes.Get;
      Path  : String renames SU.To_String (Object.Data.Path);

      Resource : constant Texture_Ptr := new Texture'(others => <>);
   begin
      Resource.Texture.Replace_Element (Read_Texture (Bytes, Path, Object.Data.Start_Time));

      --  Register resource at the resource manager
      Object.Manager.Add_Resource (Path, Resource_Ptr (Resource));
   end Execute;

   -----------------------------------------------------------------------------
   --                                 Loader                                  --
   -----------------------------------------------------------------------------

   type KTX_Loader is limited new Loaders.Loader with record
      Manager : Managers.Manager_Ptr;
   end record;

   overriding
   function Extension (Object : KTX_Loader) return Loaders.Extension_String is ("ktx");

   overriding
   procedure Load
     (Object   : KTX_Loader;
      Data     : Loaders.Resource_Data;
      Enqueue  : not null access procedure (Element : Jobs.Job_Ptr);
      Location : Locations.Location_Ptr)
   is
      Job : constant Jobs.Job_Ptr := new KTX_Load_Job'
        (Jobs.Abstract_Job with
          Data    => Data,
          Manager => Object.Manager);
   begin
      Enqueue (Job);
   end Load;

   function Create_Loader
     (Manager : Managers.Manager_Ptr) return Loaders.Loader_Ptr
   is (new KTX_Loader'(Manager => Manager));

   -----------------------------------------------------------------------------
   --                                 Writer                                  --
   -----------------------------------------------------------------------------

   package Pointers is new GL.Objects.Textures.Texture_Pointers (Byte_Pointers);

   procedure Write_Texture
     (Texture  : Rendering.Textures.Texture;
      Location : Locations.Writable_Location_Ptr;
      Path     : String)
   is
      use Ada.Streams;
      use all type Rendering.Textures.LE.Texture_Kind;

      Compressed : constant Boolean := Texture.Description.Compressed;

      Header : Orka.KTX.Header (Compressed);

      function Convert
        (Bytes : in out GL.Types.Pointers.UByte_Array_Access) return Byte_Array_Pointers.Pointer
      is
         Pointer : Byte_Array_Pointers.Pointer;
         Result  : constant Byte_Array_Access := new Byte_Array (1 .. Bytes'Length);

         procedure Free is new Ada.Unchecked_Deallocation
           (Object => GL.Types.UByte_Array, Name => GL.Types.Pointers.UByte_Array_Access);
      begin
         for Index in Bytes'Range loop
            declare
               Target_Index : constant Stream_Element_Offset
                 := Stream_Element_Offset (Index - Bytes'First + 1);
            begin
               Result (Target_Index) := Stream_Element (Bytes (Index));
            end;
         end loop;
         Free (Bytes);
         Pointer.Set (Result);
         return Pointer;
      end Convert;

      function Convert
        (Bytes : in out Pointers.Element_Array_Access) return Byte_Array_Pointers.Pointer
      is
         Pointer : Byte_Array_Pointers.Pointer;
         Result  : constant Byte_Array_Access := new Byte_Array (1 .. Bytes'Length);
      begin
         Result.all := Bytes.all;
         Pointers.Free (Bytes);
         Pointer.Set (Result);
         return Pointer;
      end Convert;

      function Get_Data
        (Level : Rendering.Textures.Mipmap_Level) return Byte_Array_Pointers.Pointer
      is
         Data : Pointers.Element_Array_Access;

         Internal_Format : constant GL.Pixels.Internal_Format
           := Texture.Description.Format;

         Format    : constant GL.Pixels.Format    := PE.Texture_Format    (Internal_Format);
         Data_Type : constant GL.Pixels.Data_Type := PE.Texture_Data_Type (Internal_Format);

         Is_Packed  : constant Boolean := Data_Type in PE.Packed_Data_Type;

         Original_Alignment : constant GL.Pixels.Alignment :=
           GL.Pixels.Pack_Alignment;

         Level_Size : constant Size_3D := Texture.Size (Level);
      begin
         GL.Pixels.Set_Pack_Alignment (if Is_Packed then GL.Pixels.Bytes else GL.Pixels.Words);
         Data := Pointers.Get_Data (Texture.GL_Texture, Level, 0, 0, 0,
           Level_Size (X), Level_Size (Y), Level_Size (Z),
           Format, Data_Type);
         GL.Pixels.Set_Pack_Alignment (Original_Alignment);
         return Convert (Data);
      end Get_Data;

      function Get_Compressed_Data
        (Level : Rendering.Textures.Mipmap_Level) return Byte_Array_Pointers.Pointer
      is
         Data : GL.Types.Pointers.UByte_Array_Access;

         Level_Size : constant Size_3D := Texture.Size (Level);
      begin
         Data := Texture.GL_Texture.Get_Compressed_Data (Level, 0, 0, 0,
           Level_Size (X), Level_Size (Y), Level_Size (Z),
           Texture.Description.Compressed_Format);
         return Convert (Data);
      end Get_Compressed_Data;

      T1, T2, T3, T4 : Duration;
   begin
      T1 := Orka.OS.Monotonic_Clock;

      Header.Kind := Texture.Kind;

      Header.Width := Texture.Description.Size (X);
      case Texture.Kind is
         when Texture_3D =>
            Header.Height := Texture.Description.Size (Y);
            Header.Depth  := Texture.Description.Size (Z);
         when Texture_2D | Texture_2D_Array | Texture_Cube_Map | Texture_Cube_Map_Array
               | Texture_Rectangle =>
            Header.Height := Texture.Description.Size (Y);
            Header.Depth  := 0;
         when Texture_1D | Texture_1D_Array =>
            Header.Height := 0;
            Header.Depth  := 0;
         when others =>
            raise Program_Error;
      end case;

      case Texture.Kind is
         when Texture_1D_Array =>
            Header.Array_Elements := Texture.Description.Size (Y);
         when Texture_2D_Array =>
            Header.Array_Elements := Texture.Description.Size (Z);
         when Texture_Cube_Map_Array =>
            Header.Array_Elements := Texture.Description.Size (Z) / 6;
         when Texture_1D | Texture_2D | Texture_3D | Texture_Cube_Map | Texture_Rectangle =>
            Header.Array_Elements := 0;
         when others =>
            raise Program_Error;
      end case;

      Header.Mipmap_Levels   := Rendering.Textures.Mipmap_Level (Texture.Description.Levels);
      Header.Bytes_Key_Value := 0;

      if Compressed then
         Header.Compressed_Format := Texture.Description.Compressed_Format;
      else
         declare
            Internal_Format : constant GL.Pixels.Internal_Format
              := Texture.Description.Format;

            Format    : constant GL.Pixels.Format    := PE.Texture_Format    (Internal_Format);
            Data_Type : constant GL.Pixels.Data_Type := PE.Texture_Data_Type (Internal_Format);
         begin
            Header.Internal_Format := Internal_Format;
            Header.Format    := Format;
            Header.Data_Type := Data_Type;
         end;
      end if;

      T2 := Orka.OS.Monotonic_Clock;

      GL.Barriers.Memory_Barrier ((Texture_Update => True, others => False));

      declare
         function Get_Level_Data
           (Level : Rendering.Textures.Mipmap_Level) return Byte_Array_Pointers.Pointer
         is (if Compressed then Get_Compressed_Data (Level) else Get_Data (Level));

         Bytes : constant Byte_Array_Pointers.Pointer
           := Orka.KTX.Create_KTX_Bytes (Header, Get_Level_Data'Access);

         Is_Packed  : constant Boolean := Header.Data_Type in PE.Packed_Data_Type;
         Components : constant Positive := Integer (PE.Components (Header.Format));
      begin
         T3 := Orka.OS.Monotonic_Clock;

         Location.Write_Data (Path, Bytes.Get);

         T4 := Orka.OS.Monotonic_Clock;

         Log (Info, "Saved texture " & Path & " in " &
           Logging.Trim (Logging.Image (+(T4 - T1))));
         Log (Info, "  dims:   " &
            Logging.Trim (Texture.Description.Size (X)'Image) & Strings.Unicode (" × ") &
            Logging.Trim (Texture.Description.Size (Y)'Image) & Strings.Unicode (" × ") &
            Logging.Trim (Texture.Description.Size (Z)'Image) &
            ", mipmap levels:" & Texture.Description.Levels'Image);
         Log (Info, "  size:   " & Trim_Image (Bytes.Get.Value'Length) & " bytes");
         Log (Info, "  kind:   " & Texture.Kind'Image);
         if Header.Compressed then
            Log (Info, "  format: " & Texture.Description.Compressed_Format'Image);
         else
            Log (Info, "  format: " & Texture.Description.Format'Image &
              (if Is_Packed then
                 " (packed with " & Trim_Image (Components) & " components)"
               else
                 " (" & Trim_Image (Components) &
                 "x " & Header.Data_Type'Image & ")"));
         end if;

         Log (Debug, "  timing:");
         Log (Debug, "    creating header: " & Logging.Image (+(T2 - T1)));
         Log (Debug, "    retrieving data: " & Logging.Image (+(T3 - T2)));
         Log (Debug, "    writing file:    " & Logging.Image (+(T4 - T3)));
      end;
   end Write_Texture;

end Orka.Resources.Textures.KTX;
