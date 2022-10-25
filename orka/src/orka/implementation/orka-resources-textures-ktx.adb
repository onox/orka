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

with GL.Low_Level.Enums;
with GL.Pixels.Extensions;
with GL.Types.Pointers;

with Orka.Jobs;
with Orka.KTX;
with Orka.Logging.Default;
with Orka.OS;
with Orka.Strings;

package body Orka.Resources.Textures.KTX is

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
      Start : Time) return GL.Objects.Textures.Texture
   is
      T1 : Time renames Start;
      T2 : constant Time := Orka.OS.Monotonic_Clock;

      use Ada.Streams;
      use GL.Low_Level.Enums;

      T3, T4, T5, T6 : Time;
   begin
      if not Orka.KTX.Valid_Identifier (Bytes) then
         raise Texture_Load_Error with Path & " is not a KTX file";
      end if;

      declare
         Header : constant Orka.KTX.Header := Orka.KTX.Get_Header (Bytes);
         Levels : constant Size := Size'Max (1, Header.Mipmap_Levels);

         Texture  : GL.Objects.Textures.Texture (Header.Kind);

         Width  : constant Size := Header.Width;
         Height :          Size := Header.Height;
         Depth  :          Size := Header.Depth;
      begin
         T3 := Orka.OS.Monotonic_Clock;

         if not Header.Compressed
           and then Header.Data_Type in GL.Pixels.Extensions.Packed_Data_Type
         then
            raise GL.Feature_Not_Supported_Exception with
              "Packed data type " & Header.Data_Type'Image & " is not supported yet";
         end if;

         --  Allocate storage
         case Header.Kind is
            when Texture_2D_Array =>
               Depth := Header.Array_Elements;
            when Texture_1D_Array =>
               Height := Header.Array_Elements;
               pragma Assert (Depth = 0);
            when Texture_Cube_Map_Array =>
               --  For a cube map array, depth is the number of layer-faces
               Depth := Header.Array_Elements * 6;
            when Texture_3D =>
               null;
            when Texture_2D | Texture_Cube_Map =>
               pragma Assert (Depth = 0);
            when Texture_1D =>
               if Header.Compressed then
                  raise Texture_Load_Error with Path & " has unknown 1D compressed format";
               end if;
               pragma Assert (Height = 0);
               pragma Assert (Depth = 0);
            when others =>
               raise Program_Error;
         end case;

         if Header.Compressed then
            Texture.Allocate_Storage (Levels, 1, Header.Compressed_Format,
              Width, Height, Depth);
         else
            Texture.Allocate_Storage (Levels, 1, Header.Internal_Format,
              Width, Height, Depth);
         end if;

         case Header.Kind is
            when Texture_1D =>
               Height := 1;
               Depth  := 1;
            when Texture_1D_Array | Texture_2D =>
               Depth := 1;
            when Texture_2D_Array | Texture_3D =>
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
            for Level in 0 .. Levels - 1 loop
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

                  Level_Width  : constant Size := Texture.Width  (Level);
                  Level_Height : constant Size := Texture.Height (Level);
                  Level_Depth  : constant Size :=
                    (if Cube_Map then 6 else Texture.Depth (Level));
               begin
                  if Header.Compressed then
                     Texture.Load_From_Data
                       (Level, 0, 0, 0, Level_Width, Level_Height, Level_Depth,
                        Header.Compressed_Format, Integer_32 (Image_Size), Image_Data);
                  else
                     declare
                        Original_Alignment : constant GL.Pixels.Alignment :=
                          GL.Pixels.Unpack_Alignment;
                     begin
                        GL.Pixels.Set_Unpack_Alignment (GL.Pixels.Words);
                        Texture.Load_From_Data (Level, 0, 0, 0,
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
         if Header.Mipmap_Levels = 0 then
            Texture.Generate_Mipmap;
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
            Log (Info, "  format: " & Header.Internal_Format'Image &
              " (" & Trim_Image (Integer (GL.Pixels.Extensions.Components (Header.Format))) &
              "x " & Header.Data_Type'Image & ")");
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
      Path     : String) return GL.Objects.Textures.Texture
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
      Bytes : Byte_Array_Pointers.Constant_Reference := Object.Data.Bytes.Get;
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
     (Texture  : GL.Objects.Textures.Texture;
      Location : Locations.Writable_Location_Ptr;
      Path     : String)
   is
      package Textures renames GL.Objects.Textures;

      Format    : GL.Pixels.Format    := GL.Pixels.RGBA;
      Data_Type : GL.Pixels.Data_Type := GL.Pixels.Float;
      --  Note: unused if texture is compressed

      Base_Level : constant := 0;

      use Ada.Streams;
      use all type GL.Low_Level.Enums.Texture_Kind;

      Compressed : constant Boolean := Texture.Compressed;

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
        (Level : Textures.Mipmap_Level) return Byte_Array_Pointers.Pointer
      is
         Data : Pointers.Element_Array_Access;
      begin
         Data := Pointers.Get_Data (Texture, Level, 0, 0, 0,
           Texture.Width (Level), Texture.Height (Level), Texture.Depth (Level),
           Format, Data_Type);
         return Convert (Data);
      end Get_Data;

      function Get_Compressed_Data
        (Level : Textures.Mipmap_Level) return Byte_Array_Pointers.Pointer
      is
         Data : GL.Types.Pointers.UByte_Array_Access;
      begin
         Data := Textures.Get_Compressed_Data (Texture, Level, 0, 0, 0,
           Texture.Width (Level), Texture.Height (Level), Texture.Depth (Level),
           Texture.Compressed_Format);
         return Convert (Data);
      end Get_Compressed_Data;

      T1, T2, T3, T4 : Duration;
   begin
      T1 := Orka.OS.Monotonic_Clock;

      Header.Kind := Texture.Kind;

      Header.Width := Texture.Width (Base_Level);
      case Texture.Kind is
         when Texture_3D =>
            Header.Height := Texture.Height (Base_Level);
            Header.Depth  := Texture.Depth (Base_Level);
         when Texture_2D | Texture_2D_Array | Texture_Cube_Map | Texture_Cube_Map_Array =>
            Header.Height := Texture.Height (Base_Level);
            Header.Depth  := 0;
         when Texture_1D | Texture_1D_Array =>
            Header.Height := 0;
            Header.Depth  := 0;
         when others =>
            raise Program_Error;
      end case;

      case Texture.Kind is
         when Texture_1D_Array =>
            Header.Array_Elements := Texture.Height (Base_Level);
         when Texture_2D_Array =>
            Header.Array_Elements := Texture.Depth (Base_Level);
         when Texture_Cube_Map_Array =>
            Header.Array_Elements := Texture.Depth (Base_Level) / 6;
         when Texture_1D | Texture_2D | Texture_3D | Texture_Cube_Map =>
            Header.Array_Elements := 0;
         when others =>
            raise Program_Error;
      end case;

      Header.Mipmap_Levels   := Texture.Mipmap_Levels;
      Header.Bytes_Key_Value := 0;

      if Compressed then
         Header.Compressed_Format := Texture.Compressed_Format;
      else
         declare
            Internal_Format : constant GL.Pixels.Internal_Format
              := Texture.Internal_Format;
         begin
            Format    := GL.Pixels.Extensions.Texture_Format    (Internal_Format);
            Data_Type := GL.Pixels.Extensions.Texture_Data_Type (Internal_Format);

            Header.Internal_Format := Internal_Format;
            Header.Format    := Format;
            Header.Data_Type := Data_Type;
         end;
      end if;

      T2 := Orka.OS.Monotonic_Clock;

      declare
         function Get_Level_Data
           (Level : Textures.Mipmap_Level) return Byte_Array_Pointers.Pointer
         is (if Compressed then Get_Compressed_Data (Level) else Get_Data (Level));

         Bytes : constant Byte_Array_Pointers.Pointer
           := Orka.KTX.Create_KTX_Bytes (Header, Get_Level_Data'Access);
      begin
         T3 := Orka.OS.Monotonic_Clock;

         Location.Write_Data (Path, Bytes.Get);

         T4 := Orka.OS.Monotonic_Clock;

         Log (Info, "Saved texture " & Path & " in " &
           Logging.Trim (Logging.Image (+(T4 - T1))));
         Log (Info, "  dims:   " &
            Logging.Trim (Texture.Width (Base_Level)'Image) & Strings.Unicode (" × ") &
            Logging.Trim (Texture.Height (Base_Level)'Image) & Strings.Unicode (" × ") &
            Logging.Trim (Texture.Depth (Base_Level)'Image) &
            ", mipmap levels:" & Texture.Mipmap_Levels'Image);
         Log (Info, "  size:   " & Trim_Image (Bytes.Get.Value'Length) & " bytes");
         Log (Info, "  kind:   " & Texture.Kind'Image);
         if Header.Compressed then
            Log (Info, "  format: " & Texture.Compressed_Format'Image);
         else
            Log (Info, "  format: " & Texture.Internal_Format'Image &
              " (" & Trim_Image (Integer (GL.Pixels.Extensions.Components (Header.Format))) &
              "x " & Header.Data_Type'Image & ")");
         end if;

         Log (Debug, "  timing:");
         Log (Debug, "    creating header: " & Logging.Image (+(T2 - T1)));
         Log (Debug, "    retrieving data: " & Logging.Image (+(T3 - T2)));
         Log (Debug, "    writing file:    " & Logging.Image (+(T4 - T3)));
      end;
   end Write_Texture;

end Orka.Resources.Textures.KTX;
