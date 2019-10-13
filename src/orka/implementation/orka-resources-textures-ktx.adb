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
with Ada.Real_Time;
with Ada.Unchecked_Deallocation;

with GL.Low_Level.Enums;
with GL.Types;
with GL.Pixels.Queries;

with Orka.Jobs;
with Orka.Logging;
with Orka.KTX;

package body Orka.Resources.Textures.KTX is

   use Orka.Logging;
   package Messages is new Orka.Logging.Messages (Resource_Loader);

   type KTX_Load_Job is new Jobs.Abstract_Job and Jobs.GPU_Job with record
      Data    : Loaders.Resource_Data;
      Manager : Managers.Manager_Ptr;
   end record;

   overriding
   procedure Execute
     (Object  : KTX_Load_Job;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr));

   -----------------------------------------------------------------------------

   use all type Ada.Real_Time.Time;

   overriding
   procedure Execute
     (Object  : KTX_Load_Job;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr))
   is
      Bytes : Byte_Array_Pointers.Constant_Reference := Object.Data.Bytes.Get;
      Path  : String renames SU.To_String (Object.Data.Path);

      T1 : Ada.Real_Time.Time renames Object.Data.Start_Time;
      T2 : constant Ada.Real_Time.Time := Clock;

      use Ada.Streams;
      use GL.Low_Level.Enums;
      use type GL.Types.Size;

      T3, T4, T5, T6 : Ada.Real_Time.Time;
   begin
      if not Orka.KTX.Valid_Identifier (Bytes) then
         raise Texture_Load_Error with Path & " is not a KTX file";
      end if;

      declare
         Header : constant Orka.KTX.Header := Orka.KTX.Get_Header (Bytes);
         Levels : constant GL.Types.Size   := GL.Types.Size'Max (1, Header.Mipmap_Levels);

         Resource : constant Texture_Ptr := new Texture'(others => <>);
         Texture  : GL.Objects.Textures.Texture (Header.Kind);

         Width  : constant GL.Types.Size := Header.Width;
         Height :          GL.Types.Size := Header.Height;
         Depth  :          GL.Types.Size := Header.Depth;
      begin
         T3 := Clock;

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
               Depth := GL.Types.Size'Max (1, Header.Array_Elements) * 6;
            when others =>
               raise Program_Error;
         end case;
         T4 := Clock;

         --  TODO Handle KTXorientation key value pair

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
                  -- If Cube_Map then Levels = 1 so no need to add it to the expression

                  --  Compute size of the whole mipmap level
                  Mip_Padding : constant Natural := 3 - ((Image_Size + 3) mod 4);
                  Mipmap_Size : constant Natural := 4 + Image_Size + Mip_Padding;

                  Offset : constant Stream_Element_Offset := Image_Size_Index + 4;
                  pragma Assert
                    (Offset + Stream_Element_Offset (Image_Size) - 1 <= Bytes.Value'Last);
                  Image_Data : constant System.Address := Bytes (Offset)'Address;
                  --  TODO Unpack_Alignment must be 4, but Load_From_Data wants 1 | 2 | 4
                  --  depending on Header.Data_Type

                  Level_Width  : constant GL.Types.Size := Texture.Width  (Level);
                  Level_Height : constant GL.Types.Size := Texture.Height (Level);
                  Level_Depth  : constant GL.Types.Size := Texture.Depth  (Level);
               begin
                  if Header.Compressed then
                     Texture.Load_From_Data (Level, 0, 0, 0, Level_Width, Level_Height, Level_Depth,
                       Header.Compressed_Format, GL.Types.Int (Image_Size), Image_Data);
                  else
                     Texture.Load_From_Data (Level, 0, 0, 0, Level_Width, Level_Height, Level_Depth,
                       Header.Format, Header.Data_Type, Image_Data);
                  end if;

                  Image_Size_Index := Image_Size_Index + Stream_Element_Offset (Mipmap_Size);
               end;
            end loop;
         end;
         T5 := Clock;

         --  Generate a full mipmap pyramid if Mipmap_Levels = 0
         if Header.Mipmap_Levels = 0 then
            Texture.Generate_Mipmap;
         end if;
         T6 := Clock;

         Resource.Texture.Replace_Element (Texture);

         --  Register resource at the resource manager
         Object.Manager.Add_Resource (Path, Resource_Ptr (Resource));

         Messages.Log (Info, "Loaded texture " & Path & " in " &
           Logging.Trim (Logging.Image (T6 - T1)));
         Messages.Log (Info, "  size:   " &
            Logging.Trim (Width'Image) & " x " &
            Logging.Trim (Height'Image) & " x " &
            Logging.Trim (Depth'Image) &
            ", mipmap levels:" & Levels'Image);
         Messages.Log (Info, "  kind:   " & Header.Kind'Image);
         if Header.Compressed then
            Messages.Log (Info, "  format: " & Header.Compressed_Format'Image);
         else
            Messages.Log (Info, "  format: " & Header.Internal_Format'Image);
         end if;

         Messages.Log (Info, "  statistics:");
         Messages.Log (Info, "    reading file:   " & Logging.Image (T2 - T1));
         Messages.Log (Info, "    parsing header: " & Logging.Image (T3 - T2));
         Messages.Log (Info, "    storage:        " & Logging.Image (T4 - T3));
         Messages.Log (Info, "    buffers:        " & Logging.Image (T5 - T4));
         if Header.Mipmap_Levels = 0 then
            Messages.Log (Info, "    generating mipmap:" & Logging.Image (T6 - T5));
         end if;
      end;
   exception
      when Error : Orka.KTX.Invalid_Enum_Error =>
         declare
            Message : constant String := Ada.Exceptions.Exception_Message (Error);
         begin
            raise Texture_Load_Error with Path & " has " & Message;
         end;
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

   procedure Write_Texture
     (Texture  : GL.Objects.Textures.Texture;
      Location : Locations.Writable_Location_Ptr;
      Path     : String)
   is
      package Textures renames GL.Objects.Textures;
      package Pointers is new Textures.Texture_Pointers (Byte_Pointers);

      Format    : GL.Pixels.Format    := GL.Pixels.RGBA;
      Data_Type : GL.Pixels.Data_Type := GL.Pixels.Float;
      --  Note: unused if texture is compressed

      Base_Level : constant := 0;

      use Ada.Streams;
      use all type GL.Low_Level.Enums.Texture_Kind;
      use type GL.Types.Size;

      Compressed : constant Boolean := Texture.Compressed;

      Header : Orka.KTX.Header (Compressed);

      function Convert
        (Bytes : in out GL.Types.UByte_Array_Access) return Byte_Array_Pointers.Pointer
      is
         Pointer : Byte_Array_Pointers.Pointer;
         Result  : constant Byte_Array_Access := new Byte_Array (1 .. Bytes'Length);

         procedure Free is new Ada.Unchecked_Deallocation
           (Object => GL.Types.UByte_Array, Name => GL.Types.UByte_Array_Access);
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
         Data : GL.Types.UByte_Array_Access;
      begin
         Data := Textures.Get_Compressed_Data (Texture, Level, 0, 0, 0,
           Texture.Width (Level), Texture.Height (Level), Texture.Depth (Level),
           Texture.Compressed_Format);
         return Convert (Data);
      end Get_Compressed_Data;
   begin
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
            Format := GL.Pixels.Queries.Get_Texture_Format (Internal_Format, Texture.Kind);
            Data_Type := GL.Pixels.Queries.Get_Texture_Type (Internal_Format, Texture.Kind);

            Header.Internal_Format := Internal_Format;
            Header.Format    := Format;
            Header.Data_Type := Data_Type;
         end;
      end if;

      declare
         function Get_Level_Data
           (Level : Textures.Mipmap_Level) return Byte_Array_Pointers.Pointer
         is (if Compressed then Get_Compressed_Data (Level) else Get_Data (Level));

         Bytes : constant Byte_Array_Pointers.Pointer
           := Orka.KTX.Create_KTX_Bytes (Header, Get_Level_Data'Access);
      begin
         Location.Write_Data (Path, Bytes.Get);
      end;
   end Write_Texture;

end Orka.Resources.Textures.KTX;
