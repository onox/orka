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
      Bytes : Byte_Array_Access renames Object.Data.Bytes;
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
      begin
         T3 := Clock;

         --  Allocate storage
         case Header.Kind is
            when Texture_3D | Texture_2D_Array | Texture_Cube_Map_Array =>
               declare
                  Texture : GL.Objects.Textures.Texture_3D (Header.Kind);
                  Depth   : GL.Types.Size;
               begin
                  case Header.Kind is
                     when Texture_2D_Array =>
                        Depth := Header.Array_Elements;
                     when Texture_Cube_Map_Array =>
                        --  For a cube map array, depth is the number of layer-faces
                        Depth := Header.Array_Elements * 6;
                     when Texture_3D =>
                        Depth := Header.Depth;
                     when others =>
                        raise Program_Error;
                  end case;

                  if Header.Compressed then
                     Texture.Allocate_Storage (Levels, Header.Compressed_Format,
                       Header.Width, Header.Height, Depth);
                  else
                     Texture.Allocate_Storage (Levels, Header.Internal_Format,
                       Header.Width, Header.Height, Depth);
                  end if;
                  Resource.Texture.Replace_Element (Texture);
               end;
            when Texture_2D | Texture_1D_Array | Texture_Cube_Map =>
               declare
                  Texture : GL.Objects.Textures.Texture_2D (Header.Kind);
                  Height  : GL.Types.Size;
               begin
                  case Header.Kind is
                     when Texture_1D_Array =>
                        Height := Header.Array_Elements;
                     when Texture_2D | Texture_Cube_Map =>
                        Height := Header.Height;
                     when others =>
                        raise Program_Error;
                  end case;

                  if Header.Compressed then
                     Texture.Allocate_Storage (Levels, Header.Compressed_Format,
                       Header.Width, Height);
                  else
                     Texture.Allocate_Storage (Levels, Header.Internal_Format,
                       Header.Width, Height);
                  end if;
                  Resource.Texture.Replace_Element (Texture);
               end;
            when Texture_1D =>
               declare
                  Texture : GL.Objects.Textures.Texture_1D (Header.Kind);
               begin
                  if Header.Compressed then
                     raise Texture_Load_Error with Path & " has unknown 1D compressed format";
                  else
                     Texture.Allocate_Storage (Levels, Header.Internal_Format,
                       Header.Width);
                  end if;
                  Resource.Texture.Replace_Element (Texture);
               end;
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

            package Textures renames GL.Objects.Textures;
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
                  pragma Assert (Offset + Stream_Element_Offset (Image_Size) - 1 <= Bytes'Last);
                  Image_Data : constant System.Address := Bytes (Offset)'Address;
                  --  TODO Unpack_Alignment must be 4, but Load_From_Data wants 1 | 2 | 4
                  --  depending on Header.Data_Type

                  procedure Load_1D (Element : Textures.Texture_Base'Class) is
                     Texture : Textures.Texture_1D renames Textures.Texture_1D (Element);
                  begin
                     if Header.Compressed then
                        raise Texture_Load_Error with Path & " has unknown 1D compressed format";
                     else
                        Texture.Load_From_Data (Level, 0, Header.Width,
                          Header.Format, Header.Data_Type, Image_Data);
                     end if;
                  end Load_1D;

                  procedure Load_2D (Element : Textures.Texture_Base'Class) is
                     Texture : Textures.Texture_2D renames Textures.Texture_2D (Element);
                  begin
                     if Header.Compressed then
                        Texture.Load_From_Compressed_Data (Level, 0, 0,
                          Header.Width, Header.Height, Header.Compressed_Format,
                          GL.Types.Int (Image_Size), Image_Data);
                     else
                        Texture.Load_From_Data (Level, 0, 0,
                          Header.Width, Header.Height, Header.Format,
                          Header.Data_Type, Image_Data);
                     end if;
                  end Load_2D;

                  procedure Load_3D (Element : Textures.Texture_Base'Class) is
                     Texture : Textures.Texture_3D renames Textures.Texture_3D (Element);
                     Depth   : GL.Types.Size := Header.Depth;
                  begin
                     --  For a cube map, depth is the number of faces, for
                     --  a cube map array, depth is the number of layer-faces
                     if Header.Kind in Texture_Cube_Map | Texture_Cube_Map_Array then
                        Depth := GL.Types.Size'Max (1, Header.Array_Elements) * 6;
                     end if;

                     if Header.Compressed then
                        Texture.Load_From_Compressed_Data (Level, 0, 0, 0,
                          Header.Width, Header.Height, Depth,
                          Header.Compressed_Format, GL.Types.Int (Image_Size),
                          Image_Data);
                     else
                        Texture.Load_From_Data (Level, 0, 0, 0,
                          Header.Width, Header.Height, Depth, Header.Format,
                          Header.Data_Type, Image_Data);
                     end if;
                  end Load_3D;
               begin
                  case Header.Kind is
                     when Texture_3D | Texture_2D_Array | Texture_Cube_Map_Array =>
                        Resource.Texture.Query_Element (Load_3D'Access);
                     when Texture_Cube_Map =>
                        --  Texture_Cube_Map uses 2D storage, but 3D load operation
                        --  according to table 8.15 of the OpenGL specification
                        Resource.Texture.Query_Element (Load_3D'Access);
                     when Texture_2D | Texture_1D_Array =>
                        Resource.Texture.Query_Element (Load_2D'Access);
                     when Texture_1D =>
                        Resource.Texture.Query_Element (Load_1D'Access);
                     when others =>
                        raise Program_Error;
                  end case;
                  Image_Size_Index := Image_Size_Index + Stream_Element_Offset (Mipmap_Size);
               end;
            end loop;
         end;
         T5 := Clock;

         --  Generate a full mipmap pyramid if Mipmap_Levels = 0
         if Header.Mipmap_Levels = 0 then
            Resource.Texture.Element.Generate_Mipmap;
         end if;
         T6 := Clock;

         --  Register resource at the resource manager
         Object.Manager.Add_Resource (Path, Resource_Ptr (Resource));

         Messages.Insert (Info, "Loaded texture " & Path & " in " &
           Logging.Trim (Logging.Image (T6 - T1)));
         Messages.Insert (Info,
            "  dimensions:" & Header.Width'Image & " x " &
            Header.Height'Image & " x " & Header.Depth'Image &
            ", array length:" & Header.Array_Elements'Image &
            ", mipmap levels:" & Levels'Image);
         if Header.Compressed then
            Messages.Insert (Info, "  compressed format: " & Header.Compressed_Format'Image);
         else
            Messages.Insert (Info, "  internal: " & Header.Internal_Format'Image);
            Messages.Insert (Info, "  format: " & Header.Format'Image);
            Messages.Insert (Info, "  type: " & Header.Data_Type'Image);
         end if;

         Messages.Insert (Info, "  statistics:");
         Messages.Insert (Info, "    reading file:   " & Logging.Image (T2 - T1));
         Messages.Insert (Info, "    parsing header: " & Logging.Image (T3 - T2));
         Messages.Insert (Info, "    storage:        " & Logging.Image (T4 - T3));
         Messages.Insert (Info, "    buffers:        " & Logging.Image (T5 - T4));
         if Header.Mipmap_Levels = 0 then
            Messages.Insert (Info, "    generating mipmap:" & Logging.Image (T6 - T5));
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
     (Texture  : GL.Objects.Textures.Texture_Base'Class;
      Location : Locations.Writable_Location_Ptr;
      Path     : String)
   is
      package Textures renames GL.Objects.Textures;
      package Pointers is new Textures.Texture_Pointers (Byte_Pointers);

      Format    : GL.Pixels.Format    := GL.Pixels.RGBA;
      Data_Type : GL.Pixels.Data_Type := GL.Pixels.Float;
      --  Note: unused if texture is compressed

      use Ada.Streams;
      use all type GL.Low_Level.Enums.Texture_Kind;
      use type GL.Types.Size;

      Compressed : constant Boolean := Texture.Compressed (0);

      Header : Orka.KTX.Header (Compressed);

      function Convert
        (Bytes : in out GL.Types.UByte_Array_Access) return Byte_Array_Access
      is
         Result : constant Byte_Array_Access := new Byte_Array (1 .. Bytes'Length);

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
         return Result;
      end Convert;

      function Convert
        (Bytes : in out Pointers.Element_Array_Access) return Byte_Array_Access
      is
         Result : constant Byte_Array_Access := new Byte_Array (1 .. Bytes'Length);
      begin
         Result.all := Bytes.all;
         Pointers.Free (Bytes);
         return Result;
      end Convert;

      function Get_Data (Level : Textures.Mipmap_Level) return Byte_Array_Access is
         Data : Pointers.Element_Array_Access;
      begin
         case Texture.Kind is
            when Texture_3D | Texture_2D_Array | Texture_Cube_Map_Array | Texture_Cube_Map =>
               Data := Pointers.Get_Data (Textures.Texture_3D (Texture), Level, 0, 0, 0,
                 Texture.Width (Level), Texture.Height (Level), Texture.Depth (Level),
                 Format, Data_Type);
            when Texture_2D | Texture_1D_Array =>
               Data := Pointers.Get_Data (Textures.Texture_2D (Texture), Level, 0, 0,
                 Texture.Width (Level), Texture.Height (Level),
                 Format, Data_Type);
            when Texture_1D =>
               Data := Pointers.Get_Data (Textures.Texture_1D (Texture), Level, 0,
                 Texture.Width (Level),
                 Format, Data_Type);
            when others =>
               raise Program_Error;
         end case;
         return Convert (Data);
      end Get_Data;

      function Get_Compressed_Data (Level : Textures.Mipmap_Level) return Byte_Array_Access is
         Data : GL.Types.UByte_Array_Access;
      begin
         case Texture.Kind is
            when Texture_3D | Texture_2D_Array | Texture_Cube_Map | Texture_Cube_Map_Array =>
               Data := Textures.Get_Compressed_Data
                 (Textures.Texture_3D (Texture), Level, 0, 0, 0,
                  Texture.Width (Level), Texture.Height (Level), Texture.Depth (Level),
                  Texture.Compressed_Format (0));
            when Texture_2D | Texture_1D_Array =>
               Data := Textures.Get_Compressed_Data
                 (Textures.Texture_2D (Texture), Level, 0, 0,
                  Texture.Width (Level), Texture.Height (Level), Texture.Compressed_Format (0));
            when others =>
               raise Program_Error;
         end case;
         return Convert (Data);
      end Get_Compressed_Data;
   begin
      Header.Kind := Texture.Kind;

      Header.Width := Texture.Width (0);
      case Texture.Kind is
         when Texture_3D =>
            Header.Height := Texture.Height (0);
            Header.Depth  := Texture.Depth (0);
         when Texture_2D | Texture_2D_Array | Texture_Cube_Map | Texture_Cube_Map_Array =>
            Header.Height := Texture.Height (0);
            Header.Depth  := 0;
         when Texture_1D | Texture_1D_Array =>
            Header.Height := 0;
            Header.Depth  := 0;
         when others =>
            raise Program_Error;
      end case;

      case Texture.Kind is
         when Texture_1D_Array =>
            Header.Array_Elements := Texture.Height (0);
         when Texture_2D_Array =>
            Header.Array_Elements := Texture.Depth (0);
         when Texture_Cube_Map_Array =>
            Header.Array_Elements := Texture.Depth (0) / 6;
         when Texture_1D | Texture_2D | Texture_3D | Texture_Cube_Map =>
            Header.Array_Elements := 0;
         when others =>
            raise Program_Error;
      end case;

      Header.Mipmap_Levels   := 1;
      Header.Bytes_Key_Value := 0;

      if Compressed then
         Header.Compressed_Format := Texture.Compressed_Format (0);
      else
         declare
            Internal_Format : constant GL.Pixels.Internal_Format
              := Texture.Internal_Format (0);
         begin
            Format := GL.Pixels.Queries.Get_Texture_Format (Internal_Format, Texture.Kind);
            Data_Type := GL.Pixels.Queries.Get_Texture_Type (Internal_Format, Texture.Kind);

            Header.Internal_Format := Internal_Format;
            Header.Format    := Format;
            Header.Data_Type := Data_Type;
         end;
      end if;

      declare
         Data : Byte_Array_Access
           := (if Compressed then Get_Compressed_Data (0) else Get_Data (0));

         Bytes : Byte_Array_Access := Orka.KTX.Create_KTX_Bytes (Header, Data);
      begin
         Location.Write_Data (Path, Bytes);
         Free_Byte_Array (Data);
         Free_Byte_Array (Bytes);
      end;
   end Write_Texture;

end Orka.Resources.Textures.KTX;
