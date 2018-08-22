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

with Ada.Exceptions;
with Ada.Real_Time;

with GL.Debug;
with GL.Low_Level.Enums;
with GL.Types;

with Orka.KTX;
with Orka.Resources.Locations;

package body Orka.Resources.Textures.KTX is

   package Debug_Messages is new GL.Debug.Messages (GL.Debug.Third_Party, GL.Debug.Other);

   type KTX_Load_Job is new Jobs.Abstract_Job and Jobs.GPU_Job with record
      Data    : Loaders.Resource_Data;
      Manager : Managers.Manager_Ptr;
   end record;

   overriding
   procedure Execute
     (Object  : KTX_Load_Job;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr));

   -----------------------------------------------------------------------------

   overriding
   procedure Execute
     (Object  : KTX_Load_Job;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr))
   is
      Bytes : Byte_Array_Access renames Object.Data.Bytes;
      Path  : String renames SU.To_String (Object.Data.Path);

      T1 : Ada.Real_Time.Time renames Object.Data.Start_Time;
      T2 : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

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
         T3 := Ada.Real_Time.Clock;

         --  Allocate storage
         case Header.Kind is
            when Texture_3D | Texture_2D_Array | Texture_Cube_Map_Array =>
               declare
                  Texture : GL.Objects.Textures.Texture_3D (Header.Kind);
               begin
                  if Header.Compressed then
                     Texture.Allocate_Storage (Levels, Header.Compressed_Format,
                       Header.Width, Header.Height, Header.Depth);
                  else
                     Texture.Allocate_Storage (Levels, Header.Internal_Format,
                       Header.Width, Header.Height, Header.Depth);
                  end if;
                  Resource.Texture.Replace_Element (Texture);
               end;
            when Texture_2D | Texture_1D_Array | Texture_Cube_Map =>
               declare
                  Texture : GL.Objects.Textures.Texture_2D (Header.Kind);
               begin
                  if Header.Compressed then
                     Texture.Allocate_Storage (Levels, Header.Compressed_Format,
                       Header.Width, Header.Height);
                  else
                     Texture.Allocate_Storage (Levels, Header.Internal_Format,
                       Header.Width, Header.Height);
                  end if;
                  Resource.Texture.Replace_Element (Texture);
               end;
            when Texture_1D =>
               declare
                  Texture : GL.Objects.Textures.Texture_1D (Header.Kind);
               begin
                  if Header.Compressed then
                     Texture.Allocate_Storage (Levels, Header.Compressed_Format,
                       Header.Width);
                  else
                     Texture.Allocate_Storage (Levels, Header.Internal_Format,
                       Header.Width);
                  end if;
                  Resource.Texture.Replace_Element (Texture);
               end;
            when others =>
               raise Program_Error;
         end case;
         T4 := Ada.Real_Time.Clock;

         --  TODO Handle KTXorientation key value pair

         --  Upload texture data
         declare
            Image_Size_Index : Stream_Element_Offset
              := Orka.KTX.Get_Data_Offset (Bytes, Header.Bytes_Key_Value);

            package Textures renames GL.Objects.Textures;
         begin
            for Level in 0 .. Levels - 1 loop
               declare
                  Image_Size   : constant Natural := Orka.KTX.Get_Length (Bytes, Image_Size_Index);
                  Padding_Size : constant Natural := 3 - ((Image_Size + 3) mod 4);
                  Mipmap_Size  : constant Natural := 4 + Image_Size + Padding_Size;

                  Image_Data : aliased Byte_Array := Bytes (Image_Size_Index + 4 .. Image_Size_Index + 4 + Stream_Element_Offset (Image_Size) - 1);

                  procedure Load_1D (Element : Textures.Texture_Base'Class) is
                     Texture : Textures.Texture_1D renames Textures.Texture_1D (Element);
                  begin
                     if Header.Compressed then
                        Texture.Load_From_Compressed_Data (Level, 0, Header.Width,
                          Header.Compressed_Format, GL.Types.Int (Image_Size),
                          Image_Data'Address);
                     else
                        Texture.Load_From_Data (Level, 0, Header.Width,
                          Header.Format, Header.Data_Type, Image_Data'Address);
                     end if;
                  end Load_1D;

                  procedure Load_2D (Element : Textures.Texture_Base'Class) is
                     Texture : Textures.Texture_2D renames Textures.Texture_2D (Element);
                  begin
                     if Header.Compressed then
                        Texture.Load_From_Compressed_Data (Level, 0, 0,
                          Header.Width, Header.Height, Header.Compressed_Format,
                          GL.Types.Int (Image_Size), Image_Data'Address);
                     else
                        Texture.Load_From_Data (Level, 0, 0,
                          Header.Width, Header.Height, Header.Format,
                          Header.Data_Type, Image_Data'Address);
                     end if;
                  end Load_2D;

                  procedure Load_3D (Element : Textures.Texture_Base'Class) is
                     Texture : Textures.Texture_3D renames Textures.Texture_3D (Element);
                  begin
                     if Header.Compressed then
                        Texture.Load_From_Compressed_Data (Level, 0, 0, 0,
                          Header.Width, Header.Height, Header.Depth,
                          Header.Compressed_Format, GL.Types.Int (Image_Size),
                          Image_Data'Address);
                        --  TODO For Texture_Cube_Map, Image_Size is the size of one face
                     else
                        Texture.Load_From_Data (Level, 0, 0, 0,
                          Header.Width, Header.Height, Header.Depth, Header.Format,
                          Header.Data_Type, Image_Data'Address);
                     end if;
                  end Load_3D;
               begin
                  case Header.Kind is
                     when Texture_3D | Texture_2D_Array | Texture_Cube_Map_Array =>
                        Resource.Texture.Query_Element (Load_3D'Access);
                     when Texture_2D | Texture_1D_Array | Texture_Cube_Map =>
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
         T5 := Ada.Real_Time.Clock;

         --  Generate a full mipmap pyramid if Mipmap_Levels = 0
         if Header.Mipmap_Levels = 0 then
            Resource.Texture.Element.Generate_Mipmap;
         end if;
         T6 := Ada.Real_Time.Clock;

         --  Register resource at the resource manager
         Object.Manager.Add_Resource (Path, Resource_Ptr (Resource));

         declare
            use type Ada.Real_Time.Time;

            Reading_Time : constant Duration := 1e3 * Ada.Real_Time.To_Duration (T2 - T1);
            Parsing_Time : constant Duration := 1e3 * Ada.Real_Time.To_Duration (T3 - T2);
            Storage_Time : constant Duration := 1e3 * Ada.Real_Time.To_Duration (T4 - T3);
            Buffers_Time : constant Duration := 1e3 * Ada.Real_Time.To_Duration (T5 - T4);
            Mipmap_Time  : constant Duration := 1e3 * Ada.Real_Time.To_Duration (T6 - T5);

            Loading_Time : constant Duration := 1e3 * Ada.Real_Time.To_Duration (T6 - T1);
         begin
            Debug_Messages.Insert (GL.Debug.Notification,
               "Loaded texture " & Path);
            Debug_Messages.Insert (GL.Debug.Notification,
               "  width:" & Header.Width'Image &
               " height:" & Header.Height'Image &
               " depth:" & Header.Depth'Image &
               " array length:" & Header.Array_Elements'Image &
               " mipmap levels:" & Levels'Image);
            if Header.Compressed then
               Debug_Messages.Insert (GL.Debug.Notification,
                  "  compressed format: " & Header.Compressed_Format'Image);
            else
               Debug_Messages.Insert (GL.Debug.Notification,
                  "  internal: " & Header.Internal_Format'Image);
               Debug_Messages.Insert (GL.Debug.Notification,
                  "  format: " & Header.Format'Image);
               Debug_Messages.Insert (GL.Debug.Notification,
                  "  format: " & Header.Data_Type'Image);
            end if;

            Debug_Messages.Insert (GL.Debug.Notification,
               "  loaded in" & Loading_Time'Image & " ms");
            Debug_Messages.Insert (GL.Debug.Notification,
               "    reading file:" & Reading_Time'Image & " ms");
            Debug_Messages.Insert (GL.Debug.Notification,
               "    parsing header:" & Parsing_Time'Image & " ms");
            Debug_Messages.Insert (GL.Debug.Notification,
               "    storage:" & Storage_Time'Image & " ms");
            Debug_Messages.Insert (GL.Debug.Notification,
               "    buffers:" & Buffers_Time'Image & " ms");
            if Header.Mipmap_Levels = 0 then
               Debug_Messages.Insert (GL.Debug.Notification,
                  "    generating mipmap:" & Mipmap_Time'Image & " ms");
            end if;
         end;
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

end Orka.Resources.Textures.KTX;
