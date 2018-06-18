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

with GL.Debug;
with GL.Low_Level.Enums;
with GL.Pixels;
with GL.Types;

with Orka.KTX;

package body Orka.Resources.Textures.KTX is

   package Debug_Messages is new GL.Debug.Messages (GL.Debug.Third_Party, GL.Debug.Other);

   function Load_Texture (Path : String) return Texture is
      T1 : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

--      File : Byte_Array_File'Class := Open_File (Path);
--      Bytes : constant Byte_Array_Access := File.Read_File;
      Bytes : constant Byte_Array_Access := null;
      --  FIXME Use Loader task or Location object

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
      begin
         return Result : Texture do
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
                     Result.Texture.Replace_Element (Texture);
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
                     Result.Texture.Replace_Element (Texture);
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
                     Result.Texture.Replace_Element (Texture);
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

                     Image_Data : Byte_Array := Bytes (Image_Size_Index + 4 .. Image_Size_Index + 4 + Stream_Element_Offset (Image_Size) - 1);

                     procedure Load_1D (Element : Textures.Texture_Base'Class) is
                        Texture : Textures.Texture_1D renames Textures.Texture_1D (Element);
                     begin
                        if Header.Compressed then
                           Texture.Load_From_Compressed_Data (Level, 0, Header.Width,
                             Header.Compressed_Format, GL.Types.Int (Image_Size), Image_Data'Address);
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
                           Result.Texture.Query_Element (Load_3D'Access);
                        when Texture_2D | Texture_1D_Array | Texture_Cube_Map =>
                           Result.Texture.Query_Element (Load_2D'Access);
                        when Texture_1D =>
                           Result.Texture.Query_Element (Load_1D'Access);
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
               Result.Texture.Element.Generate_Mipmap;
            end if;
            T6 := Ada.Real_Time.Clock;

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
                  "  width:" & GL.Types.Size'Image (Header.Width) &
                  " height:" & GL.Types.Size'Image (Header.Height) &
                  " depth:" & GL.Types.Size'Image (Header.Depth) &
                  " array length:" & GL.Types.Size'Image (Header.Array_Elements) &
                  " mipmap levels:" & GL.Types.Size'Image (Levels));
               if Header.Compressed then
                  Debug_Messages.Insert (GL.Debug.Notification,
                     "  compressed format: " & GL.Pixels.Compressed_Format'Image (Header.Compressed_Format));
               else
                  Debug_Messages.Insert (GL.Debug.Notification,
                     "  internal: " & GL.Pixels.Internal_Format'Image (Header.Internal_Format));
                  Debug_Messages.Insert (GL.Debug.Notification,
                     "  format: " & GL.Pixels.Format'Image (Header.Format));
                  Debug_Messages.Insert (GL.Debug.Notification,
                     "  format: " & GL.Pixels.Data_Type'Image (Header.Data_Type));
               end if;

               Debug_Messages.Insert (GL.Debug.Notification,
                  "  loaded in" & Duration'Image (Loading_Time) & " ms");
               Debug_Messages.Insert (GL.Debug.Notification,
                  "    reading file:" & Duration'Image (Reading_Time) & " ms");
               Debug_Messages.Insert (GL.Debug.Notification,
                  "    parsing header:" & Duration'Image (Parsing_Time) & " ms");
               Debug_Messages.Insert (GL.Debug.Notification,
                  "    storage:" & Duration'Image (Storage_Time) & " ms");
               Debug_Messages.Insert (GL.Debug.Notification,
                  "    buffers:" & Duration'Image (Buffers_Time) & " ms");
               if Header.Mipmap_Levels = 0 then
                  Debug_Messages.Insert (GL.Debug.Notification,
                     "    generating mipmap:" & Duration'Image (Mipmap_Time) & " ms");
               end if;
            end;
         end return;
      end;
   exception
      when Error : Orka.KTX.Invalid_Enum_Error =>
         declare
            Message : constant String := Ada.Exceptions.Exception_Message (Error);
         begin
            raise Texture_Load_Error with Path & " has " & Message;
         end;
   end Load_Texture;

   procedure Load
     (Bytes  : in out Byte_Array_Access;
      Time   : Ada.Real_Time.Time_Span;
      Path   : SU.Unbounded_String;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr)) is
   begin
      --  FIXME Implement
      null;
   end Load;

end Orka.Resources.Textures.KTX;
