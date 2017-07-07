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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Streams;
with Ada.Strings.Hash;

with GL.Low_Level.Enums;
with GL.Objects.Textures;
with GL.Pixels;
with GL.Types;

limited with Orka.Resources;

private package Orka.KTX is
   pragma Preelaborate;

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   type Header (Compressed : Boolean) is record
      Kind   : GL.Low_Level.Enums.Texture_Kind;
      Width  : GL.Types.Size;
      Height : GL.Types.Size;
      Depth  : GL.Types.Size;
      Array_Elements : GL.Types.Size;
      Mipmap_Levels  : GL.Objects.Textures.Mipmap_Level;
      Bytes_Key_Value : GL.Types.Size;
      case Compressed is
         when True =>
            Compressed_Format : GL.Pixels.Compressed_Format;
         when False =>
            Data_Type       : GL.Pixels.Data_Type;
            Format          : GL.Pixels.Format;
            Internal_Format : GL.Pixels.Internal_Format;
      end case;
   end record;

   function Valid_Identifier (Bytes : Resources.Byte_Array_Access) return Boolean;

   function Get_Header (Bytes : Resources.Byte_Array_Access) return Header;

   function Get_Key_Value_Map
     (Bytes  : Resources.Byte_Array_Access;
      Length : GL.Types.Size) return String_Maps.Map;

   function Get_Length
     (Bytes  : Resources.Byte_Array_Access;
      Offset : Ada.Streams.Stream_Element_Offset) return Natural;

   function Get_Data_Offset
     (Bytes  : Resources.Byte_Array_Access;
      Bytes_Key_Value : GL.Types.Size) return Ada.Streams.Stream_Element_Offset;

   Invalid_Enum_Error : exception;

end Orka.KTX;
