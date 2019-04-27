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

with Orka.Resources;

private package Orka.KTX is
   pragma Preelaborate;

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   use GL.Low_Level.Enums;
   use type GL.Types.Size;

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
   end record
     with Dynamic_Predicate => Header.Width > 0
       and not (Header.Height = 0 and Header.Depth > 0)
       and (if Header.Compressed then Header.Mipmap_Levels > 0)
       and (case Header.Kind is
              when Texture_1D | Texture_2D | Texture_3D => Header.Array_Elements = 0,
              when Texture_Cube_Map                     => Header.Array_Elements = 0,
              when others                               => Header.Array_Elements > 0)
       and (case Header.Kind is
              when Texture_1D | Texture_1D_Array => Header.Height = 0,
              when Texture_2D | Texture_2D_Array => Header.Height > 0 and Header.Depth = 0,
              when Texture_3D                    => Header.Depth > 0,
              when Texture_Cube_Map       => Header.Width = Header.Height and Header.Depth = 0,
              when Texture_Cube_Map_Array => Header.Width = Header.Height and Header.Depth = 0,
              when others => raise Constraint_Error);

   subtype Bytes_Reference is Resources.Byte_Array_Pointers.Constant_Reference;

   function Valid_Identifier (Bytes : Bytes_Reference) return Boolean;

   function Get_Header (Bytes : Bytes_Reference) return Header;

   function Get_Key_Value_Map
     (Bytes  : Bytes_Reference;
      Length : GL.Types.Size) return String_Maps.Map;

   function Get_Length
     (Bytes  : Bytes_Reference;
      Offset : Ada.Streams.Stream_Element_Offset) return Natural;

   function Get_Data_Offset
     (Bytes  : Bytes_Reference;
      Bytes_Key_Value : GL.Types.Size) return Ada.Streams.Stream_Element_Offset;

   Invalid_Enum_Error : exception;

   function Create_KTX_Bytes
     (KTX_Header : Header;
      Get_Data   : not null access function (Level : GL.Objects.Textures.Mipmap_Level)
                                     return Resources.Byte_Array_Pointers.Pointer)
   return Resources.Byte_Array_Pointers.Pointer;

end Orka.KTX;
