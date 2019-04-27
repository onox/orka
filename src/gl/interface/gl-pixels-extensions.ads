--  Copyright (c) 2018 onox <denkpadje@gmail.com>
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

with Ada.Unchecked_Conversion;

package GL.Pixels.Extensions is
   pragma Preelaborate;

   subtype Floating_Point_Format is Format range Red .. RG;

   subtype Integer_Format is Format range RG_Integer .. BGRA_Integer
     with Static_Predicate => Integer_Format /= Depth_Stencil;

   -----------------------------------------------------------------------------

   subtype Non_Packed_Data_Type is Data_Type range Byte .. Half_Float;

   subtype Packed_Data_Type is Data_Type
     range Unsigned_Byte_3_3_2 .. Float_32_Unsigned_Int_24_8_Rev;

   subtype Floating_Point_Data_Type is Data_Type
     with Static_Predicate => Floating_Point_Data_Type in
       Half_Float | Float | Unsigned_Int_10F_11F_11F_Rev | Unsigned_Int_5_9_9_9_Rev;

   -----------------------------------------------------------------------------

   function Compatible
     (Format    : Pixels.Format;
      Data_Type : Pixels.Data_Type) return Boolean
   is (not (Format in Integer_Format and Data_Type in Floating_Point_Data_Type));
   --  Floating point types are incompatible with integer formats according
   --  to Table 8.2 of the OpenGL specification

   function Components (Format : Pixels.Format) return Component_Count is
     (case Format is
        when Red | Green | Blue | Red_Integer | Green_Integer | Blue_Integer => 1,
        when RG | RG_Integer => 2,
        when RGB  | BGR  | RGB_Integer  | BGR_Integer  => 3,
        when RGBA | BGRA | RGBA_Integer | BGRA_Integer => 4,
        when others => raise Constraint_Error with "Unexpected format " & Format'Image);

   function Bytes (Data_Type : Non_Packed_Data_Type) return Byte_Count is
     (case Data_Type is
        when Byte  | Unsigned_Byte               => 1,
        when Short | Unsigned_Short | Half_Float => 2,
        when Int   | Unsigned_Int   | Float      => 4);

   function Packed_Bytes (Data_Type : Packed_Data_Type) return Byte_Count is
     (case Data_Type is
        --  Unsigned_Byte formats (Table 8.6)
        when Unsigned_Byte_3_3_2            => 1,
        when Unsigned_Byte_2_3_3_Rev        => 1,

        --  Unsigned_Short formats (Table 8.7)
        when Unsigned_Short_5_6_5           => 2,
        when Unsigned_Short_5_6_5_Rev       => 2,
        when Unsigned_Short_4_4_4_4         => 2,
        when Unsigned_Short_4_4_4_4_Rev     => 2,
        when Unsigned_Short_5_5_5_1         => 2,
        when Unsigned_Short_1_5_5_5_Rev     => 2,

        --  Unsigned_Int formats (Table 8.8)
        when Unsigned_Int_8_8_8_8           => 4,
        when Unsigned_Int_8_8_8_8_Rev       => 4,
        when Unsigned_Int_10_10_10_2        => 4,
        when Unsigned_Int_2_10_10_10_Rev    => 4,
        when Unsigned_Int_24_8              => 4,
        when Unsigned_Int_10F_11F_11F_Rev   => 4,
        when Unsigned_Int_5_9_9_9_Rev       => 4,

        --  Float_Unsigned_Int formats (Table 8.9)
        when Float_32_Unsigned_Int_24_8_Rev => raise Constraint_Error);

   function Byte_Alignment (Value : Alignment) return Byte_Count;

   -----------------------------------------------------------------------------

   subtype Compressed_Byte_Count is Types.Int range 8 .. 16
     with Static_Predicate => Compressed_Byte_Count in 8 | 16;

   function Block_Bytes (Format : Pixels.Compressed_Format) return Compressed_Byte_Count is
     (case Format is
        --  RGTC
        when Compressed_Red_RGTC1                      => 8,
        when Compressed_Signed_Red_RGTC1               => 8,
        when Compressed_RG_RGTC2                       => 16,
        when Compressed_Signed_RG_RGTC2                => 16,

        --  BPTC
        when Compressed_RGBA_BPTC_Unorm                => 16,
        when Compressed_SRGB_Alpha_BPTC_UNorm          => 16,
        when Compressed_RGB_BPTC_Signed_Float          => 16,
        when Compressed_RGB_BPTC_Unsigned_Float        => 16,

        --  EAC / ETC
        when Compressed_R11_EAC                        => 8,
        when Compressed_Signed_R11_EAC                 => 8,
        when Compressed_RG11_EAC                       => 16,
        when Compressed_Signed_RG11_EAC                => 16,
        when Compressed_RGB8_ETC2                      => 8,
        when Compressed_SRGB8_ETC2                     => 8,
        when Compressed_RGB8_Punchthrough_Alpha1_ETC2  => 8,
        when Compressed_SRGB8_Punchthrough_Alpha1_ETC2 => 8,
        when Compressed_RGBA8_ETC2_EAC                 => 16,
        when Compressed_SRGB8_Alpha8_ETC2_EAC          => 16);

private

   function Convert is new Ada.Unchecked_Conversion
     (Source => Alignment, Target => Types.Int);

   function Byte_Alignment (Value : Alignment) return Byte_Count is
     (Convert (Value));

end GL.Pixels.Extensions;
