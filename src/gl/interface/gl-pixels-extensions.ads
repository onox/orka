--  SPDX-License-Identifier: Apache-2.0
--
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

   function Texture_Format (Format : Pixels.Internal_Format) return Pixels.Format is
     (case Format is
        when Depth_Component16 | Depth_Component24 | Depth_Component32F =>
           Depth_Component,
        when Depth24_Stencil8 | Depth32F_Stencil8 =>
           Depth_Stencil,
        when Stencil_Index8 =>
           Stencil_Index,
        when R8 | R8_SNorm | R16 | R16_SNorm | R16F | R32F =>
           Red,
        when RG8 | RG8_SNorm | RG16 | RG16_SNorm | RG16F | RG32F =>
           RG,
        when R3_G3_B2 | RGB4 | RGB5 | RGB8 | RGB8_SNorm | RGB10 | RGB12 |
          RGB16 | RGB16_SNorm | RGB16F | RGB32F | R11F_G11F_B10F | RGB9_E5 | SRGB8 =>
           RGB,
        when RGBA2 | RGBA4 | RGB5_A1 | RGBA8 | RGBA8_SNorm | RGB10_A2 | RGBA12 |
          RGBA16 | RGBA16_SNorm | RGBA16F | RGBA32F | SRGB8_Alpha8 =>
           RGBA,
        when R8I | R8UI | R16I | R16UI | R32I | R32UI =>
           Red_Integer,
        when RG8I | RG8UI | RG16I | RG16UI | RG32I | RG32UI =>
           RG_Integer,
        when RGB8I | RGB8UI | RGB16I | RGB16UI | RGB32I | RGB32UI =>
           RGB_Integer,
        when RGBA8I | RGBA8UI | RGBA16I | RGBA16UI | RGBA32I | RGBA32UI | RGB10_A2UI =>
           RGBA_Integer);

   function Texture_Data_Type (Format : Pixels.Internal_Format) return Pixels.Data_Type is
     (case Format is
        when Depth_Component16 =>
           Unsigned_Short,
        when Depth_Component24 =>
           Unsigned_Int,
        when Depth24_Stencil8 =>
           Unsigned_Int_24_8,
        when Depth32F_Stencil8 =>
           Float_32_Unsigned_Int_24_8_Rev,
        when Depth_Component32F =>
           Float,
        when Stencil_Index8 =>
           Unsigned_Byte,
        --  Educated guesses, might be wrong
        --  Based on Table 8.13 of the OpenGL specification

        when R3_G3_B2 =>
           Unsigned_Byte_2_3_3_Rev,
        when RGB5_A1 =>
           Unsigned_Short_1_5_5_5_Rev,
        when RGB9_E5 =>
           Unsigned_Int_5_9_9_9_Rev,
        when R11F_G11F_B10F =>
           Unsigned_Int_10F_11F_11F_Rev,
        when RGB10_A2UI | RGB10_A2 =>
           Unsigned_Int_2_10_10_10_Rev,
        when R8I | RG8I | RGB8I | RGBA8I | R8_SNorm | RG8_SNorm | RGB8_SNorm | RGBA8_SNorm =>
           Byte,
        when R8UI | RG8UI | RGB8UI | RGBA8UI | R8 | RG8 | RGB8 | SRGB8 | RGBA8 | SRGB8_Alpha8 =>
           Unsigned_Byte,
        when R16I | RG16I | RGB16I | RGBA16I | R16_SNorm | RG16_SNorm | RGB16_SNorm | RGBA16_SNorm =>
           Short,
        when R16UI | RG16UI | RGB16UI | RGBA16UI | R16 | RG16 | RGB16 | RGBA16 =>
           Unsigned_Short,
        when R32I | RG32I | RGB32I | RGBA32I =>
           Int,
        when R32UI | RG32UI | RGB32UI | RGBA32UI =>
           Unsigned_Int,
        when R32F | RG32F | RGB32F | RGBA32F =>
           Float,
        when R16F | RG16F | RGB16F | RGBA16F =>
           Half_Float,
        --  Based on Table 8.8 and 8.27 of the OpenGL specification

        when RGB4 | RGB5 | RGB10 | RGB12 | RGBA2 | RGBA4 | RGBA12 =>
          raise Constraint_Error);

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
        when Compressed_SRGB8_Alpha8_ETC2_EAC          => 16,

        --  ASTC
        when Compressed_RGBA_ASTC_4x4_KHR              => 16,
        when Compressed_RGBA_ASTC_5x4_KHR              => 16,
        when Compressed_RGBA_ASTC_5x5_KHR              => 16,
        when Compressed_RGBA_ASTC_6x5_KHR              => 16,
        when Compressed_RGBA_ASTC_6x6_KHR              => 16,
        when Compressed_RGBA_ASTC_8x5_KHR              => 16,
        when Compressed_RGBA_ASTC_8x6_KHR              => 16,
        when Compressed_RGBA_ASTC_8x8_KHR              => 16,
        when Compressed_RGBA_ASTC_10x5_KHR             => 16,
        when Compressed_RGBA_ASTC_10x6_KHR             => 16,
        when Compressed_RGBA_ASTC_10x8_KHR             => 16,
        when Compressed_RGBA_ASTC_10x10_KHR            => 16,
        when Compressed_RGBA_ASTC_12x10_KHR            => 16,
        when Compressed_RGBA_ASTC_12x12_KHR            => 16,

        when Compressed_SRGB8_ALPHA8_ASTC_4x4_KHR      => 16,
        when Compressed_SRGB8_ALPHA8_ASTC_5x4_KHR      => 16,
        when Compressed_SRGB8_ALPHA8_ASTC_5x5_KHR      => 16,
        when Compressed_SRGB8_ALPHA8_ASTC_6x5_KHR      => 16,
        when Compressed_SRGB8_ALPHA8_ASTC_6x6_KHR      => 16,
        when Compressed_SRGB8_ALPHA8_ASTC_8x5_KHR      => 16,
        when Compressed_SRGB8_ALPHA8_ASTC_8x6_KHR      => 16,
        when Compressed_SRGB8_ALPHA8_ASTC_8x8_KHR      => 16,
        when Compressed_SRGB8_ALPHA8_ASTC_10x5_KHR     => 16,
        when Compressed_SRGB8_ALPHA8_ASTC_10x6_KHR     => 16,
        when Compressed_SRGB8_ALPHA8_ASTC_10x8_KHR     => 16,
        when Compressed_SRGB8_ALPHA8_ASTC_10x10_KHR    => 16,
        when Compressed_SRGB8_ALPHA8_ASTC_12x10_KHR    => 16,
        when Compressed_SRGB8_ALPHA8_ASTC_12x12_KHR    => 16);

   -----------------------------------------------------------------------------

   function Depth_Stencil_Format (Format : Internal_Format) return Boolean is
     (Format in Depth24_Stencil8 | Depth32F_Stencil8);

   function Depth_Format (Format : Internal_Format) return Boolean is
     (Format in Depth_Component16 | Depth_Component24 | Depth_Component32F);

   function Stencil_Format (Format : Internal_Format) return Boolean is
     (Format in Stencil_Index8);

private

   function Convert is new Ada.Unchecked_Conversion
     (Source => Alignment, Target => Types.Int);

   function Byte_Alignment (Value : Alignment) return Byte_Count is
     (Convert (Value));

end GL.Pixels.Extensions;
