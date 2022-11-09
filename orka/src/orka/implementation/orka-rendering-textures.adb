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

with GL.Low_Level.Enums;
with GL.Pixels.Extensions;
with GL.Types;

with Orka.Strings;

package body Orka.Rendering.Textures is

   procedure Bind
     (Object : GL.Objects.Textures.Texture_Base'Class;
      Target : Indexed_Texture_Target;
      Index  : Natural) is
   begin
      case Target is
         when Texture =>
            Object.Bind_Texture_Unit (GL.Types.UInt (Index));
         when Image =>
            Object.Bind_Image_Texture (GL.Types.UInt (Index));
      end case;
   end Bind;

   function Bayer_Dithering_Pattern return GL.Objects.Samplers.Sampler is
      use all type GL.Objects.Samplers.Minifying_Function;
      use all type GL.Objects.Samplers.Magnifying_Function;
      use all type GL.Objects.Samplers.Wrapping_Mode;
   begin
      return Result : GL.Objects.Samplers.Sampler do
         Result.Set_X_Wrapping (Repeat);
         Result.Set_Y_Wrapping (Repeat);

         Result.Set_Minifying_Filter (Nearest);
         Result.Set_Magnifying_Filter (Nearest);
      end return;
   end Bayer_Dithering_Pattern;

   function Bayer_Dithering_Pattern return GL.Objects.Textures.Texture is
      Pixels : aliased constant GL.Types.UByte_Array
        := (0, 32,  8, 40,  2, 34, 10, 42,
           48, 16, 56, 24, 50, 18, 58, 26,
           12, 44,  4, 36, 14, 46,  6, 38,
           60, 28, 52, 20, 62, 30, 54, 22,
            3, 35, 11, 43,  1, 33,  9, 41,
           51, 19, 59, 27, 49, 17, 57, 25,
           15, 47,  7, 39, 13, 45,  5, 37,
           63, 31, 55, 23, 61, 29, 53, 21);
      --  * Bayer, B. E. (1973). An optimum method for two-level rendition of
      --    continuous-tone pictures. In IEEE Int. Conf. on Communications
      --    (Vol. 26, pp. 11-15).
      --  * http://www.anisopteragames.com/how-to-fix-color-banding-with-dithering/
   begin
      return Result : GL.Objects.Textures.Texture (GL.Low_Level.Enums.Texture_2D) do
         Result.Allocate_Storage (1, 1, GL.Pixels.R8, Width => 8, Height => 8, Depth => 1);
         Result.Load_From_Data
           (Level => 0,
            Width => 8, Height => 8, Depth => 1,
            Source_Format => GL.Pixels.Red,
            Source_Type   => GL.Pixels.Unsigned_Byte,
            Source        => Pixels'Address);
      end return;
   end Bayer_Dithering_Pattern;

   function Get_Format_Kind
     (Format : GL.Pixels.Internal_Format) return Format_Kind
   is
      package PE renames GL.Pixels.Extensions;
   begin
      if PE.Depth_Stencil_Format (Format) then
         return Depth_Stencil;
      elsif PE.Depth_Format (Format) then
         return Depth;
      elsif PE.Stencil_Format (Format) then
         return Stencil;
      else
         return Color;
      end if;
   end Get_Format_Kind;

   function Image
     (Texture : GL.Objects.Textures.Texture;
      Level   : GL.Objects.Textures.Mipmap_Level := 0) return String
   is
      Width  : constant String := Orka.Strings.Trim (Texture.Width  (Level)'Image);
      Height : constant String := Orka.Strings.Trim (Texture.Height (Level)'Image);
      Depth  : constant String := Orka.Strings.Trim (Texture.Depth  (Level)'Image);

      function U (Value : Wide_Wide_String) return String renames Orka.Strings.Unicode;
   begin
      return (if Texture.Allocated then "" else "unallocated ") &
        Width & U (" × ") & Height & U (" × ") & Depth & " " & Texture.Kind'Image &
        " with " &
        (if Texture.Compressed then
           Texture.Compressed_Format'Image
         else
           Texture.Internal_Format'Image) & " format";
   end Image;

end Orka.Rendering.Textures;
