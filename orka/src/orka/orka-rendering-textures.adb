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

with GL.Pixels.Extensions;
with GL.Types;

with Orka.Strings;

package body Orka.Rendering.Textures is

   function Create_Texture (Description : Texture_Description) return Texture is
   begin
      return Result : Texture (Description.Kind) do
         Result.Texture.Allocate_Storage
           (Levels  => Size (Description.Levels),
            Samples => Size (Description.Samples),
            Format  => Description.Format,
            Width   => Description.Size (X),
            Height  => Description.Size (Y),
            Depth   => Description.Size (Z));
      end return;
   end Create_Texture;

   function Description (Object : Texture) return Texture_Description is
   begin
      return
        (Kind    => Object.Texture.Kind,
         Format  => Object.Texture.Internal_Format,
         Size    => (X => Object.Texture.Width (0), Y => Object.Texture.Height (0), Z => Object.Texture.Depth (0)),
         Levels  => Positive (Object.Texture.Mipmap_Levels),
         Layers  => (if Object.Texture.Layered then 1 else 1),  -- FIXME Fix layers if layered
         Samples => Natural (Object.Texture.Samples));
   end Description;

   procedure Bind (Object : Texture; Index : Natural) is
   begin
      Object.Texture.Bind_Texture_Unit (Unsigned_32 (Index));
   end Bind;

   procedure Bind_As_Image (Object : Texture; Index : Natural) is
   begin
      Object.Texture.Bind_Image_Texture (Unsigned_32 (Index));
   end Bind_As_Image;

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
