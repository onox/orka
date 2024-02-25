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

with Orka.Strings;
with Orka.Types;

package body Orka.Rendering.Textures is

   function Create_Texture (Description : Texture_Description) return Texture is
   begin
      return Result : Texture :=
        (Kind        => Description.Kind,
         Description => Description,
         others      => <>)
      do
         if Result.Description.Compressed then
            Result.Texture.Allocate_Storage
              (Levels  => Orka.Size (Description.Levels),
               Samples => Orka.Size (Description.Samples),
               Format  => Description.Compressed_Format,
               Width   => Description.Size (X),
               Height  => Description.Size (Y),
               Depth   => Description.Size (Z));
         else
            Result.Texture.Allocate_Storage
              (Levels  => Orka.Size (Description.Levels),
               Samples => Orka.Size (Description.Samples),
               Format  => Description.Format,
               Width   => Description.Size (X),
               Height  => Description.Size (Y),
               Depth   => Description.Size (Z));
         end if;
      end return;
   end Create_Texture;

   function Create_View (Object : Texture; Layer : Natural) return Texture is
      Kind : constant LE.Texture_Kind := Layer_Kind (Object.Kind);
   begin
      if Object.Description.Compressed then
         raise Constraint_Error;
      end if;

      return
        (Kind        => Kind,
         Texture     => Object.Texture.Create_View (Kind, Integer_32 (Layer)),
         Description =>
           (Kind    => Kind,
            Compressed => False,
            Format  => Object.Description.Format,
            Size    =>
              (case Kind is
                 when Texture_1D             => (Object.Description.Size (X), 1, 1),
                 when Texture_2D             => (Object.Description.Size (X), Object.Description.Size (Y), 1),
                 when Texture_2D_Multisample => (Object.Description.Size (X), Object.Description.Size (Y), 1),
                 when Texture_3D             => Object.Description.Size,
                 when Texture_Cube_Map       => (Object.Description.Size (X), Object.Description.Size (Y), 6),
                 when others => raise Constraint_Error),
            Levels  => Object.Description.Levels,
            Samples => Object.Description.Samples));
   end Create_View;

   function Size
     (Object : Texture;
      Level  : GL.Objects.Textures.Mipmap_Level := 0) return Size_3D
   is
      Has_Height : constant Boolean := Object.Kind not in Texture_1D | Texture_1D_Array;
      Has_Depth  : constant Boolean := Object.Kind = Texture_3D;
   begin
      return
        (X => Orka.Size'Max (1, Object.Description.Size (X) / 2 ** Natural (Level)),
         Y => Orka.Size'Max (1, Object.Description.Size (Y) / (if Has_Height then 2 ** Natural (Level) else 1)),
         Z => Orka.Size'Max (1, Object.Description.Size (Z) / (if Has_Depth then 2 ** Natural (Level) else 1)));
   end Size;

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

   function Levels (Size : Size_3D) return Positive is
      Max_Size : constant Orka.Size := Orka.Size'Max (Orka.Size'Max (Size (X), Size (Y)), Size (Z));
   begin
      return Orka.Types.Log2 (Natural (Max_Size)) + 1;
   end Levels;

   function Image
     (Object : Texture;
      Level  : GL.Objects.Textures.Mipmap_Level := 0) return String
   is
      Size : constant Size_3D := Object.Size (Level);

      Width  : constant String := Orka.Strings.Trim (Size (X)'Image);
      Height : constant String := Orka.Strings.Trim (Size (Y)'Image);
      Depth  : constant String := Orka.Strings.Trim (Size (Z)'Image);

      function U (Value : Wide_Wide_String) return String renames Orka.Strings.Unicode;
   begin
      return
        Width & U (" × ") & Height & U (" × ") & Depth & " " & Object.Kind'Image &
        " with " &
        (if Object.Texture.Compressed then
           Object.Texture.Compressed_Format'Image
         else
           Object.Texture.Internal_Format'Image) & " format";
   end Image;

end Orka.Rendering.Textures;
