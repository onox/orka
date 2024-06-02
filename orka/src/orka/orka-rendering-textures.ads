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
with GL.Objects.Textures;
with GL.Pixels;

package Orka.Rendering.Textures is
   pragma Preelaborate;

   package LE renames GL.Low_Level.Enums;

   use all type LE.Texture_Kind;

   subtype Mipmap_Level is GL.Objects.Textures.Mipmap_Level;

   type Format_Kind is (Depth_Stencil, Depth, Stencil, Color);

   function Get_Format_Kind
     (Format : GL.Pixels.Internal_Format) return Format_Kind;

   function Has_Layers (Kind : LE.Texture_Kind) return Boolean is
     (Kind in Texture_1D_Array | Texture_2D_Array | Texture_2D_Multisample_Array |
              Texture_3D | Texture_Cube_Map | Texture_Cube_Map_Array);

   function Layer_Kind (Kind : LE.Texture_Kind) return LE.Texture_Kind is
     (case Kind is
        when Texture_1D_Array             => Texture_1D,
        when Texture_2D_Array             => Texture_2D,
        when Texture_2D_Multisample_Array => Texture_2D_Multisample,
        when Texture_3D                   => Texture_3D,
        when Texture_Cube_Map             => Texture_2D,
        when Texture_Cube_Map_Array       => Texture_Cube_Map,
        when others => raise Constraint_Error);
   --  Return the kind of *one* layer of a texture with the given kind

   function Layers (Kind : LE.Texture_Kind; Size : Size_3D) return Natural is
     (case Kind is
        when Texture_1D_Array             => Natural (Size (Y)),
        when Texture_2D_Array             => Natural (Size (Z)),
        when Texture_2D_Multisample_Array => Natural (Size (Z)),
        when Texture_3D                   => Natural (Size (Z)),
        when Texture_Cube_Map             => 6,
        when Texture_Cube_Map_Array       => Natural (Size (Z)),
        when Texture_Buffer               => raise Constraint_Error,
        when others                       => 0);

   function Levels (Size : Size_3D) return Positive;

   -----------------------------------------------------------------------------

   type Texture_Description (Compressed : Boolean := False) is record
      Kind    : LE.Texture_Kind;
      Size    : Size_3D  := [others => 1];
      Levels  : Positive := 1;
      Samples : Natural  := 0;
      case Compressed is
         when True =>
            Compressed_Format : GL.Pixels.Compressed_Format;
         when False =>
            Format  : GL.Pixels.Internal_Format;
      end case;
   end record
     with Dynamic_Predicate =>
       (if Texture_Description.Kind = Texture_Cube_Map_Array then Texture_Description.Size (Z) mod 6 = 0);

   type Texture (Kind : LE.Texture_Kind) is tagged private;

   function Create_Texture (Description : Texture_Description) return Texture;

   function Create_View (Object : Texture; Layer : Natural) return Texture
     with Pre  => Has_Layers (Object.Kind) and then Layer < Layers (Object.Kind, Object.Description.Size),
          Post => Create_View'Result.Kind = Layer_Kind (Object.Kind);

   function Description (Object : Texture) return Texture_Description;

   function Size
     (Object : Texture;
      Level  : Mipmap_Level := 0) return Size_3D;

   procedure Bind (Object : Texture; Index : Natural);
   --  Bind the texture to the binding point at the given index

   procedure Bind_As_Image (Object : Texture; Index : Natural);
   --  Bind the texture as an image to the binding point at the given index

   function Image
     (Object : Texture;
      Level  : Mipmap_Level := 0) return String;
   --  Return a description of the texture

   -----------------------------------------------------------------------------

   function GL_Texture (Object : Texture) return GL.Objects.Textures.Texture;

private

   type Texture (Kind : LE.Texture_Kind) is tagged record
      Texture     : GL.Objects.Textures.Texture (Kind);
      Description : Texture_Description;
   end record;

   function Description (Object : Texture) return Texture_Description is (Object.Description);

   function GL_Texture (Object : Texture) return GL.Objects.Textures.Texture is (Object.Texture);

end Orka.Rendering.Textures;
