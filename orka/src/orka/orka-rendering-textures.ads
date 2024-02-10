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

   type Format_Kind is (Depth_Stencil, Depth, Stencil, Color);

   function Get_Format_Kind
     (Format : GL.Pixels.Internal_Format) return Format_Kind;

   function Image
     (Texture : GL.Objects.Textures.Texture;
      Level   : GL.Objects.Textures.Mipmap_Level := 0) return String;
   --  Return a description of the texture

   function Has_Layers (Kind : LE.Texture_Kind) return Boolean is
     (Kind in Texture_1D_Array | Texture_2D_Array |
              Texture_2D_Multisample_Array | Texture_Cube_Map_Array);

   -----------------------------------------------------------------------------

   type Texture_Description is record
      Kind    : LE.Texture_Kind;
      Format  : GL.Pixels.Internal_Format;
      Size    : Size_3D  := (others => 1);
      Levels  : Positive := 1;
      Layers  : Positive := 1;
      Samples : Natural  := 0;
   end record
     with Dynamic_Predicate => (if not Has_Layers (Texture_Description.Kind) then Texture_Description.Layers = 1);

   type Texture (Kind : LE.Texture_Kind) is tagged private;

   function Create_Texture (Description : Texture_Description) return Texture;

   function Description (Object : Texture) return Texture_Description;

   procedure Bind (Object : Texture; Index : Natural);
   --  Bind the texture to the binding point at the given index

   procedure Bind_As_Image (Object : Texture; Index : Natural);
   --  Bind the texture as an image to the binding point at the given index

   function GL_Texture (Object : Texture) return GL.Objects.Textures.Texture;

   function From_GL_Texture (Object : GL.Objects.Textures.Texture) return Texture;

private

   type Texture (Kind : LE.Texture_Kind) is tagged record
      Texture : GL.Objects.Textures.Texture (Kind);
   end record;

   function GL_Texture (Object : Texture) return GL.Objects.Textures.Texture is (Object.Texture);

   function From_GL_Texture (Object : GL.Objects.Textures.Texture) return Texture is
     (Kind => Object.Kind, Texture => Object);

end Orka.Rendering.Textures;
