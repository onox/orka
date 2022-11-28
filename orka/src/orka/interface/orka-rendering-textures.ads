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
with GL.Objects.Samplers;
with GL.Objects.Textures;
with GL.Pixels;

package Orka.Rendering.Textures is
   pragma Preelaborate;

   package LE renames GL.Low_Level.Enums;

   use all type LE.Texture_Kind;
   use all type GL.Objects.Textures.Dimension_Count;

   type Indexed_Texture_Target is (Texture, Image);

   procedure Bind
     (Object : GL.Objects.Textures.Texture_Base'Class;
      Target : Indexed_Texture_Target;
      Index  : Natural);
   --  Bind the texture or image to the binding point at the given index

   function Bayer_Dithering_Pattern return GL.Objects.Samplers.Sampler;
   --  Return a sampler for sampling a texture containing the Bayer ordered
   --  dithering pattern

   function Bayer_Dithering_Pattern return GL.Objects.Textures.Texture
     with Post => Bayer_Dithering_Pattern'Result.Dimensions = Two;
   --  Return a texture that contains the Bayer ordered dithering pattern
   --
   --  The texture can be used in a fragment shader as follows:
   --
   --  uniform sampler2D dither;
   --  color.xyz += vec3(texture(dither, gl_FragCoord.xy / 8.0).r / 64.0 - (1.0 / 128.0));

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

end Orka.Rendering.Textures;
