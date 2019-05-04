--  Copyright (c) 2016 onox <denkpadje@gmail.com>
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

with GL.Objects.Textures;
with GL.Types.Colors;

package GL.Objects.Samplers is
   pragma Preelaborate;

   type Sampler is new GL_Object with private;

   type Sampler_Array is array (Positive range <>) of Sampler;

   procedure Bind (Object : Sampler; Unit : Textures.Texture_Unit);

   procedure Bind (Objects : Sampler_Array; First_Unit : Textures.Texture_Unit);

   overriding
   procedure Initialize_Id (Object : in out Sampler);

   overriding
   procedure Delete_Id (Object : in out Sampler);

   overriding
   function Identifier (Object : Sampler) return Types.Debug.Identifier is
     (Types.Debug.Sampler);

   -----------------------------------------------------------------------------
   --                           Sampler Parameters                            --
   -----------------------------------------------------------------------------

   use GL.Objects.Textures;

   procedure Set_Minifying_Filter (Object : Sampler;
                                   Filter : Minifying_Function);

   function Minifying_Filter (Object : Sampler) return Minifying_Function;
   --  Return the minification function. By default this is
   --  Nearest_Mipmap_Linear.

   procedure Set_Magnifying_Filter (Object : Sampler;
                                    Filter : Magnifying_Function);

   function Magnifying_Filter (Object : Sampler) return Magnifying_Function;
   --  Return the magnification function. By default this is Linear.

   procedure Set_Minimum_LoD (Object : Sampler; Level : Double);

   function Minimum_LoD (Object : Sampler) return Double;
   --  Return the minimum LOD. By default this is -1000.

   procedure Set_Maximum_LoD (Object : Sampler; Level : Double);

   function Maximum_LoD (Object : Sampler) return Double;
   --  Return the maximum LOD. By default this is 1000.

   procedure Set_LoD_Bias (Object : Sampler; Level : Double);
   --  Adjust the selection of a mipmap image. A positive level will
   --  cause larger mipmaps to be selected. A too large bias can
   --  result in visual aliasing, but if the bias is small enough it
   --  can make the texture look a bit sharper.

   function LoD_Bias (Object : Sampler) return Double;
   --  Return the LOD bias for the selection of a mipmap.
   --  By default this is 0.0.

   procedure Set_Seamless_Filtering (Object : Sampler; Enable : Boolean);
   --  Enable seamless cubemap filtering
   --
   --  Note: this procedure requires the ARB_seamless_cubemap_per_texture
   --  extension. If this extension is not available, you can enable seamless
   --  filtering globally via GL.Toggles.

   function Seamless_Filtering (Object : Sampler) return Boolean;
   --  Return whether seamless filtering is enabled for cube map
   --  textures. By default this is False.

   procedure Set_Max_Anisotropy (Object : Sampler; Degree : Double)
     with Pre => Degree >= 1.0;
   --  Set the maximum amount of anisotropy filtering to reduce the blurring
   --  of textures (caused by mipmap filtering) that are viewed at an
   --  oblique angle.
   --
   --  For best results, combine the use of anisotropy filtering with
   --  a Linear_Mipmap_Linear minification filter and a Linear maxification
   --  filter.

   function Max_Anisotropy (Object : Sampler) return Double
     with Post => Max_Anisotropy'Result >= 1.0;

   procedure Set_X_Wrapping (Object : Sampler; Mode : Wrapping_Mode);

   function X_Wrapping (Object : Sampler) return Wrapping_Mode;
   --  Return the wrapping mode for the X direction. By default this
   --  is Repeat.

   procedure Set_Y_Wrapping (Object : Sampler; Mode : Wrapping_Mode);

   function Y_Wrapping (Object : Sampler) return Wrapping_Mode;
   --  Return the wrapping mode for the Y direction. By default this
   --  is Repeat.

   procedure Set_Z_Wrapping (Object : Sampler; Mode : Wrapping_Mode);

   function Z_Wrapping (Object : Sampler) return Wrapping_Mode;
   --  Return the wrapping mode for the Z direction. By default this
   --  is Repeat.

   procedure Set_Border_Color (Object : Sampler; Color : Colors.Border_Color);
   function Border_Color (Object : Sampler) return Colors.Border_Color;

   procedure Set_Compare_X_To_Texture (Object : Sampler; Enabled : Boolean);

   function Compare_X_To_Texture_Enabled (Object : Sampler) return Boolean;
   --  Return whether to enable comparing. By default this is false.

   procedure Set_Compare_Function (Object : Sampler;
                                   Func   : Compare_Function);

   function Current_Compare_Function (Object : Sampler) return Compare_Function;
   --  Return the comparison function. By default this is LEqual.

private

   type Sampler is new GL_Object with null record;

end GL.Objects.Samplers;
