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

with GL.Low_Level.Enums;

package GL.Pixels.Queries is
   pragma Preelaborate;

   package LE renames GL.Low_Level.Enums;
   use all type LE.Texture_Kind;

   type Support is (None, Full_Support, Caveat_Support);

   type Encoding is (Linear, sRGB);

   type Image_Format_Compatibility_Type is
     (Image_Format_Compatibility_By_Size,
      Image_Format_Compatibility_By_Class);

   type Image_Class is
     (Image_Class_4_X_32,
      Image_Class_2_X_32,
      Image_Class_1_X_32,
      Image_Class_4_X_16,
      Image_Class_2_X_16,
      Image_Class_1_X_16,
      Image_Class_4_X_8,
      Image_Class_2_X_8,
      Image_Class_1_X_8,
      Image_Class_11_11_10,
      Image_Class_10_10_10_2);

   type View_Class is
     (View_Class_128_Bits,
      View_Class_96_Bits,
      View_Class_64_Bits,
      View_Class_48_Bits,
      View_Class_32_Bits,
      View_Class_24_Bits,
      View_Class_16_Bits,
      View_Class_8_Bits,
      View_Class_S3tc_Dxt1_Rgb,
      View_Class_S3tc_Dxt1_Rgba,
      View_Class_S3tc_Dxt3_Rgba,
      View_Class_S3tc_Dxt5_Rgba,
      View_Class_Rgtc1_Red,
      View_Class_Rgtc2_Rg,
      View_Class_Bptc_Unorm,
      View_Class_Bptc_Float);

   function Sample_Counts
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Size_Array
   with Pre  => Kind in Texture_2D_Multisample | Texture_2D_Multisample_Array,
        Post => Sample_Counts'Result'Length > 0
                  and then (for all Samples of Sample_Counts'Result => Samples > 0);
   --  Return the sample counts available for the internal format of the texture

   function Supported
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Boolean;

   function Preferred
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Internal_Format;

   --  Note: the size and type of components (R, G, B, A, depth, and stencil)
   --  can be retrieved via the function *_Type and *_Size in GL.Objects.Textures

   -----------------------------------------------------------------------------

   function Max_Width
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Size;

   function Max_Height
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Size
   with Pre => Kind /= Texture_1D;

   function Max_Depth
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Size
   with Pre => Kind in Texture_3D | Texture_2D_Array | Texture_Cube_Map_Array |
                       Texture_2D_Multisample_Array;

   function Max_Layers
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Size
   with Pre => Kind in Texture_1D_Array | Texture_2D_Array | Texture_Cube_Map_Array |
                       Texture_2D_Multisample_Array;

   function Max_Combined_Dimensions
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Long_Size;

   -----------------------------------------------------------------------------

   function Color_Components
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Boolean;
   --  Return True iff Format contains any color component (R, G, B, or A)

   function Depth_Components
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Boolean;
   --  Return True iff Format contains a depth component

   function Stencil_Components
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Boolean;
   --  Return True iff Format contains a stencil component

   function Color_Renderable
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Boolean;
   --  Return True iff Format is color-renderable according to Table 8.12
   --  of the OpenGL specification

   function Depth_Renderable
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Boolean;
   --  Return True iff Format is depth-renderable according to Table 8.13
   --  of the OpenGL specification

   function Stencil_Renderable
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Boolean;
   --  Return True iff Format is stencil-renderable according to Table 8.13
   --  of the OpenGL specification

   -----------------------------------------------------------------------------

   function Framebuffer_Renderable
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;
   --  Return the support for rendering to a resource with the given
   --  internal format and texture kind when attached to a framebuffer

   function Framebuffer_Renderable_Layered
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;
   --  Return the support for layered rendering to a resource with the
   --  given internal format and texture kind when attached to a framebuffer

   function Framebuffer_Blend
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;
   --  Return the support for rendering to a resource with the given
   --  internal format and texture kind when attached to a framebuffer while
   --  blending is enabled

   -----------------------------------------------------------------------------

   function Texture_Format
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Pixels.Format;
   --  Return the format to use for uploading data to a texture for best
   --  performance and image quality
   --
   --  A Constraint_Error is raised if the internal format is not supported

   function Texture_Type
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Pixels.Data_Type;
   --  Return the data type to use for uploading data to a texture for
   --  best performance and image quality
   --
   --  A Constraint_Error is raised if the internal format is not supported

   function Get_Texture_Format
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Pixels.Format;
   --  Return the format to use for downloading data from a texture for
   --  best performance and image quality
   --
   --  A Constraint_Error is raised if the internal format is not supported

   function Get_Texture_Type
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Pixels.Data_Type;
   --  Return the data type to use for downloading data from a texture
   --  for best performance and image quality
   --
   --  A Constraint_Error is raised if the internal format is not supported

   -----------------------------------------------------------------------------

   function Mipmap
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Boolean;

   function Manual_Generate_Mipmap
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;

   function Auto_Generate_Mipmap
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;

   -----------------------------------------------------------------------------

   function Color_Encoding
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Encoding
   with Pre => Color_Components (Format, Kind);
   --  Return the color encoding
   --
   --  A Constraint_Error is raised if the format is not supported

   function sRGB_Read
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;
   --  Return support for converting from sRGB colorspace

   function sRGB_Write
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;
   --  Return support for converting to sRGB colorspace

   function sRGB_Decode_Sampling_Time
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;
   --  Return support for decoding during sampling time

   -----------------------------------------------------------------------------

   function Filter
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;
   --  Return support for filter types other than Nearest and
   --  Nearest_Mipmap_Nearest

   function Vertex_Texture
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;
   --  Return support for using the resource for texture sampling in
   --  a vertex shader

   function Tess_Control_Texture
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;
   --  Return support for using the resource for texture sampling in
   --  a tesselation control shader

   function Tess_Evaluation_Texture
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;
   --  Return support for using the resource for texture sampling in
   --  a tesselation evaluation shader

   function Geometry_Texture
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;
   --  Return support for using the resource for texture sampling in
   --  a geometry shader

   function Fragment_Texture
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;
   --  Return support for using the resource for texture sampling in
   --  a fragment shader

   function Compute_Texture
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;
   --  Return support for using the resource for texture sampling in
   --  a compute shader

   function Texture_Shadow
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;
   --  Return support for using the resource for shadow samplers

   function Texture_Gather
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;
   --  Return support for using the resource in texture gather operations

   function Texture_Gather_Shadow
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;
   --  Return support for using the resource in texture gather operations
   --  with shadow samplers

   -----------------------------------------------------------------------------

   function Shader_Image_Load
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;
   --  Return support for using the resource in image load operations

   function Shader_Image_Store
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;
   --  Return support for using the resource in image store operations

   function Shader_Image_Atomic
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;
   --  Return support for using the resource in atomic memory operations

   -----------------------------------------------------------------------------

   function Image_Texel_Size
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Size;
   --  Return the size of a texel when the resource is used as image texture

   function Image_Pixel_Format
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Pixels.Format;
   --  Return the format of the resource when used as image texture
   --
   --  A Constraint_Error is raised if the internal format is not supported

   function Image_Pixel_Type
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Pixels.Data_Type;
   --  Return the data type of the resource when used as image texture
   --
   --  A Constraint_Error is raised if the internal format is not supported

   function Image_Format_Compatibility
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Image_Format_Compatibility_Type;
   --  Return matching criteria of the resource when used as image texture
   --
   --  A Constraint_Error is raised if the internal format is not supported

   function Simultaneous_Texture_And_Depth_Test
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   with Pre => Depth_Components (Format, Kind);
   --  Return support for using the resource in texture sampling while
   --  bound as a buffer for depth testing

   function Simultaneous_Texture_And_Stencil_Test
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   with Pre => Stencil_Components (Format, Kind);
   --  Return support for using the resource in texture sampling while
   --  bound as a buffer for stencil testing

   function Simultaneous_Texture_And_Depth_Write
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   with Pre => Depth_Components (Format, Kind);
   --  Return support for using the resource in texture sampling while
   --  depth writes to the resource

   function Simultaneous_Texture_And_Stencil_Write
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   with Pre => Stencil_Components (Format, Kind);
   --  Return support for using the resource in texture sampling while
   --  stencil writes to the resource

   -----------------------------------------------------------------------------

   function Texture_Compressed_Block_Width
     (Format : Compressed_Format;
      Kind   : LE.Texture_Kind) return Size;
   --  Return width of a compressed block in bytes

   function Texture_Compressed_Block_Height
     (Format : Compressed_Format;
      Kind   : LE.Texture_Kind) return Size;
   --  Return height of a compressed block in bytes

   function Texture_Compressed_Block_Size
     (Format : Compressed_Format;
      Kind   : LE.Texture_Kind) return Size;
   --  Return number of bytes of a compressed block

   -----------------------------------------------------------------------------

   function Clear_Buffer
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;

   function Texture_View
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support;

   function Image_Compatibility_Class
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Image_Class;
   --  Return the compatibility class when the resource is used as image texture
   --
   --  A Constraint_Error is raised if the internal format is not supported

   function View_Compatibility_Class
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return View_Class;
   --  Return the compatibility class when the resource is used as texture view
   --
   --  A Constraint_Error is raised if the internal format is not supported

private

   for Support use
     (None           => 16#0000#,
      Full_Support   => 16#82B7#,
      Caveat_Support => 16#82B8#);
   for Support'Size use Low_Level.Enum'Size;

   for Encoding use
      (Linear => 16#2601#,
       sRGB   => 16#8C40#);
   for Encoding'Size use Low_Level.Enum'Size;

   for Image_Format_Compatibility_Type use
     (Image_Format_Compatibility_By_Size  => 16#90C8#,
      Image_Format_Compatibility_By_Class => 16#90C9#);
   for Image_Format_Compatibility_Type'Size use Low_Level.Enum'Size;

   for Image_Class use
     (Image_Class_4_X_32        => 16#82B9#,
      Image_Class_2_X_32        => 16#82BA#,
      Image_Class_1_X_32        => 16#82BB#,
      Image_Class_4_X_16        => 16#82BC#,
      Image_Class_2_X_16        => 16#82BD#,
      Image_Class_1_X_16        => 16#82BE#,
      Image_Class_4_X_8         => 16#82BF#,
      Image_Class_2_X_8         => 16#82C0#,
      Image_Class_1_X_8         => 16#82C1#,
      Image_Class_11_11_10      => 16#82C2#,
      Image_Class_10_10_10_2    => 16#82C3#);
   for Image_Class'Size use Low_Level.Enum'Size;

   for View_Class use
     (View_Class_128_Bits       => 16#82C4#,
      View_Class_96_Bits        => 16#82C5#,
      View_Class_64_Bits        => 16#82C6#,
      View_Class_48_Bits        => 16#82C7#,
      View_Class_32_Bits        => 16#82C8#,
      View_Class_24_Bits        => 16#82C9#,
      View_Class_16_Bits        => 16#82CA#,
      View_Class_8_Bits         => 16#82CB#,
      View_Class_S3tc_Dxt1_Rgb  => 16#82CC#,
      View_Class_S3tc_Dxt1_Rgba => 16#82CD#,
      View_Class_S3tc_Dxt3_Rgba => 16#82CE#,
      View_Class_S3tc_Dxt5_Rgba => 16#82CF#,
      View_Class_Rgtc1_Red      => 16#82D0#,
      View_Class_Rgtc2_Rg       => 16#82D1#,
      View_Class_Bptc_Unorm     => 16#82D2#,
      View_Class_Bptc_Float     => 16#82D3#);
   for View_Class'Size use Low_Level.Enum'Size;

end GL.Pixels.Queries;
