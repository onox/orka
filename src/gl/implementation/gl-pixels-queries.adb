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

with GL.API;
with GL.Enums.Internalformat;

package body GL.Pixels.Queries is

   use all type Enums.Internalformat.Parameter;

   function Get_Support
     (Format    : Internal_Format;
      Kind      : LE.Texture_Kind;
      Parameter : Enums.Internalformat.Parameter) return Support
   is
      Result : Support := None;
   begin
      API.Get_Internal_Format (Kind, Format, Parameter, 1, Result);
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Get_Support;

   function Get_Size
     (Format    : Internal_Format;
      Kind      : LE.Texture_Kind;
      Parameter : Enums.Internalformat.Parameter) return Size
   is
      Result : Size := 0;
   begin
      API.Get_Internal_Format (Kind, Format, Parameter, 1, Result);
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Get_Size;

   function Get_Long_Size
     (Format    : Internal_Format;
      Kind      : LE.Texture_Kind;
      Parameter : Enums.Internalformat.Parameter) return Long_Size
   is
      Result : Long_Size := 0;
   begin
      API.Get_Internal_Format (Kind, Format, Parameter, 1, Result);
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Get_Long_Size;

   function Get_Boolean
     (Format    : Internal_Format;
      Kind      : LE.Texture_Kind;
      Parameter : Enums.Internalformat.Parameter) return Boolean
   is (Get_Size (Format, Kind, Parameter) = 1);

   function Get_Size
     (Format    : Compressed_Format;
      Kind      : LE.Texture_Kind;
      Parameter : Enums.Internalformat.Parameter) return Size
   is
      Result : Size := 0;
   begin
      API.Get_Internal_Format (Kind, Format, Parameter, 1, Result);
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Get_Size;

   -----------------------------------------------------------------------------

   function Sample_Counts
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Size_Array
   is
      Count : Size := 0;
   begin
      API.Get_Internal_Format (Kind, Format, Num_Sample_Counts, 1, Count);
      Raise_Exception_On_OpenGL_Error;

      declare
         Result : Size_Array (1 .. Count) := (others => 0);
      begin
         API.Get_Internal_Format (Kind, Format, Samples, Result'Length, Result);
         Raise_Exception_On_OpenGL_Error;
         return Result;
      end;
   end Sample_Counts;

   function Supported
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Boolean
   is (Get_Boolean (Format, Kind, Internalformat_Supported));

   function Preferred
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Boolean
   is (Get_Boolean (Format, Kind, Internalformat_Preferred));

   -----------------------------------------------------------------------------

   function Max_Width
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Size
   is (Get_Size (Format, Kind, Max_Width));

   function Max_Height
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Size
   is (Get_Size (Format, Kind, Max_Height));

   function Max_Depth
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Size
   is (Get_Size (Format, Kind, Max_Depth));

   function Max_Layers
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Size
   is (Get_Size (Format, Kind, Max_Layers));

   function Max_Combined_Dimensions
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Long_Size
   is (Get_Long_Size (Format, Kind, Max_Combined_Dimensions));

   -----------------------------------------------------------------------------

   function Color_Components
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Boolean
   is (Get_Boolean (Format, Kind, Color_Components));

   function Depth_Components
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Boolean
   is (Get_Boolean (Format, Kind, Depth_Components));

   function Stencil_Components
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Boolean
   is (Get_Boolean (Format, Kind, Stencil_Components));

   function Color_Renderable
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Boolean
   is (Get_Boolean (Format, Kind, Color_Renderable));

   function Depth_Renderable
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Boolean
   is (Get_Boolean (Format, Kind, Depth_Renderable));

   function Stencil_Renderable
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Boolean
   is (Get_Boolean (Format, Kind, Stencil_Renderable));

   -----------------------------------------------------------------------------

   function Framebuffer_Renderable
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Framebuffer_Renderable));

   function Framebuffer_Renderable_Layered
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Framebuffer_Renderable_Layered));

   function Framebuffer_Blend
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Framebuffer_Blend));

   function Read_Pixels
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Read_Pixels));

   -----------------------------------------------------------------------------

   function Read_Pixels_Format
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Pixels.Format
   is
      Result : Size := 0;

      function Convert is new Ada.Unchecked_Conversion
        (Source => GL.Types.Int, Target => Pixels.Format);
   begin
      API.Get_Internal_Format (Kind, Format, Read_Pixels_Format, 1, Result);
      Raise_Exception_On_OpenGL_Error;
      if Result = 0 then
         raise Constraint_Error with "Invalid internal format for Read_Pixels";
      end if;
      return Convert (GL.Types.Int (Result));
   end Read_Pixels_Format;

   function Read_Pixels_Type
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Pixels.Data_Type
   is
      Result : Size := 0;

      function Convert is new Ada.Unchecked_Conversion
        (Source => GL.Types.Int, Target => Pixels.Data_Type);
   begin
      API.Get_Internal_Format (Kind, Format, Read_Pixels_Type, 1, Result);
      Raise_Exception_On_OpenGL_Error;
      if Result = 0 then
         raise Constraint_Error with "Invalid internal format for Read_Pixels";
      end if;
      return Convert (GL.Types.Int (Result));
   end Read_Pixels_Type;

   function Texture_Format
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Pixels.Format
   is
      Result : Size := 0;

      function Convert is new Ada.Unchecked_Conversion
        (Source => GL.Types.Int, Target => Pixels.Format);
   begin
      API.Get_Internal_Format (Kind, Format, Texture_Image_Format, 1, Result);
      Raise_Exception_On_OpenGL_Error;
      if Result = 0 then
         raise Constraint_Error with "Invalid internal format for uploading to texture";
      end if;
      return Convert (GL.Types.Int (Result));
   end Texture_Format;

   function Texture_Type
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Pixels.Data_Type
   is
      Result : Size := 0;

      function Convert is new Ada.Unchecked_Conversion
        (Source => GL.Types.Int, Target => Pixels.Data_Type);
   begin
      API.Get_Internal_Format (Kind, Format, Texture_Image_Type, 1, Result);
      Raise_Exception_On_OpenGL_Error;
      if Result = 0 then
         raise Constraint_Error with "Invalid internal format for uploading to texture";
      end if;
      return Convert (GL.Types.Int (Result));
   end Texture_Type;

   function Get_Texture_Format
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Pixels.Format
   is
      Result : Size := 0;

      function Convert is new Ada.Unchecked_Conversion
        (Source => GL.Types.Int, Target => Pixels.Format);
   begin
      API.Get_Internal_Format (Kind, Format, Get_Texture_Image_Format, 1, Result);
      Raise_Exception_On_OpenGL_Error;
      if Result = 0 then
         raise Constraint_Error with "Invalid internal format for downloading to texture";
      end if;
      return Convert (GL.Types.Int (Result));
   end Get_Texture_Format;

   function Get_Texture_Type
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Pixels.Data_Type
   is
      Result : Size := 0;

      function Convert is new Ada.Unchecked_Conversion
        (Source => GL.Types.Int, Target => Pixels.Data_Type);
   begin
      API.Get_Internal_Format (Kind, Format, Get_Texture_Image_Type, 1, Result);
      Raise_Exception_On_OpenGL_Error;
      if Result = 0 then
         raise Constraint_Error with "Invalid internal format for downloading to texture";
      end if;
      return Convert (GL.Types.Int (Result));
   end Get_Texture_Type;

   -----------------------------------------------------------------------------

   function Mipmap
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Boolean
   is (Get_Boolean (Format, Kind, Mipmap));

   function Manual_Generate_Mipmap
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Manual_Generate_Mipmap));

   function Auto_Generate_Mipmap
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Auto_Generate_Mipmap));

   -----------------------------------------------------------------------------

   function Color_Encoding
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Encoding
   is
      Result : Size := 0;

      function Convert is new Ada.Unchecked_Conversion
        (Source => GL.Types.Int, Target => Encoding);
   begin
      API.Get_Internal_Format (Kind, Format, Color_Encoding, 1, Result);
      Raise_Exception_On_OpenGL_Error;
      if Result = 0 then
         raise Constraint_Error with "Invalid internal format for color encoding";
      end if;
      return Convert (GL.Types.Int (Result));
   end Color_Encoding;

   function sRGB_Read
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, sRGB_Read));

   function sRGB_Write
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, sRGB_Write));

   function sRGB_Decode_Sampling_Time
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, sRGB_Decode_ARB));

   -----------------------------------------------------------------------------

   function Filter
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Filter));

   function Vertex_Texture
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Vertex_Texture));

   function Tess_Control_Texture
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Tess_Control_Texture));

   function Tess_Evaluation_Texture
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Tess_Evaluation_Texture));

   function Geometry_Texture
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Geometry_Texture));

   function Fragment_Texture
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Fragment_Texture));

   function Compute_Texture
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Compute_Texture));

   function Texture_Shadow
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Texture_Shadow));

   function Texture_Gather
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Texture_Gather));

   function Texture_Gather_Shadow
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Texture_Gather_Shadow));

   -----------------------------------------------------------------------------

   function Shader_Image_Load
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Shader_Image_Load));

   function Shader_Image_Store
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Shader_Image_Store));

   function Shader_Image_Atomic
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Shader_Image_Atomic));

   -----------------------------------------------------------------------------

   function Image_Texel_Size
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Size
   is (Get_Size (Format, Kind, Image_Texel_Size));

   function Image_Pixel_Format
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Pixels.Format
   is
      Result : Size := 0;

      function Convert is new Ada.Unchecked_Conversion
        (Source => GL.Types.Int, Target => Pixels.Format);
   begin
      API.Get_Internal_Format (Kind, Format, Image_Pixel_Format, 1, Result);
      Raise_Exception_On_OpenGL_Error;
      if Result = 0 then
         raise Constraint_Error with "Invalid internal format for image texture";
      end if;
      return Convert (GL.Types.Int (Result));
   end Image_Pixel_Format;

   function Image_Pixel_Type
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Pixels.Data_Type
   is
      Result : Size := 0;

      function Convert is new Ada.Unchecked_Conversion
        (Source => GL.Types.Int, Target => Pixels.Data_Type);
   begin
      API.Get_Internal_Format (Kind, Format, Image_Pixel_Type, 1, Result);
      Raise_Exception_On_OpenGL_Error;
      if Result = 0 then
         raise Constraint_Error with "Invalid internal format for image texture";
      end if;
      return Convert (GL.Types.Int (Result));
   end Image_Pixel_Type;

   function Image_Format_Compatibility
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Image_Format_Compatibility_Type
   is
      Result : Size := 0;

      function Convert is new Ada.Unchecked_Conversion
        (Source => GL.Types.Int, Target => Image_Format_Compatibility_Type);
   begin
      API.Get_Internal_Format (Kind, Format,
        Enums.Internalformat.Image_Format_Compatibility_Type, 1, Result);
      Raise_Exception_On_OpenGL_Error;
      if Result = 0 then
         raise Constraint_Error with "Invalid internal format for image texture";
      end if;
      return Convert (GL.Types.Int (Result));
   end Image_Format_Compatibility;

   function Simultaneous_Texture_And_Depth_Test
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Simultaneous_Texture_And_Depth_Test));

   function Simultaneous_Texture_And_Stencil_Test
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Simultaneous_Texture_And_Stencil_Test));

   function Simultaneous_Texture_And_Depth_Write
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Simultaneous_Texture_And_Depth_Write));

   function Simultaneous_Texture_And_Stencil_Write
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Simultaneous_Texture_And_Stencil_Write));

   -----------------------------------------------------------------------------

   function Texture_Compressed_Block_Width
     (Format : Compressed_Format;
      Kind   : LE.Texture_Kind) return Size
   is (Get_Size (Format, Kind, Texture_Compressed_Block_Width));

   function Texture_Compressed_Block_Height
     (Format : Compressed_Format;
      Kind   : LE.Texture_Kind) return Size
   is (Get_Size (Format, Kind, Texture_Compressed_Block_Height));

   function Texture_Compressed_Block_Size
     (Format : Compressed_Format;
      Kind   : LE.Texture_Kind) return Size
   is (Get_Size (Format, Kind, Texture_Compressed_Block_Size));

   -----------------------------------------------------------------------------

   function Clear_Buffer
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Clear_Buffer));

   function Texture_View
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Support
   is (Get_Support (Format, Kind, Texture_View));

   function Image_Compatibility_Class
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return Image_Class
   is
      Result : Size := 0;

      function Convert is new Ada.Unchecked_Conversion
        (Source => GL.Types.Int, Target => Image_Class);
   begin
      API.Get_Internal_Format (Kind, Format,
        Enums.Internalformat.Image_Compatibility_Class, 1, Result);
      Raise_Exception_On_OpenGL_Error;
      if Result = 0 then
         raise Constraint_Error with "Invalid internal format for image texture";
      end if;
      return Convert (GL.Types.Int (Result));
   end Image_Compatibility_Class;

   function View_Compatibility_Class
     (Format : Internal_Format;
      Kind   : LE.Texture_Kind) return View_Class
   is
      Result : Size := 0;

      function Convert is new Ada.Unchecked_Conversion
        (Source => GL.Types.Int, Target => View_Class);
   begin
      API.Get_Internal_Format (Kind, Format,
        Enums.Internalformat.View_Compatibility_Class, 1, Result);
      Raise_Exception_On_OpenGL_Error;
      if Result = 0 then
         raise Constraint_Error with "Invalid internal format for texture view";
      end if;
      return Convert (GL.Types.Int (Result));
   end View_Compatibility_Class;

end GL.Pixels.Queries;
