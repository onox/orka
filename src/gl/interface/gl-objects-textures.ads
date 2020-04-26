--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2012 Felix Krause <contact@flyx.org>
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

with System;

with Interfaces.C.Pointers;

with Ada.Unchecked_Deallocation;

with GL.Low_Level.Enums;
with GL.Objects.Buffers;
with GL.Objects.Shaders;
with GL.Pixels.Extensions;
with GL.Types.Pointers;

package GL.Objects.Textures is
   pragma Preelaborate;

   package LE renames Low_Level.Enums;
   package PE renames Pixels.Extensions;

   use all type LE.Texture_Kind;

   type Dimension_Count is (One, Two, Three);

   function Get_Dimensions (Kind : LE.Texture_Kind) return Dimension_Count;

   function Maximum_Anisotropy return Single
     with Post => Maximum_Anisotropy'Result >= 16.0;

   -----------------------------------------------------------------------------
   --                            Basic Types                                  --
   -----------------------------------------------------------------------------

   type Minifying_Function is (Nearest, Linear, Nearest_Mipmap_Nearest,
                               Linear_Mipmap_Nearest, Nearest_Mipmap_Linear,
                               Linear_Mipmap_Linear);

   --  Has to be defined here because of following subtype declaration.
   for Minifying_Function use (Nearest                => 16#2600#,
                               Linear                 => 16#2601#,
                               Nearest_Mipmap_Nearest => 16#2700#,
                               Linear_Mipmap_Nearest  => 16#2701#,
                               Nearest_Mipmap_Linear  => 16#2702#,
                               Linear_Mipmap_Linear   => 16#2703#);
   for Minifying_Function'Size use Int'Size;

   subtype Magnifying_Function is Minifying_Function range Nearest .. Linear;

   type Wrapping_Mode is (Repeat, Clamp_To_Border, Clamp_To_Edge,
                          Mirrored_Repeat, Mirror_Clamp_To_Edge);

   --  Actual range is implementation-defined
   --
   --  - OpenGL 2.x: At least 2
   --  - OpenGL 3.x: At least 48
   --  - OpenGL 4.x: At least 80
   subtype Texture_Unit is UInt;

   subtype Image_Unit is UInt;

   subtype Mipmap_Level is Size;

   -----------------------------------------------------------------------------
   --                          Texture Objects                                --
   -----------------------------------------------------------------------------

   type Texture_Base (Kind : LE.Texture_Kind)
     is abstract new GL_Object with private;

   function Has_Levels (Object : Texture_Base) return Boolean is
     (Object.Kind not in Texture_Buffer | Texture_Rectangle |
        Texture_2D_Multisample | Texture_2D_Multisample_Array)
   with Inline;

   function Layered (Object : Texture_Base) return Boolean is
     (Object.Kind in Texture_1D_Array | Texture_2D_Array | Texture_3D |
        Texture_Cube_Map | Texture_Cube_Map_Array | Texture_2D_Multisample_Array)
   with Inline;

   overriding
   procedure Initialize_Id (Object : in out Texture_Base);

   overriding
   procedure Delete_Id (Object : in out Texture_Base);

   overriding
   function Identifier (Object : Texture_Base) return Types.Debug.Identifier is
     (Types.Debug.Texture);

   procedure Invalidate_Image (Object : Texture_Base; Level : Mipmap_Level)
     with Pre  => (if not Object.Has_Levels then Level = 0);

   procedure Invalidate_Sub_Image (Object : Texture_Base; Level : Mipmap_Level;
                                   X, Y, Z : Int; Width, Height, Depth : Size)
   with Pre  => (if not Object.Has_Levels then Level = 0);

   procedure Bind_Texture_Unit (Object : Texture_Base; Unit : Texture_Unit);

   procedure Bind_Image_Texture (Object : Texture_Base; Unit : Image_Unit);

   -----------------------------------------------------------------------------

   type Texture is new Texture_Base with private;

   function Dimensions (Object : Texture) return Dimension_Count;

   function Allocated (Object : Texture) return Boolean;

   procedure Clear_Using_Data
     (Object : Texture; Level : Mipmap_Level;
      Source_Format : Pixels.Format;
      Source_Type   : Pixels.Data_Type;
      Source        : System.Address)
   with Pre => not Object.Compressed;

   procedure Clear_Using_Zeros
     (Object : Texture; Level : Mipmap_Level)
   with Pre => not Object.Compressed;

   procedure Generate_Mipmap (Object : Texture)
     with Pre => Object.Has_Levels;

   -----------------------------------------------------------------------------

   generic
      Kind : LE.Texture_Kind;
   package Texture_Bindings is
      type Texture_Array is array (Texture_Unit range <>) of Texture (Kind);

      type Image_Array is array (Image_Unit range <>) of Texture (Kind);

      procedure Bind_Textures (Textures : Texture_Array);

      procedure Bind_Images (Images : Image_Array);
   end Texture_Bindings;

   -----------------------------------------------------------------------------
   --                            Texture Parameters                           --
   -----------------------------------------------------------------------------

   procedure Set_Lowest_Mipmap_Level  (Object : Texture; Level : Mipmap_Level);
   procedure Set_Highest_Mipmap_Level (Object : Texture; Level : Mipmap_Level);

   function Lowest_Mipmap_Level  (Object : Texture) return Mipmap_Level;
   function Highest_Mipmap_Level (Object : Texture) return Mipmap_Level;

   function Mipmap_Levels (Object : Texture) return Mipmap_Level
     with Pre  => Object.Allocated,
          Post => Mipmap_Levels'Result >= 1;

   -----------------------------------------------------------------------------

   function Internal_Format (Object : Texture) return Pixels.Internal_Format
     with Pre => Object.Allocated and not Object.Compressed;

   function Compressed_Format (Object : Texture) return Pixels.Compressed_Format
     with Pre => Object.Allocated and Object.Compressed;

   function Compressed (Object : Texture) return Boolean;

   function Samples (Object : Texture) return Size;

   function Fixed_Sample_Locations (Object : Texture) return Boolean
     with Pre => Object.Kind in Texture_2D_Multisample | Texture_2D_Multisample_Array;

   function Red_Type   (Object : Texture) return Pixels.Channel_Data_Type;
   function Green_Type (Object : Texture) return Pixels.Channel_Data_Type;
   function Blue_Type  (Object : Texture) return Pixels.Channel_Data_Type;
   function Alpha_Type (Object : Texture) return Pixels.Channel_Data_Type;
   function Depth_Type (Object : Texture) return Pixels.Channel_Data_Type;

   function Red_Size   (Object : Texture) return Size;
   function Green_Size (Object : Texture) return Size;
   function Blue_Size  (Object : Texture) return Size;
   function Alpha_Size (Object : Texture) return Size;
   function Depth_Size (Object : Texture) return Size;
   function Stencil_Size (Object : Texture) return Size;
   function Shared_Size  (Object : Texture) return Size;

   -----------------------------------------------------------------------------
   --                         Texture Level Parameters                        --
   -----------------------------------------------------------------------------

   function Width  (Object : Texture; Level : Mipmap_Level) return Size;
   function Height (Object : Texture; Level : Mipmap_Level) return Size;
   function Depth  (Object : Texture; Level : Mipmap_Level) return Size;

   function Compressed_Image_Size (Object : Texture; Level : Mipmap_Level) return Size
     with Pre => Object.Compressed;

   -----------------------------------------------------------------------------
   --                            Texture Units                                --
   -----------------------------------------------------------------------------

   function Texture_Unit_Count return Natural;
   --  Return the maximum combined number of texture image units available
   --  to all shaders
   --
   --  If a texture image unit is used by multiple shaders, each shader stage
   --  is counted separately.

   function Texture_Unit_Count (Shader : Shaders.Shader_Type) return Natural;
   --  Return the maximum number of texture image units available for
   --  the specified shader

   -----------------------------------------------------------------------------
   --                        Buffer Texture Loading                           --
   -----------------------------------------------------------------------------

   type Buffer_Texture is new Texture_Base (Kind => Texture_Buffer) with private;

   procedure Attach_Buffer (Object : Buffer_Texture;
                            Internal_Format : Pixels.Internal_Format_Buffer_Texture;
                            Buffer : Objects.Buffers.Buffer);

   procedure Attach_Buffer (Object : Buffer_Texture;
                            Internal_Format : Pixels.Internal_Format_Buffer_Texture;
                            Buffer : Objects.Buffers.Buffer;
                            Offset, Size : Types.Size);

   function Buffer_Offset (Object : Buffer_Texture) return Size;
   function Buffer_Size   (Object : Buffer_Texture) return Size;

   -----------------------------------------------------------------------------
   --                           Texture Loading                               --
   -----------------------------------------------------------------------------

   procedure Allocate_Storage
     (Object : in out Texture;
      Levels, Samples : Types.Size;
      Format : Pixels.Internal_Format;
      Width, Height, Depth : Types.Size;
      Fixed_Locations : Boolean := True)
   with Pre  => not Object.Allocated,
        Post => Object.Allocated;

   procedure Allocate_Storage
     (Object : in out Texture;
      Levels, Samples : Types.Size;
      Format : Pixels.Compressed_Format;
      Width, Height, Depth : Types.Size;
      Fixed_Locations : Boolean := True)
   with Pre  => not Object.Allocated and Object.Kind /= Texture_Rectangle,
        Post => Object.Allocated;

   procedure Allocate_Storage
     (Object  : in out Texture;
      Subject : Texture;
      Fixed_Locations : Boolean := True)
   with Pre  => not Object.Allocated and Subject.Allocated,
        Post => Object.Allocated;
   --  Allocate storage using the same format, mipmap levels, samples, and
   --  dimensions of the given texture

   procedure Load_From_Data
     (Object : Texture;
      Level  : Mipmap_Level;
      X, Y, Z              : Types.Size := 0;
      Width, Height, Depth : Types.Positive_Size;
      Source_Format : Pixels.Format;
      Source_Type   : Pixels.Data_Type;
      Source        : System.Address)
   with Pre => Object.Allocated and not Object.Compressed;

   procedure Load_From_Data
     (Object : Texture;
      Level  : Mipmap_Level;
      X, Y, Z              : Types.Size := 0;
      Width, Height, Depth : Types.Positive_Size;
      Source_Format : Pixels.Compressed_Format;
      Image_Size    : Types.Size;
      Source        : System.Address)
   with Pre => Object.Dimensions /= One and Object.Allocated and Object.Compressed;

   procedure Copy_Data
     (Object  : Texture;
      Subject : Texture;
      Source_Level, Target_Level : Mipmap_Level)
   with Pre => Object.Allocated and Subject.Allocated;

   procedure Copy_Sub_Data
     (Object  : Texture;
      Subject : Texture;
      Source_Level, Target_Level : Mipmap_Level;
      Source_X, Source_Y, Source_Z : Types.Size := 0;
      Target_X, Target_Y, Target_Z : Types.Size := 0;
      Width, Height, Depth : Types.Size)
   with Pre => Object.Allocated and Subject.Allocated;

   procedure Clear_Using_Data
     (Object : Texture;
      Level  : Mipmap_Level;
      X, Y, Z              : Types.Size := 0;
      Width, Height, Depth : Types.Positive_Size;
      Source_Format : Pixels.Format;
      Source_Type   : Pixels.Data_Type;
      Source        : System.Address)
   with Pre => not Object.Compressed;

   procedure Clear_Using_Zeros
     (Object : Texture;
      Level  : Mipmap_Level;
      X, Y, Z              : Types.Size := 0;
      Width, Height, Depth : Types.Positive_Size)
   with Pre => not Object.Compressed;

   -----------------------------------------------------------------------------

   function Get_Compressed_Data
     (Object : Texture;
      Level  : Mipmap_Level;
      X, Y, Z              : Types.Size := 0;
      Width, Height, Depth : Types.Positive_Size;
      Format : Pixels.Compressed_Format) return not null Types.Pointers.UByte_Array_Access
   with Pre => Object.Dimensions /= One and Object.Allocated and Object.Compressed
     and Object.Kind not in Texture_2D_Multisample | Texture_2D_Multisample_Array;

   generic
      with package Pointers is new Interfaces.C.Pointers (<>);
   package Texture_Pointers is

      type Element_Array_Access is access Pointers.Element_Array;

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Pointers.Element_Array, Name => Element_Array_Access);

      function Get_Data
        (Object : Texture;
         Level  : Mipmap_Level;
         X, Y, Z              : Types.Size := 0;
         Width, Height, Depth : Types.Positive_Size;
         Format    : Pixels.Format;
         Data_Type : PE.Non_Packed_Data_Type) return not null Element_Array_Access
      with Pre => Object.Allocated and
                    not Object.Compressed and PE.Compatible (Format, Data_Type);

   end Texture_Pointers;

   -----------------------------------------------------------------------------
   --                              Texture Views                              --
   -----------------------------------------------------------------------------

   function Create_View
     (Object    : Texture;
      Kind      : LE.Texture_Kind;
      Format    : Pixels.Internal_Format;
      Min_Level, Levels : Mipmap_Level;
      Min_Layer, Layers : Size) return Texture
   with Pre => Object.Allocated;
   --  Create a Texture object that shares some of the original texture's data
   --
   --  The format and kind must be compatible with the original texture. See
   --  the OpenGL documentation.

   function Create_View
     (Object    : Texture;
      Kind      : LE.Texture_Kind;
      Format    : Pixels.Compressed_Format;
      Min_Level, Levels : Mipmap_Level;
      Min_Layer, Layers : Size) return Texture
   with Pre => Object.Allocated;
   --  Create a Texture object that shares some of the original texture's data
   --
   --  The format and kind must be compatible with the original texture. See
   --  the OpenGL documentation.

   function Create_View
     (Object : Texture;
      Kind   : LE.Texture_Kind;
      Layer  : Size) return Texture
   with Pre => Object.Allocated and Object.Layered;
   --  Create a Texture object that shares one layer or six layer-faces
   --  of the original texture's data

private

   for Wrapping_Mode use
     (Repeat               => 16#2901#,
      Clamp_To_Border      => 16#812D#,
      Clamp_To_Edge        => 16#812F#,
      Mirrored_Repeat      => 16#8370#,
      Mirror_Clamp_To_Edge => 16#8743#);
   for Wrapping_Mode'Size use Int'Size;

   type Texture_Base (Kind : LE.Texture_Kind)
     is new GL_Object with null record;

   type Texture is new Texture_Base with record
      Allocated  : Boolean := False;
      Dimensions : Dimension_Count := Get_Dimensions (Texture.Kind);
   end record;

   type Buffer_Texture is new Texture_Base (Kind => Texture_Buffer) with null record;

end GL.Objects.Textures;
