--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <contact@flyx.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with System;

with GL.Low_Level.Enums;
with GL.Objects.Buffers;
with GL.Pixels;
with GL.Types.Colors;

package GL.Objects.Textures is
   pragma Preelaborate;

   function Maximum_Anisotropy return Single
     with Post => Maximum_Anisotropy'Result >= 2.0;

   -----------------------------------------------------------------------------
   --                            Basic Types                                  --
   -----------------------------------------------------------------------------

   type Minifying_Function is (Nearest, Linear, Nearest_Mipmap_Nearest,
                               Linear_Mipmap_Nearest, Nearest_Mipmap_Linear,
                               Linear_Mipmap_Linear);

   -- has to be defined here because of following subtype declaration.
   for Minifying_Function use (Nearest                => 16#2600#,
                               Linear                 => 16#2601#,
                               Nearest_Mipmap_Nearest => 16#2700#,
                               Linear_Mipmap_Nearest  => 16#2701#,
                               Nearest_Mipmap_Linear  => 16#2702#,
                               Linear_Mipmap_Linear   => 16#2703#);
   for Minifying_Function'Size use Int'Size;

   subtype Magnifying_Function is Minifying_Function range Nearest .. Linear;

   type Wrapping_Mode is (Clamp, Repeat, Clamp_To_Border, Clamp_To_Edge,
                          Mirrored_Repeat);

   subtype Priority is Double range 0.0 .. 1.0;

   type Depth_Mode is (Alpha, Luminance, Intensity);

   -- Actual range is implementation-defined.
   --  OpenGL 2.x: At least 2
   --  OpenGL 3.x: At least 48
   --  OpenGL 4.x: At least 80
   subtype Texture_Unit is Int range 0 .. Int'Last;

   subtype Mipmap_Level is Int range 0 .. Int'Last;

   -----------------------------------------------------------------------------
   --                          Texture Objects                                --
   -----------------------------------------------------------------------------

   type Texture is abstract new GL_Object with private;

   overriding
   procedure Initialize_Id (Object : in out Texture);

   procedure Initialize_Id (Object : in out Texture;
                            Kind   :        Low_Level.Enums.Non_Proxy_Texture_Kind);

   overriding
   procedure Delete_Id (Object : in out Texture);

   procedure Invalidate_Image (Object : Texture; Level : Mipmap_Level);

   procedure Invalidate_Sub_Image (Object : Texture; Level : Mipmap_Level;
                                   X, Y, Z : Int; Width, Height, Depth : Size);

   procedure Bind_Texture_Unit (Object : Texture; Unit : Texture_Unit);

   type Texture_Base is abstract new Texture with private;

   procedure Generate_Mipmap (Object : Texture_Base);

   -----------------------------------------------------------------------------
   --                            Texture Parameters                           --
   -----------------------------------------------------------------------------

   procedure Set_Minifying_Filter     (Object : Texture_Base; Filter : Minifying_Function);
   procedure Set_Magnifying_Filter    (Object : Texture_Base; Filter : Magnifying_Function);
   procedure Set_Minimum_LoD          (Object : Texture_Base; Level : Double);
   procedure Set_Maximum_LoD          (Object : Texture_Base; Level : Double);
   procedure Set_Lowest_Mipmap_Level  (Object : Texture_Base; Level : Mipmap_Level);
   procedure Set_Highest_Mipmap_Level (Object : Texture_Base; Level : Mipmap_Level);

   function Minifying_Filter     (Object : Texture_Base) return Minifying_Function;
   function Magnifying_Filter    (Object : Texture_Base) return Magnifying_Function;
   function Minimum_LoD          (Object : Texture_Base) return Double;
   function Maximum_LoD          (Object : Texture_Base) return Double;
   function Lowest_Mipmap_Level  (Object : Texture_Base) return Mipmap_Level;
   function Highest_Mipmap_Level (Object : Texture_Base) return Mipmap_Level;

   procedure Set_Max_Anisotropy (Object : Texture_Base; Degree : Double)
     with Pre => Degree >= 1.0;
   --  Set the maximum amount of anisotropy filtering to reduce the blurring
   --  of textures (caused by mipmap filtering) that are viewed at an
   --  oblique angle.
   --
   --  For best results, combine the use of anisotropy filtering with
   --  a Linear_Mipmap_Linear minification filter and a Linear maxification
   --  filter.
   --
   --  Note: this procedure requires the EXT_texture_filter_anisotropic
   --  extension. This extension is not part of core OpenGL, but is basically
   --  available anywhere.

   function Max_Anisotropy (Object : Texture_Base) return Double
     with Post => Max_Anisotropy'Result >= 1.0;

   procedure Set_X_Wrapping (Object : Texture_Base; Mode : Wrapping_Mode);
   procedure Set_Y_Wrapping (Object : Texture_Base; Mode : Wrapping_Mode);
   procedure Set_Z_Wrapping (Object : Texture_Base; Mode : Wrapping_Mode);

   function X_Wrapping (Object : Texture_Base) return Wrapping_Mode;
   function Y_Wrapping (Object : Texture_Base) return Wrapping_Mode;
   function Z_Wrapping (Object : Texture_Base) return Wrapping_Mode;

   procedure Set_Border_Color         (Object : Texture_Base; Color : Colors.Color);
   procedure Set_Texture_Priority     (Object : Texture_Base; Value : Priority);
   procedure Set_Depth_Texture_Mode   (Object : Texture_Base; Mode  : Depth_Mode);
   procedure Toggle_Mipmap_Autoupdate (Object : Texture_Base; Enabled : Boolean);

   function Border_Color              (Object : Texture_Base) return Colors.Color;
   function Texture_Priority          (Object : Texture_Base) return Priority;
   function Depth_Texture_Mode        (Object : Texture_Base) return Depth_Mode;
   function Mipmap_Autoupdate_Enabled (Object : Texture_Base) return Boolean;

   procedure Toggle_Compare_X_To_Texture (Object : Texture_Base; Enabled : Boolean);
   procedure Set_Compare_Function        (Object : Texture_Base; Func : Compare_Function);

   function Compare_X_To_Texture_Enabled (Object : Texture_Base) return Boolean;
   function Current_Compare_Function     (Object : Texture_Base) return Compare_Function;

   -----------------------------------------------------------------------------
   --                         Texture Level Parameters                        --
   -----------------------------------------------------------------------------

   function Width  (Object : Texture_Base; Level : Mipmap_Level) return Size;
   function Height (Object : Texture_Base; Level : Mipmap_Level) return Size;
   function Depth  (Object : Texture_Base; Level : Mipmap_Level) return Size;

   function Format (Object : Texture_Base; Level : Mipmap_Level)
     return Pixels.Internal_Format;

   function Red_Type (Object : Texture_Base; Level : Mipmap_Level)
     return Pixels.Channel_Data_Type;
   function Green_Type (Object : Texture_Base; Level : Mipmap_Level)
     return Pixels.Channel_Data_Type;
   function Blue_Type (Object : Texture_Base; Level : Mipmap_Level)
     return Pixels.Channel_Data_Type;
   function Alpha_Type (Object : Texture_Base; Level : Mipmap_Level)
     return Pixels.Channel_Data_Type;
   function Depth_Type (Object : Texture_Base; Level : Mipmap_Level)
     return Pixels.Channel_Data_Type;

   function Red_Size   (Object : Texture_Base; Level : Mipmap_Level) return Size;
   function Green_Size (Object : Texture_Base; Level : Mipmap_Level) return Size;
   function Blue_Size  (Object : Texture_Base; Level : Mipmap_Level) return Size;
   function Alpha_Size (Object : Texture_Base; Level : Mipmap_Level) return Size;
   function Depth_Size (Object : Texture_Base; Level : Mipmap_Level) return Size;

   function Compressed (Object : Texture_Base; Level : Mipmap_Level) return Boolean;
   function Compressed_Image_Size (Object : Texture_Base; Level : Mipmap_Level) return Size;

   function Buffer_Offset (Object : Texture_Base; Level : Mipmap_Level) return Size;
   function Buffer_Size   (Object : Texture_Base; Level : Mipmap_Level) return Size;

   -----------------------------------------------------------------------------
   --                            Texture Units                                --
   -----------------------------------------------------------------------------

   function Active_Unit return Texture_Unit;

   function Texture_Unit_Count return Natural;

   -----------------------------------------------------------------------------
   --                        Buffer Texture Loading                           --
   -----------------------------------------------------------------------------

   type Buffer_Texture is new Texture with private;

   procedure Attach_Buffer (Object : Buffer_Texture;
                            Internal_Format : Pixels.Internal_Format_Buffer_Texture;
                            Buffer : Objects.Buffers.Buffer);

   procedure Attach_Buffer (Object : Buffer_Texture;
                            Internal_Format : Pixels.Internal_Format_Buffer_Texture;
                            Buffer : Objects.Buffers.Buffer;
                            Offset, Size : Types.Size);

   -----------------------------------------------------------------------------
   --                          Texture 1D Loading                             --
   -----------------------------------------------------------------------------

   type Texture_1D is new Texture_Base with private;

   procedure Allocate_Storage (Object : Texture_1D; Levels : Types.Size;
                               Internal_Format : Pixels.Internal_Format;
                               Width : Types.Size);

   procedure Load_Empty_Texture (Object : Texture_1D; Level : Mipmap_Level;
                                 Offset_X : Types.Size;
                                 Width    : Types.Size);

   procedure Load_From_Data (Object : Texture_1D; Level : Mipmap_Level;
                             Offset_X : Types.Size;
                             Width    : Types.Size;
                             Source_Format : Pixels.Format;
                             Source_Type   : Pixels.Data_Type;
                             Source        : System.Address);

   procedure Load_From_Compressed_Data (Object : Texture_1D; Level : Mipmap_Level;
                                        Offset_X : Types.Size;
                                        Width    : Types.Size;
                                        Source_Format : Pixels.Format;
                                        Image_Size : Types.Size;
                                        Source     : System.Address);

   procedure Load_From_Buffer (Object : Texture_1D; Level : Mipmap_Level;
                               Offset_X : Types.Size;
                               X, Y  : Types.Size;
                               Width : Types.Size);

   -----------------------------------------------------------------------------
   --                          Texture 2D Loading                             --
   -----------------------------------------------------------------------------

   type Texture_2D is new Texture_Base with private;

   procedure Allocate_Storage (Object : Texture_2D; Levels : Types.Size;
                               Internal_Format : Pixels.Internal_Format;
                               Width, Height   : Types.Size);

   procedure Allocate_Storage_Multisample (Object : Texture_2D; Samples : Types.Size;
                                           Internal_Format : Pixels.Internal_Format;
                                           Width, Height   : Types.Size;
                                           Fixed_Locations : Boolean);

   procedure Load_Empty_Texture (Object : Texture_2D; Level : Mipmap_Level;
                                 Offset_X, Offset_Y : Types.Size;
                                 Width, Height      : Types.Size);

   procedure Load_From_Data (Object : Texture_2D; Level : Mipmap_Level;
                             Offset_X, Offset_Y : Types.Size;
                             Width, Height      : Types.Size;
                             Source_Format : Pixels.Format;
                             Source_Type   : Pixels.Data_Type;
                             Source        : System.Address);

   procedure Load_From_Compressed_Data (Object : Texture_2D; Level : Mipmap_Level;
                                        Offset_X, Offset_Y : Types.Size;
                                        Width, Height      : Types.Size;
                                        Source_Format : Pixels.Format;
                                        Image_Size    : Types.Size;
                                        Source        : System.Address);

   procedure Load_From_Buffer (Object : Texture_2D; Level : Mipmap_Level;
                               Offset_X, Offset_Y : Types.Size;
                               X, Y          : Types.Size;
                               Width, Height : Types.Size);

   -----------------------------------------------------------------------------
   --                          Texture 3D Loading                             --
   -----------------------------------------------------------------------------

   type Texture_3D is new Texture_Base with private;

   procedure Allocate_Storage (Object : Texture_3D; Levels : Types.Size;
                               Internal_Format      : Pixels.Internal_Format;
                               Width, Height, Depth : Types.Size);

   procedure Allocate_Storage_Multisample (Object : Texture_3D; Samples : Types.Size;
                                           Internal_Format      : Pixels.Internal_Format;
                                           Width, Height, Depth : Types.Size;
                                           Fixed_Locations : Boolean);

   procedure Load_Empty_Texture (Object : Texture_3D; Level  : Mipmap_Level;
                                 Offset_X, Offset_Y, Offset_Z : Types.Size;
                                 Width, Height, Depth         : Types.Size);

   procedure Load_From_Data (Object : Texture_3D; Level : Mipmap_Level;
                             Offset_X, Offset_Y, Offset_Z : Types.Size;
                             Width, Height, Depth         : Types.Size;
                             Source_Format : Pixels.Format;
                             Source_Type   : Pixels.Data_Type;
                             Source        : System.Address);

   procedure Load_From_Compressed_Data (Object : Texture_3D; Level : Mipmap_Level;
                                        Offset_X, Offset_Y, Offset_Z : Types.Size;
                                        Width, Height, Depth         : Types.Size;
                                        Source_Format : Pixels.Format;
                                        Image_Size    : Types.Size;
                                        Source        : System.Address);

   procedure Load_From_Buffer (Object : Texture_3D; Level : Mipmap_Level;
                               Offset_X, Offset_Y, Offset_Z : Types.Size;
                               X, Y          : Types.Size;
                               Width, Height : Types.Size);

private

   for Wrapping_Mode use (Clamp           => 16#2900#,
                          Repeat          => 16#2901#,
                          Clamp_To_Border => 16#812D#,
                          Clamp_To_Edge   => 16#812F#,
                          Mirrored_Repeat => 16#8370#);
   for Wrapping_Mode'Size use Int'Size;

   for Depth_Mode use (Alpha     => 16#1906#,
                       Luminance => 16#1909#,
                       Intensity => 16#8049#);
   for Depth_Mode'Size use Int'Size;

   type Texture is new GL_Object with null record;
   type Texture_Base is new Texture with null record;

   type Buffer_Texture is new Texture with null record;

   type Texture_1D is new Texture_Base with null record;
   type Texture_2D is new Texture_Base with null record;
   type Texture_3D is new Texture_Base with null record;

end GL.Objects.Textures;
