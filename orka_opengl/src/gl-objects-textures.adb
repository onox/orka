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

with GL.API;
with GL.Enums.Getter;
with GL.Enums.Textures;

package body GL.Objects.Textures is

   Base_Level : constant := 0;

   function Get_Dimensions (Kind : LE.Texture_Kind) return Dimension_Count is
   begin
      case Kind is
         when Texture_1D =>
            return One;
         when Texture_2D | Texture_2D_Multisample | Texture_1D_Array |
           Texture_Rectangle | Texture_Cube_Map =>
            return Two;
         when Texture_3D | Texture_2D_Array | Texture_2D_Multisample_Array |
           Texture_Cube_Map_Array =>
            return Three;
         when Texture_Buffer =>
            raise Constraint_Error;
      end case;
   end Get_Dimensions;

   function Dimensions (Object : Texture) return Dimension_Count is (Object.Dimensions);

   function Allocated (Object : Texture) return Boolean is (Object.Allocated);

   type Texture_View is new Texture with null record;

   overriding
   procedure Initialize_Id (Object : in out Texture_View) is
      New_Id : UInt := 0;
   begin
      --  A texture that is going to be used as a view must not be created
      --  with a target, therefore use Gen_Textures instead of Create_Textures
      API.Gen_Textures.Ref (1, New_Id);
      Object.Reference.GL_Id := New_Id;
   end Initialize_Id;

   function Create_View
     (Object    : Texture;
      Kind      : LE.Texture_Kind;
      Format    : Pixels.Internal_Format;
      Min_Level, Levels : Mipmap_Level;
      Min_Layer, Layers : Size) return Texture
   is
      Result : Texture_View (Kind => Kind);
   begin
      API.Texture_View_I.Ref (Result.Reference.GL_Id, Kind, Object.Reference.GL_Id,
        Format, UInt (Min_Level), UInt (Levels), UInt (Min_Layer), UInt (Layers));

      return Texture (Result);
   end Create_View;

   function Create_Compressed_View
     (Object    : Texture;
      Kind      : LE.Texture_Kind;
      Format    : Pixels.Compressed_Format;
      Min_Level, Levels : Mipmap_Level;
      Min_Layer, Layers : Size) return Texture
   is
      Result : Texture_View (Kind => Kind);
   begin
      API.Texture_View_C.Ref (Result.Reference.GL_Id, Kind, Object.Reference.GL_Id,
        Format, UInt (Min_Level), UInt (Levels), UInt (Min_Layer), UInt (Layers));

      return Texture (Result);
   end Create_Compressed_View;

   function Internal_Format (Object : Texture) return Pixels.Internal_Format is
      Result : Pixels.Internal_Format := Pixels.Internal_Format'First;
   begin
      API.Get_Texture_Level_Parameter_Format_I.Ref
        (Object.Reference.GL_Id, Base_Level, Enums.Textures.Internal_Format, Result);
      return Result;
   end Internal_Format;

   function Compressed (Object : Texture) return Boolean is
      Result : Size := 0;
   begin
      API.Get_Texture_Level_Parameter.Ref
        (Object.Reference.GL_Id, Base_Level, Enums.Textures.Compressed, Result);
      return Result = 1;
   end Compressed;

   function Compressed_Image_Size (Object : Texture; Level : Mipmap_Level) return Size is
      Ret : Size := 0;
   begin
      API.Get_Texture_Level_Parameter.Ref
        (Object.Reference.GL_Id, Level, Enums.Textures.Compressed_Image_Size, Ret);
      return Ret;
   end Compressed_Image_Size;

   procedure Bind_Texture_Unit (Object : Texture_Base; Unit : Texture_Unit) is
      IDs : constant Low_Level.UInt_Array := [Object.Reference.GL_Id];
   begin
      API.Bind_Textures.Ref (Unit, 1, IDs);
   end Bind_Texture_Unit;

   procedure Bind_Image_Texture (Object : Texture_Base; Unit : Image_Unit) is
      IDs : constant Low_Level.UInt_Array := [Object.Reference.GL_Id];
   begin
      API.Bind_Image_Textures.Ref (Unit, 1, IDs);
   end Bind_Image_Texture;

   overriding
   procedure Initialize_Id (Object : in out Texture_Base) is
      New_Id : UInt := 0;
   begin
      API.Create_Textures.Ref (Object.Kind, 1, New_Id);
      Object.Reference.GL_Id := New_Id;
   end Initialize_Id;

   overriding
   procedure Delete_Id (Object : in out Texture_Base) is
      Arr : constant Low_Level.UInt_Array := [Object.Reference.GL_Id];
   begin
      API.Delete_Textures.Ref (1, Arr);
      Object.Reference.GL_Id := 0;
   end Delete_Id;

   procedure Invalidate_Image (Object : Texture_Base; Level : Mipmap_Level) is
   begin
      API.Invalidate_Tex_Image.Ref (Object.Reference.GL_Id, Level);
   end Invalidate_Image;

   procedure Invalidate_Sub_Image (Object : Texture_Base; Level : Mipmap_Level;
                                   X, Y, Z : Int; Width, Height, Depth : Size)
   is
   begin
      API.Invalidate_Tex_Sub_Image.Ref (Object.Reference.GL_Id, Level, X, Y, Z,
                                    Width, Height, Depth);
   end Invalidate_Sub_Image;

   procedure Set_Lowest_Mipmap_Level (Object : Texture; Level : Mipmap_Level) is
   begin
      API.Texture_Parameter_Int.Ref (Object.Reference.GL_Id,
                                 Enums.Textures.Base_Level, Level);
   end Set_Lowest_Mipmap_Level;

   procedure Set_Highest_Mipmap_Level (Object : Texture; Level : Mipmap_Level) is
   begin
      API.Texture_Parameter_Int.Ref
        (Object.Reference.GL_Id, Enums.Textures.Max_Level, Level);
   end Set_Highest_Mipmap_Level;

   procedure Clear_Using_Data
     (Object : Texture; Level : Mipmap_Level;
      Source_Format : Pixels.Format;
      Source_Type   : Pixels.Data_Type;
      Source        : System.Address) is
   begin
      API.Clear_Tex_Image.Ref
        (Object.Reference.GL_Id, Level, Source_Format, Source_Type, Source);
   end Clear_Using_Data;

   procedure Clear_Using_Zeros
     (Object : Texture; Level : Mipmap_Level) is
   begin
      Object.Clear_Using_Data (Level, Pixels.Format'First, Pixels.Data_Type'First, System.Null_Address);
   end Clear_Using_Zeros;

   procedure Generate_Mipmap (Object : Texture) is
   begin
      API.Generate_Texture_Mipmap.Ref (Object.Reference.GL_Id);
   end Generate_Mipmap;

   function Maximum_Anisotropy return Single is
      Ret : Single := 16.0;
   begin
      API.Get_Single.Ref (Enums.Getter.Max_Texture_Max_Anisotropy, Ret);
      return Ret;
   end Maximum_Anisotropy;

   -----------------------------------------------------------------------------
   --                        Buffer Texture Loading                           --
   -----------------------------------------------------------------------------

   procedure Attach_Buffer (Object : Buffer_Texture;
                            Internal_Format : Pixels.Internal_Format_Buffer_Texture;
                            Buffer : Objects.Buffers.Buffer) is
   begin
      API.Texture_Buffer.Ref (Object.Reference.GL_Id, Internal_Format, Buffer.Raw_Id);
   end Attach_Buffer;

   procedure Attach_Buffer (Object : Buffer_Texture;
                            Internal_Format : Pixels.Internal_Format_Buffer_Texture;
                            Buffer : Objects.Buffers.Buffer;
                            Offset, Size : Types.Size) is
   begin
      API.Texture_Buffer_Range.Ref (Object.Reference.GL_Id, Internal_Format, Buffer.Raw_Id,
                                    Low_Level.IntPtr (Offset), Size);
   end Attach_Buffer;

   -----------------------------------------------------------------------------
   --                           Texture Loading                               --
   -----------------------------------------------------------------------------

   procedure Allocate_Storage
     (Object : in out Texture;
      Levels, Samples : Types.Size;
      Format : Pixels.Internal_Format;
      Width, Height, Depth : Types.Size;
      Fixed_Locations : Boolean := True) is
   begin
      if Object.Kind in Texture_2D_Multisample | Texture_2D_Multisample_Array then
         case Object.Dimensions is
            when One =>
               raise Program_Error;
            when Two =>
               API.Texture_Storage_2D_Multisample_I.Ref
                 (Object.Reference.GL_Id, Samples,
                  Format, Width, Height, Low_Level.Bool (Fixed_Locations));
            when Three =>
               API.Texture_Storage_3D_Multisample_I.Ref
                 (Object.Reference.GL_Id, Samples,
                  Format, Width, Height, Depth, Low_Level.Bool (Fixed_Locations));
         end case;
      else
         case Object.Dimensions is
            when One =>
               API.Texture_Storage_1D.Ref
                 (Object.Reference.GL_Id, Levels, Format, Width);
            when Two =>
               API.Texture_Storage_2D_I.Ref
                 (Object.Reference.GL_Id, Levels, Format, Width, Height);
            when Three =>
               API.Texture_Storage_3D_I.Ref
                 (Object.Reference.GL_Id, Levels, Format, Width, Height, Depth);
         end case;
      end if;
      Object.Allocated := True;
   end Allocate_Storage;

   procedure Allocate_Storage
     (Object : in out Texture;
      Levels, Samples : Types.Size;
      Format : Pixels.Compressed_Format;
      Width, Height, Depth : Types.Size;
      Fixed_Locations : Boolean := True) is
   begin
      if Object.Kind in Texture_2D_Multisample | Texture_2D_Multisample_Array then
         case Object.Dimensions is
            when One =>
               raise Program_Error;
            when Two =>
               API.Texture_Storage_2D_Multisample_C.Ref
                 (Object.Reference.GL_Id, Samples,
                  Format, Width, Height, Low_Level.Bool (Fixed_Locations));
            when Three =>
               API.Texture_Storage_3D_Multisample_C.Ref
                 (Object.Reference.GL_Id, Samples,
                  Format, Width, Height, Depth, Low_Level.Bool (Fixed_Locations));
         end case;
      else
         case Object.Dimensions is
            when One =>
               raise Program_Error;
            when Two =>
               API.Texture_Storage_2D_C.Ref
                 (Object.Reference.GL_Id, Levels, Format, Width, Height);
            when Three =>
               API.Texture_Storage_3D_C.Ref
                 (Object.Reference.GL_Id, Levels, Format, Width, Height, Depth);
         end case;
      end if;
      Object.Allocated := True;
   end Allocate_Storage;

   procedure Load_From_Data
     (Object : Texture;
      Level  : Mipmap_Level;
      X, Y, Z              : Types.Size := 0;
      Width, Height, Depth : Types.Positive_Size;
      Source_Format : Pixels.Format;
      Source_Type   : Pixels.Data_Type;
      Source        : System.Address)
   is
      --  Texture_Cube_Map uses 2D storage, but 3D load operation
      --  according to Table 8.15 of the OpenGL specification
      Dimensions : constant Dimension_Count
        := (if Object.Kind = Texture_Cube_Map then Three else Object.Dimensions);
   begin
      case Dimensions is
         when One =>
            API.Texture_Sub_Image_1D.Ref
              (Object.Reference.GL_Id, Level, X, Width, Source_Format,
               Source_Type, Source);
         when Two =>
            API.Texture_Sub_Image_2D.Ref
              (Object.Reference.GL_Id, Level, X, Y, Width, Height,
               Source_Format, Source_Type, Source);
         when Three =>
            API.Texture_Sub_Image_3D.Ref
              (Object.Reference.GL_Id, Level, X, Y, Z, Width, Height, Depth,
               Source_Format, Source_Type, Source);
      end case;
   end Load_From_Data;

   procedure Load_From_Data
     (Object : Texture;
      Level  : Mipmap_Level;
      X, Y, Z              : Types.Size := 0;
      Width, Height, Depth : Types.Positive_Size;
      Source_Format : Pixels.Compressed_Format;
      Image_Size    : Types.Size;
      Source        : System.Address)
   is
      --  Texture_Cube_Map uses 2D storage, but 3D load operation
      --  according to Table 8.15 of the OpenGL specification
      Dimensions : constant Dimension_Count
        := (if Object.Kind = Texture_Cube_Map then Three else Object.Dimensions);
   begin
      case Dimensions is
         when One =>
            raise Program_Error;
         when Two =>
            API.Compressed_Texture_Sub_Image_2D.Ref
              (Object.Reference.GL_Id, Level, X, Y, Width, Height,
               Source_Format, Image_Size, Source);
         when Three =>
            API.Compressed_Texture_Sub_Image_3D.Ref
              (Object.Reference.GL_Id, Level, X, Y, Z, Width, Height, Depth,
               Source_Format, Image_Size, Source);
      end case;
   end Load_From_Data;

   procedure Copy_Sub_Data
     (Object  : Texture;
      Subject : Texture;
      Source_Level, Target_Level : Mipmap_Level;
      Source_X, Source_Y, Source_Z : Types.Size := 0;
      Target_X, Target_Y, Target_Z : Types.Size := 0;
      Width, Height, Depth : Types.Size) is
   begin
      API.Copy_Image_Sub_Data.Ref
        (Object.Reference.GL_Id, Object.Kind, Source_Level,
         Source_X, Source_Y, Source_Z,
         Subject.Reference.GL_Id, Subject.Kind, Target_Level,
         Target_X, Target_Y, Target_Z,
         Width, Height, Depth);
   end Copy_Sub_Data;

   procedure Clear_Using_Data
     (Object : Texture;
      Level  : Mipmap_Level;
      X, Y, Z              : Types.Size := 0;
      Width, Height, Depth : Types.Positive_Size;
      Source_Format : Pixels.Format;
      Source_Type   : Pixels.Data_Type;
      Source        : System.Address) is
   begin
      API.Clear_Tex_Sub_Image.Ref
        (Object.Reference.GL_Id, Level, X, Y, Z, Width, Height, Depth,
         Source_Format, Source_Type, Source);
   end Clear_Using_Data;

   procedure Clear_Using_Zeros
     (Object : Texture;
      Level  : Mipmap_Level;
      X, Y, Z              : Types.Size := 0;
      Width, Height, Depth : Types.Positive_Size) is
   begin
      Object.Clear_Using_Data
        (Level, X, Y, Z, Width, Height, Depth,
         Pixels.Format'First, Pixels.Data_Type'First, System.Null_Address);
   end Clear_Using_Zeros;

   -----------------------------------------------------------------------------

   function Get_Compressed_Data
     (Object : Texture;
      Level  : Mipmap_Level;
      X, Y, Z              : Types.Size := 0;
      Width, Height, Depth : Types.Positive_Size;
      Format : Pixels.Compressed_Format) return not null Types.Pointers.UByte_Array_Access
   is
      Blocks : constant Int := ((Width + 3) / 4) * ((Height + 3) / 4) * Depth;
      Number_Of_Bytes : constant Int := Blocks * PE.Block_Bytes (Format);

      Result : constant Types.Pointers.UByte_Array_Access
        := new UByte_Array (1 .. Number_Of_Bytes);
   begin
      API.Get_Compressed_Texture_Sub_Image.Ref
        (Object.Reference.GL_Id, Level, X, Y, Z, Width, Height, Depth,
         Number_Of_Bytes, Result);
      return Result;
   end Get_Compressed_Data;

   package body Texture_Pointers is

      package Get_Texture_Sub_Image is new API.Loader.Procedure_With_12_Params
        ("glGetTextureSubImage", UInt, Objects.Textures.Mipmap_Level,
         Int, Int, Int, Size, Size, Size, Pixels.Format, Pixels.Data_Type,
         Size, Element_Array_Access);

      function Get_Data
        (Object    : Texture;
         Level     : Mipmap_Level;
         X, Y, Z              : Types.Size := 0;
         Width, Height, Depth : Types.Positive_Size;
         Format    : Pixels.Format;
         Data_Type : Pixels.Data_Type) return not null Element_Array_Access
      is
         Is_Packed : constant Boolean := Data_Type in PE.Packed_Data_Type;

         Bytes_Data_Type : constant Byte_Count :=
           (if Is_Packed then PE.Packed_Bytes (Data_Type) else PE.Bytes (Data_Type));

         --  Texture data is considered to be unpacked. When retrieving
         --  it from a texture, it will be packed. Therefore, each row
         --  must be a multiple of the current pack alignment. Call
         --  Set_Pack_Alignment if necessary.
         Alignment : constant Byte_Count := PE.Byte_Alignment (Pixels.Pack_Alignment);
         pragma Assert ((Width * Bytes_Data_Type) mod Alignment = 0);

         Bytes_Per_Element : constant Int := Pointers.Element'Size / System.Storage_Unit;
         Bytes_Per_Texel   : constant Int :=
           (if Is_Packed then Bytes_Data_Type else PE.Components (Format) * PE.Bytes (Data_Type));
         --  See packed data types and depth/stencil formats (see Table 8.5)
         pragma Assert (Bytes_Per_Texel rem Bytes_Per_Element = 0);

         Texels : constant Size := Width * Height * Depth;
         pragma Assert (Texels > 0);

         Number_Of_Bytes : constant Int := Texels * Bytes_Per_Texel;
         Length : constant Long := Long (Texels * (Bytes_Per_Texel / Bytes_Per_Element));

         I1 : constant Pointers.Index := Pointers.Index'First;
         I2 : constant Pointers.Index := Pointers.Index'Val (Pointers.Index'Pos (I1) + Length);
         Result : constant Element_Array_Access
           := new Pointers.Element_Array (I1 .. Pointers.Index'Pred (I2));
         pragma Assert (Result'Length > 0);
      begin
         Get_Texture_Sub_Image.Ref
           (Object.Reference.GL_Id, Level, X, Y, Z, Width, Height, Depth,
            Format, Data_Type, Number_Of_Bytes, Result);
         return Result;
      end Get_Data;

   end Texture_Pointers;

end GL.Objects.Textures;
