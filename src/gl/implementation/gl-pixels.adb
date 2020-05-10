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

package body GL.Pixels is

   procedure Set_Pack_Swap_Bytes (Value : Boolean) is
   begin
      API.Pixel_Store_Bool.Ref (Enums.Pack_Swap_Bytes, Low_Level.Bool (Value));
   end Set_Pack_Swap_Bytes;

   procedure Set_Pack_LSB_First (Value : Boolean) is
   begin
      API.Pixel_Store_Bool.Ref (Enums.Pack_LSB_First, Low_Level.Bool (Value));
   end Set_Pack_LSB_First;

   procedure Set_Pack_Row_Length (Value : Size) is
   begin
      API.Pixel_Store_Size.Ref (Enums.Pack_Row_Length, Value);
   end Set_Pack_Row_Length;

   procedure Set_Pack_Image_Height (Value : Size) is
   begin
      API.Pixel_Store_Size.Ref (Enums.Pack_Image_Height, Value);
   end Set_Pack_Image_Height;

   procedure Set_Pack_Skip_Pixels (Value : Size) is
   begin
      API.Pixel_Store_Size.Ref (Enums.Pack_Skip_Pixels, Value);
   end Set_Pack_Skip_Pixels;

   procedure Set_Pack_Skip_Rows (Value : Size) is
   begin
      API.Pixel_Store_Size.Ref (Enums.Pack_Skip_Rows, Value);
   end Set_Pack_Skip_Rows;

   procedure Set_Pack_Skip_Images (Value : Size) is
   begin
      API.Pixel_Store_Size.Ref (Enums.Pack_Skip_Images, Value);
   end Set_Pack_Skip_Images;

   procedure Set_Pack_Alignment (Value : Alignment) is
   begin
      API.Pixel_Store_Alignment.Ref (Enums.Pack_Alignment, Value);
   end Set_Pack_Alignment;

   procedure Set_Pack_Compressed_Block_Width (Value : Size) is
   begin
      API.Pixel_Store_Size.Ref (Enums.Pack_Compressed_Block_Width, Value);
   end Set_Pack_Compressed_Block_Width;

   procedure Set_Pack_Compressed_Block_Height (Value : Size) is
   begin
      API.Pixel_Store_Size.Ref (Enums.Pack_Compressed_Block_Height, Value);
   end Set_Pack_Compressed_Block_Height;

   procedure Set_Pack_Compressed_Block_Depth (Value : Size) is
   begin
      API.Pixel_Store_Size.Ref (Enums.Pack_Compressed_Block_Depth, Value);
   end Set_Pack_Compressed_Block_Depth;

   procedure Set_Pack_Compressed_Block_Size (Value : Size) is
   begin
      API.Pixel_Store_Size.Ref (Enums.Pack_Compressed_Block_Size, Value);
   end Set_Pack_Compressed_Block_Size;

   -----------------------------------------------------------------------------

   function Pack_Swap_Bytes return Boolean is
      Ret : Low_Level.Bool := Low_Level.Bool (False);
   begin
      API.Get_Boolean.Ref (Enums.Getter.Pack_Swap_Bytes, Ret);
      return Boolean (Ret);
   end Pack_Swap_Bytes;

   function Pack_LSB_First return Boolean is
      Ret : Low_Level.Bool := Low_Level.Bool (False);
   begin
      API.Get_Boolean.Ref (Enums.Getter.Pack_Lsb_First, Ret);
      return Boolean (Ret);
   end Pack_LSB_First;

   function Pack_Row_Length return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Pack_Row_Length, Ret);
      return Size (Ret);
   end Pack_Row_Length;

   function Pack_Image_Height return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Pack_Image_Height, Ret);
      return Size (Ret);
   end Pack_Image_Height;

   function Pack_Skip_Pixels return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Pack_Skip_Pixels, Ret);
      return Size (Ret);
   end Pack_Skip_Pixels;

   function Pack_Skip_Rows return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Pack_Skip_Rows, Ret);
      return Size (Ret);
   end Pack_Skip_Rows;

   function Pack_Skip_Images return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Pack_Skip_Images, Ret);
      return Size (Ret);
   end Pack_Skip_Images;

   function Pack_Alignment return Alignment is
      Ret : Alignment := Words;
   begin
      API.Get_Alignment.Ref (Enums.Getter.Pack_Alignment, Ret);
      return Ret;
   end Pack_Alignment;

   function Pack_Compressed_Block_Width return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Pack_Compressed_Block_Width, Ret);
      return Size (Ret);
   end Pack_Compressed_Block_Width;

   function Pack_Compressed_Block_Height return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Pack_Compressed_Block_Height, Ret);
      return Size (Ret);
   end Pack_Compressed_Block_Height;

   function Pack_Compressed_Block_Depth return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Pack_Compressed_Block_Depth, Ret);
      return Size (Ret);
   end Pack_Compressed_Block_Depth;

   function Pack_Compressed_Block_Size return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Pack_Compressed_Block_Size, Ret);
      return Size (Ret);
   end Pack_Compressed_Block_Size;

   -----------------------------------------------------------------------------

   procedure Set_Unpack_Swap_Bytes (Value : Boolean) is
   begin
      API.Pixel_Store_Bool.Ref (Enums.Unpack_Swap_Bytes, Low_Level.Bool (Value));
   end Set_Unpack_Swap_Bytes;

   procedure Set_Unpack_LSB_First (Value : Boolean) is
   begin
      API.Pixel_Store_Bool.Ref (Enums.Unpack_LSB_First, Low_Level.Bool (Value));
   end Set_Unpack_LSB_First;

   procedure Set_Unpack_Row_Length (Value : Size) is
   begin
      API.Pixel_Store_Size.Ref (Enums.Unpack_Row_Length, Value);
   end Set_Unpack_Row_Length;

   procedure Set_Unpack_Image_Height (Value : Size) is
   begin
      API.Pixel_Store_Size.Ref (Enums.Unpack_Image_Height, Value);
   end Set_Unpack_Image_Height;

   procedure Set_Unpack_Skip_Pixels (Value : Size) is
   begin
      API.Pixel_Store_Size.Ref (Enums.Unpack_Skip_Pixels, Value);
   end Set_Unpack_Skip_Pixels;

   procedure Set_Unpack_Skip_Rows (Value : Size) is
   begin
      API.Pixel_Store_Size.Ref (Enums.Unpack_Skip_Rows, Value);
   end Set_Unpack_Skip_Rows;

   procedure Set_Unpack_Skip_Images (Value : Size) is
   begin
      API.Pixel_Store_Size.Ref (Enums.Unpack_Skip_Images, Value);
   end Set_Unpack_Skip_Images;

   procedure Set_Unpack_Alignment (Value : Alignment) is
   begin
      API.Pixel_Store_Alignment.Ref (Enums.Unpack_Alignment, Value);
   end Set_Unpack_Alignment;

   procedure Set_Unpack_Compressed_Block_Width (Value : Size) is
   begin
      API.Pixel_Store_Size.Ref (Enums.Unpack_Compressed_Block_Width, Value);
   end Set_Unpack_Compressed_Block_Width;

   procedure Set_Unpack_Compressed_Block_Height (Value : Size) is
   begin
      API.Pixel_Store_Size.Ref (Enums.Unpack_Compressed_Block_Height, Value);
   end Set_Unpack_Compressed_Block_Height;

   procedure Set_Unpack_Compressed_Block_Depth (Value : Size) is
   begin
      API.Pixel_Store_Size.Ref (Enums.Unpack_Compressed_Block_Depth, Value);
   end Set_Unpack_Compressed_Block_Depth;

   procedure Set_Unpack_Compressed_Block_Size (Value : Size) is
   begin
      API.Pixel_Store_Size.Ref (Enums.Unpack_Compressed_Block_Size, Value);
   end Set_Unpack_Compressed_Block_Size;

   -----------------------------------------------------------------------------

   function Unpack_Swap_Bytes return Boolean is
      Ret : Low_Level.Bool := Low_Level.Bool (False);
   begin
      API.Get_Boolean.Ref (Enums.Getter.Unpack_Swap_Bytes, Ret);
      return Boolean (Ret);
   end Unpack_Swap_Bytes;

   function Unpack_LSB_First return Boolean is
      Ret : Low_Level.Bool := Low_Level.Bool (False);
   begin
      API.Get_Boolean.Ref (Enums.Getter.Unpack_Lsb_First, Ret);
      return Boolean (Ret);
   end Unpack_LSB_First;

   function Unpack_Row_Length return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Unpack_Row_Length, Ret);
      return Size (Ret);
   end Unpack_Row_Length;

   function Unpack_Image_Height return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Unpack_Image_Height, Ret);
      return Size (Ret);
   end Unpack_Image_Height;

   function Unpack_Skip_Pixels return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Unpack_Skip_Pixels, Ret);
      return Size (Ret);
   end Unpack_Skip_Pixels;

   function Unpack_Skip_Rows return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Unpack_Skip_Rows, Ret);
      return Size (Ret);
   end Unpack_Skip_Rows;

   function Unpack_Skip_Images return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Unpack_Skip_Images, Ret);
      return Size (Ret);
   end Unpack_Skip_Images;

   function Unpack_Alignment return Alignment is
      Ret : Alignment := Words;
   begin
      API.Get_Alignment.Ref (Enums.Getter.Unpack_Alignment, Ret);
      return Ret;
   end Unpack_Alignment;

   function Unpack_Compressed_Block_Width return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Unpack_Compressed_Block_Width, Ret);
      return Size (Ret);
   end Unpack_Compressed_Block_Width;

   function Unpack_Compressed_Block_Height return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Unpack_Compressed_Block_Height, Ret);
      return Size (Ret);
   end Unpack_Compressed_Block_Height;

   function Unpack_Compressed_Block_Depth return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Unpack_Compressed_Block_Depth, Ret);
      return Size (Ret);
   end Unpack_Compressed_Block_Depth;

   function Unpack_Compressed_Block_Size return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Unpack_Compressed_Block_Size, Ret);
      return Size (Ret);
   end Unpack_Compressed_Block_Size;

end GL.Pixels;
