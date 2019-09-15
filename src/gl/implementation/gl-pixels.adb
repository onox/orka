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
      API.Pixel_Store (Enums.Pack_Swap_Bytes, Low_Level.Bool (Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Pack_Swap_Bytes;

   procedure Set_Pack_LSB_First (Value : Boolean) is
   begin
      API.Pixel_Store (Enums.Pack_LSB_First, Low_Level.Bool (Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Pack_LSB_First;

   procedure Set_Pack_Row_Length (Value : Size) is
   begin
      API.Pixel_Store (Enums.Pack_Row_Length, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Pack_Row_Length;

   procedure Set_Pack_Image_Height (Value : Size) is
   begin
      API.Pixel_Store (Enums.Pack_Image_Height, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Pack_Image_Height;

   procedure Set_Pack_Skip_Pixels (Value : Size) is
   begin
      API.Pixel_Store (Enums.Pack_Skip_Pixels, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Pack_Skip_Pixels;

   procedure Set_Pack_Skip_Rows (Value : Size) is
   begin
      API.Pixel_Store (Enums.Pack_Skip_Rows, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Pack_Skip_Rows;

   procedure Set_Pack_Skip_Images (Value : Size) is
   begin
      API.Pixel_Store (Enums.Pack_Skip_Images, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Pack_Skip_Images;

   procedure Set_Pack_Alignment (Value : Alignment) is
   begin
      API.Pixel_Store (Enums.Pack_Alignment, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Pack_Alignment;

   procedure Set_Pack_Compressed_Block_Width (Value : Size) is
   begin
      API.Pixel_Store (Enums.Pack_Compressed_Block_Width, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Pack_Compressed_Block_Width;

   procedure Set_Pack_Compressed_Block_Height (Value : Size) is
   begin
      API.Pixel_Store (Enums.Pack_Compressed_Block_Height, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Pack_Compressed_Block_Height;

   procedure Set_Pack_Compressed_Block_Depth (Value : Size) is
   begin
      API.Pixel_Store (Enums.Pack_Compressed_Block_Depth, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Pack_Compressed_Block_Depth;

   procedure Set_Pack_Compressed_Block_Size (Value : Size) is
   begin
      API.Pixel_Store (Enums.Pack_Compressed_Block_Size, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Pack_Compressed_Block_Size;

   -----------------------------------------------------------------------------

   function Pack_Swap_Bytes return Boolean is
      Ret : Low_Level.Bool := Low_Level.Bool (False);
   begin
      API.Get_Boolean (Enums.Getter.Pack_Swap_Bytes, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Boolean (Ret);
   end Pack_Swap_Bytes;

   function Pack_LSB_First return Boolean is
      Ret : Low_Level.Bool := Low_Level.Bool (False);
   begin
      API.Get_Boolean (Enums.Getter.Pack_Lsb_First, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Boolean (Ret);
   end Pack_LSB_First;

   function Pack_Row_Length return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer (Enums.Getter.Pack_Row_Length, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Pack_Row_Length;

   function Pack_Image_Height return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer (Enums.Getter.Pack_Image_Height, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Pack_Image_Height;

   function Pack_Skip_Pixels return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer (Enums.Getter.Pack_Skip_Pixels, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Pack_Skip_Pixels;

   function Pack_Skip_Rows return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer (Enums.Getter.Pack_Skip_Rows, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Pack_Skip_Rows;

   function Pack_Skip_Images return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer (Enums.Getter.Pack_Skip_Images, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Pack_Skip_Images;

   function Pack_Alignment return Alignment is
      Ret : Alignment := Words;
   begin
      API.Get_Alignment (Enums.Getter.Pack_Alignment, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Pack_Alignment;

   function Pack_Compressed_Block_Width return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer (Enums.Getter.Pack_Compressed_Block_Width, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Pack_Compressed_Block_Width;

   function Pack_Compressed_Block_Height return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer (Enums.Getter.Pack_Compressed_Block_Height, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Pack_Compressed_Block_Height;

   function Pack_Compressed_Block_Depth return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer (Enums.Getter.Pack_Compressed_Block_Depth, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Pack_Compressed_Block_Depth;

   function Pack_Compressed_Block_Size return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer (Enums.Getter.Pack_Compressed_Block_Size, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Pack_Compressed_Block_Size;

   -----------------------------------------------------------------------------

   procedure Set_Unpack_Swap_Bytes (Value : Boolean) is
   begin
      API.Pixel_Store (Enums.Unpack_Swap_Bytes, Low_Level.Bool (Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Unpack_Swap_Bytes;

   procedure Set_Unpack_LSB_First (Value : Boolean) is
   begin
      API.Pixel_Store (Enums.Unpack_LSB_First, Low_Level.Bool (Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Unpack_LSB_First;

   procedure Set_Unpack_Row_Length (Value : Size) is
   begin
      API.Pixel_Store (Enums.Unpack_Row_Length, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Unpack_Row_Length;

   procedure Set_Unpack_Image_Height (Value : Size) is
   begin
      API.Pixel_Store (Enums.Unpack_Image_Height, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Unpack_Image_Height;

   procedure Set_Unpack_Skip_Pixels (Value : Size) is
   begin
      API.Pixel_Store (Enums.Unpack_Skip_Pixels, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Unpack_Skip_Pixels;

   procedure Set_Unpack_Skip_Rows (Value : Size) is
   begin
      API.Pixel_Store (Enums.Unpack_Skip_Rows, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Unpack_Skip_Rows;

   procedure Set_Unpack_Skip_Images (Value : Size) is
   begin
      API.Pixel_Store (Enums.Unpack_Skip_Images, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Unpack_Skip_Images;

   procedure Set_Unpack_Alignment (Value : Alignment) is
   begin
      API.Pixel_Store (Enums.Unpack_Alignment, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Unpack_Alignment;

   procedure Set_Unpack_Compressed_Block_Width (Value : Size) is
   begin
      API.Pixel_Store (Enums.Unpack_Compressed_Block_Width, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Unpack_Compressed_Block_Width;

   procedure Set_Unpack_Compressed_Block_Height (Value : Size) is
   begin
      API.Pixel_Store (Enums.Unpack_Compressed_Block_Height, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Unpack_Compressed_Block_Height;

   procedure Set_Unpack_Compressed_Block_Depth (Value : Size) is
   begin
      API.Pixel_Store (Enums.Unpack_Compressed_Block_Depth, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Unpack_Compressed_Block_Depth;

   procedure Set_Unpack_Compressed_Block_Size (Value : Size) is
   begin
      API.Pixel_Store (Enums.Unpack_Compressed_Block_Size, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Unpack_Compressed_Block_Size;

   -----------------------------------------------------------------------------

   function Unpack_Swap_Bytes return Boolean is
      Ret : Low_Level.Bool := Low_Level.Bool (False);
   begin
      API.Get_Boolean (Enums.Getter.Unpack_Swap_Bytes, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Boolean (Ret);
   end Unpack_Swap_Bytes;

   function Unpack_LSB_First return Boolean is
      Ret : Low_Level.Bool := Low_Level.Bool (False);
   begin
      API.Get_Boolean (Enums.Getter.Unpack_Lsb_First, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Boolean (Ret);
   end Unpack_LSB_First;

   function Unpack_Row_Length return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer (Enums.Getter.Unpack_Row_Length, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Unpack_Row_Length;

   function Unpack_Image_Height return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer (Enums.Getter.Unpack_Image_Height, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Unpack_Image_Height;

   function Unpack_Skip_Pixels return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer (Enums.Getter.Unpack_Skip_Pixels, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Unpack_Skip_Pixels;

   function Unpack_Skip_Rows return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer (Enums.Getter.Unpack_Skip_Rows, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Unpack_Skip_Rows;

   function Unpack_Skip_Images return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer (Enums.Getter.Unpack_Skip_Images, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Unpack_Skip_Images;

   function Unpack_Alignment return Alignment is
      Ret : Alignment := Words;
   begin
      API.Get_Alignment (Enums.Getter.Unpack_Alignment, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Unpack_Alignment;

   function Unpack_Compressed_Block_Width return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer (Enums.Getter.Unpack_Compressed_Block_Width, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Unpack_Compressed_Block_Width;

   function Unpack_Compressed_Block_Height return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer (Enums.Getter.Unpack_Compressed_Block_Height, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Unpack_Compressed_Block_Height;

   function Unpack_Compressed_Block_Depth return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer (Enums.Getter.Unpack_Compressed_Block_Depth, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Unpack_Compressed_Block_Depth;

   function Unpack_Compressed_Block_Size return Size is
      Ret : Types.Int := 0;
   begin
      API.Get_Integer (Enums.Getter.Unpack_Compressed_Block_Size, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Unpack_Compressed_Block_Size;

end GL.Pixels;
