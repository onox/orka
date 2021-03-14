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

   procedure Set_Pack_Alignment (Value : Alignment) is
   begin
      API.Pixel_Store_Alignment.Ref (Enums.Pack_Alignment, Value);
   end Set_Pack_Alignment;

   function Pack_Alignment return Alignment is
      Ret : Alignment := Words;
   begin
      API.Get_Alignment.Ref (Enums.Getter.Pack_Alignment, Ret);
      return Ret;
   end Pack_Alignment;

   procedure Set_Unpack_Alignment (Value : Alignment) is
   begin
      API.Pixel_Store_Alignment.Ref (Enums.Unpack_Alignment, Value);
   end Set_Unpack_Alignment;

   function Unpack_Alignment return Alignment is
      Ret : Alignment := Words;
   begin
      API.Get_Alignment.Ref (Enums.Getter.Unpack_Alignment, Ret);
      return Ret;
   end Unpack_Alignment;

   -----------------------------------------------------------------------------

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
