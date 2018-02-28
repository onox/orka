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
with GL.Pixels;
with GL.Types;

package body Orka.Rendering.Textures is

   use GL.Objects.Textures;

   function Bayer_Dithering_Pattern return GL.Objects.Textures.Texture_2D is
      Pixels : constant GL.Types.UByte_Array
        := (0, 32,  8, 40,  2, 34, 10, 42,
           48, 16, 56, 24, 50, 18, 58, 26,
           12, 44,  4, 36, 14, 46,  6, 38,
           60, 28, 52, 20, 62, 30, 54, 22,
            3, 35, 11, 43,  1, 33,  9, 41,
           51, 19, 59, 27, 49, 17, 57, 25,
           15, 47,  7, 39, 13, 45,  5, 37,
           63, 31, 55, 23, 61, 29, 53, 21);
      --  * Bayer, B. E. (1973). An optimum method for two-level rendition of
      --    continuous-tone pictures. In IEEE Int. Conf. on Communications
      --    (Vol. 26, pp. 11-15).
      --  * http://www.anisopteragames.com/how-to-fix-color-banding-with-dithering/
   begin
      return Result : Texture_2D (GL.Low_Level.Enums.Texture_2D) do
         Result.Set_X_Wrapping (Repeat);
         Result.Set_Y_Wrapping (Repeat);

         Result.Set_Minifying_Filter (Nearest);
         Result.Set_Magnifying_Filter (Nearest);

         Result.Allocate_Storage (1, GL.Pixels.R8, 8, 8);
         Result.Load_From_Data (0, 0, 0, 8, 8, GL.Pixels.Red,
           GL.Pixels.Unsigned_Byte, Pixels'Address);
      end return;
   end Bayer_Dithering_Pattern;

end Orka.Rendering.Textures;
