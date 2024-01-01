--  SPDX-License-Identifier: Apache-2.0
--
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

with GL.Pixels.Extensions;
with GL.Types;

with Orka.Strings;

package body Orka.Rendering.Textures is

   procedure Bind
     (Object : GL.Objects.Textures.Texture_Base'Class;
      Target : Indexed_Texture_Target;
      Index  : Natural) is
   begin
      case Target is
         when Texture =>
            Object.Bind_Texture_Unit (GL.Types.UInt (Index));
         when Image =>
            Object.Bind_Image_Texture (GL.Types.UInt (Index));
      end case;
   end Bind;

   function Get_Format_Kind
     (Format : GL.Pixels.Internal_Format) return Format_Kind
   is
      package PE renames GL.Pixels.Extensions;
   begin
      if PE.Depth_Stencil_Format (Format) then
         return Depth_Stencil;
      elsif PE.Depth_Format (Format) then
         return Depth;
      elsif PE.Stencil_Format (Format) then
         return Stencil;
      else
         return Color;
      end if;
   end Get_Format_Kind;

   function Image
     (Texture : GL.Objects.Textures.Texture;
      Level   : GL.Objects.Textures.Mipmap_Level := 0) return String
   is
      Width  : constant String := Orka.Strings.Trim (Texture.Width  (Level)'Image);
      Height : constant String := Orka.Strings.Trim (Texture.Height (Level)'Image);
      Depth  : constant String := Orka.Strings.Trim (Texture.Depth  (Level)'Image);

      function U (Value : Wide_Wide_String) return String renames Orka.Strings.Unicode;
   begin
      return (if Texture.Allocated then "" else "unallocated ") &
        Width & U (" × ") & Height & U (" × ") & Depth & " " & Texture.Kind'Image &
        " with " &
        (if Texture.Compressed then
           Texture.Compressed_Format'Image
         else
           Texture.Internal_Format'Image) & " format";
   end Image;

end Orka.Rendering.Textures;
