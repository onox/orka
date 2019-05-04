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

with Interfaces.C.Pointers;

package GL.Types.Colors is
   pragma Preelaborate;

   type Color_Index is (R, G, B, A);

   subtype Component is Single range 0.0 .. 1.0;

   type Color is array (Color_Index) of aliased Component;

   pragma Convention (C, Color);

   type Enabled_Color is array (Color_Index) of Boolean
     with Convention => C;

   type Color_Array is array (Size range <>) of aliased Color;

   package Color_Pointers is new Interfaces.C.Pointers
     (Size, Color, Color_Array, Color'(others => 0.0));

   -----------------------------------------------------------------------------

   type Border_Color is (Transparent_Black, Opaque_Black, Opaque_White);
   --  Table 15 of the Vulkan specification

   Vulkan_To_OpenGL : constant array (Border_Color) of Color
     := (Transparent_Black => (0.0, 0.0, 0.0, 0.0),
         Opaque_Black      => (0.0, 0.0, 0.0, 1.0),
         Opaque_White      => (1.0, 1.0, 1.0, 1.0));

   function OpenGL_To_Vulkan (Value : Color) return Border_Color is
     (if Value = Vulkan_To_OpenGL (Transparent_Black) then
         Transparent_Black
      elsif Value = Vulkan_To_OpenGL (Opaque_Black) then
         Opaque_Black
      elsif Value = Vulkan_To_OpenGL (Opaque_White) then
         Opaque_White
      else
         raise Constraint_Error);

end GL.Types.Colors;
