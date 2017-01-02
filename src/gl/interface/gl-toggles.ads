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

private with GL.Low_Level;

package GL.Toggles is
   pragma Preelaborate;

   type Toggle_State is (Disabled, Enabled);

   type Toggle is (Line_Smooth, Polygon_Smooth, Cull_Face, Depth_Test,
                   Stencil_Test, Dither, Blend, Color_Logic_Op, Scissor_Test,
                   Texture_1D, Texture_2D, Polygon_Offset_Point, Polygon_Offset_Line,
                   Clip_Plane_0, Clip_Plane_1, Clip_Plane_2, Clip_Plane_3,
                   Clip_Plane_4, Clip_Plane_5,
                   Polygon_Offset_Fill, Texture_3D, Multisample,
                   Sample_Alpha_To_Coverage, Sample_Alpha_To_One, Sample_Coverage,
                   Texture_Cube_Map, Vertex_Program_Point_Size, Depth_Clamp,
                   Texture_Cube_Map_Seamless, Rasterizer_Discard);

   procedure Enable (Subject : Toggle);
   procedure Disable (Subject : Toggle);
   procedure Set (Subject : Toggle; Value : Toggle_State);
   function State (Subject : Toggle) return Toggle_State;

private

   for Toggle use (Line_Smooth               => 16#0B20#,
                   Polygon_Smooth            => 16#0B41#,
                   Cull_Face                 => 16#0B44#,
                   Depth_Test                => 16#0B71#,
                   Stencil_Test              => 16#0B90#,
                   Dither                    => 16#0BD0#,
                   Blend                     => 16#0BE2#,
                   Color_Logic_Op            => 16#0BF2#,
                   Scissor_Test              => 16#0C11#,
                   Texture_1D                => 16#0DE0#,
                   Texture_2D                => 16#0DE1#,
                   Polygon_Offset_Point      => 16#2A01#,
                   Polygon_Offset_Line       => 16#2A02#,
                   Clip_Plane_0              => 16#3000#,
                   Clip_Plane_1              => 16#3001#,
                   Clip_Plane_2              => 16#3002#,
                   Clip_Plane_3              => 16#3003#,
                   Clip_Plane_4              => 16#3004#,
                   Clip_Plane_5              => 16#3005#,
                   Polygon_Offset_Fill       => 16#8037#,
                   Texture_3D                => 16#806F#,
                   Multisample               => 16#809D#,
                   Sample_Alpha_To_Coverage  => 16#809E#,
                   Sample_Alpha_To_One       => 16#809F#,
                   Sample_Coverage           => 16#80A0#,
                   Texture_Cube_Map          => 16#8513#,
                   Vertex_Program_Point_Size => 16#8642#,
                   Depth_Clamp               => 16#864F#,
                   Texture_Cube_Map_Seamless => 16#884F#,
                   Rasterizer_Discard        => 16#8C89#);
   for Toggle'Size use Low_Level.Enum'Size;

end GL.Toggles;
