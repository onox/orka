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

private with GL.Low_Level;
with GL.Types;

package GL.Toggles is
   pragma Preelaborate;

   type Toggle_State is (Disabled, Enabled);

   type Toggle is (Line_Smooth, Polygon_Smooth, Cull_Face, Depth_Test,
                   Stencil_Test, Dither, Blend, Color_Logic_Op, Scissor_Test,
                   Polygon_Offset_Point, Polygon_Offset_Line,
                   Clip_Distance_0, Clip_Distance_1, Clip_Distance_2, Clip_Distance_3,
                   Clip_Distance_4, Clip_Distance_5, Clip_Distance_6, Clip_Distance_7,
                   Polygon_Offset_Fill, Multisample,
                   Sample_Alpha_To_Coverage, Sample_Alpha_To_One, Sample_Coverage,
                   Debug_Output_Synchronous,
                   Program_Point_Size, Depth_Clamp,
                   Texture_Cube_Map_Seamless, Sample_Shading,
                   Rasterizer_Discard, Primitive_Restart_Fixed_Index,
                   Framebuffer_SRGB, Sample_Mask, Primitive_Restart,
                   Debug_Output);

   procedure Enable  (Subject : Toggle);
   procedure Disable (Subject : Toggle);

   procedure Set (Subject : Toggle; Value : Toggle_State);
   function State (Subject : Toggle) return Toggle_State;

   type Toggle_Indexed is (Blend, Scissor_Test);

   procedure Enable  (Subject : Toggle_Indexed; Index : Types.UInt);
   procedure Disable (Subject : Toggle_Indexed; Index : Types.UInt);

   procedure Set (Subject : Toggle_Indexed; Index : Types.UInt; Value : Toggle_State);
   function State (Subject : Toggle_Indexed; Index : Types.UInt) return Toggle_State;

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
                   Polygon_Offset_Point      => 16#2A01#,
                   Polygon_Offset_Line       => 16#2A02#,
                   Clip_Distance_0           => 16#3000#,
                   Clip_Distance_1           => 16#3001#,
                   Clip_Distance_2           => 16#3002#,
                   Clip_Distance_3           => 16#3003#,
                   Clip_Distance_4           => 16#3004#,
                   Clip_Distance_5           => 16#3005#,
                   Clip_Distance_6           => 16#3006#,
                   Clip_Distance_7           => 16#3007#,
                   Polygon_Offset_Fill       => 16#8037#,
                   Multisample               => 16#809D#,
                   Sample_Alpha_To_Coverage  => 16#809E#,
                   Sample_Alpha_To_One       => 16#809F#,
                   Sample_Coverage           => 16#80A0#,
                   Debug_Output_Synchronous  => 16#8242#,
                   Program_Point_Size        => 16#8642#,
                   Depth_Clamp               => 16#864F#,
                   Texture_Cube_Map_Seamless => 16#884F#,
                   Sample_Shading            => 16#8C36#,
                   Rasterizer_Discard        => 16#8C89#,
                   Primitive_Restart_Fixed_Index => 16#8D69#,
                   Framebuffer_SRGB          => 16#8DB9#,
                   Sample_Mask               => 16#8E51#,
                   Primitive_Restart         => 16#8F9D#,
                   Debug_Output              => 16#92E0#);
   for Toggle'Size use Low_Level.Enum'Size;

   for Toggle_Indexed use
     (Blend        => 16#0BE2#,
      Scissor_Test => 16#0C11#);
   for Toggle_Indexed'Size use Low_Level.Enum'Size;

end GL.Toggles;
