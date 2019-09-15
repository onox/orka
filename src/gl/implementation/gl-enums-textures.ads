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

package GL.Enums.Textures is
   pragma Preelaborate;

   -- Texture_Kind is declared in GL.Low_Level.Enums to be accessible for
   -- OpenCLAda

   type Parameter is (Border_Color, Mag_Filter, Min_Filter, Wrap_S,
                      Wrap_T, Wrap_R, Min_LoD, Max_LoD,
                      Base_Level, Max_Level, Immutable_Levels, Max_Anisotropy,
                      LoD_Bias, Compare_Mode, Compare_Func, Cube_Map_Seamless);

   -- needs to be declared here because of subtypes
   for Parameter use (Border_Color    => 16#1004#,
                      Mag_Filter      => 16#2800#,
                      Min_Filter      => 16#2801#,
                      Wrap_S          => 16#2802#,
                      Wrap_T          => 16#2803#,
                      Wrap_R          => 16#8072#,
                      Min_LoD         => 16#813A#,
                      Max_LoD         => 16#813B#,
                      Base_Level      => 16#813C#,
                      Max_Level       => 16#813D#,
                      Immutable_Levels => 16#82DF#,
                      Max_Anisotropy  => 16#84FE#,
                      LoD_Bias        => 16#8501#,
                      Compare_Mode    => 16#884C#,
                      Compare_Func    => 16#884D#,
                      Cube_Map_Seamless => 16#884F#);
   for Parameter'Size use Low_Level.Enum'Size;

   subtype LoD is Parameter range Min_LoD .. Max_Level;

   type Compare_Kind is (None, Compare_R_To_Texture);

   type Env_Parameter is (LoD_Bias, Src1_Alpha);

   type Level_Parameter is (Width, Height, Internal_Format, Red_Size,
                            Green_Size, Blue_Size, Alpha_Size, Depth,
                            Compressed_Image_Size, Compressed,
                            Depth_Size, Stencil_Size,
                            Red_Type, Green_Type, Blue_Type,
                            Alpha_Type, Depth_Type, Shared_Size,
                            Samples, Fixed_Sample_Locations,
                            Buffer_Offset, Buffer_Size);

   Texture_Unit_Start_Rep : constant := 16#84C0#;

private

   for Compare_Kind use (None => 0, Compare_R_To_Texture => 16#884E#);
   for Compare_Kind'Size use Low_Level.Enum'Size;

   for Env_Parameter use (LoD_Bias       => 16#8501#,
                          Src1_Alpha     => 16#8589#);
   for Env_Parameter'Size use Low_Level.Enum'Size;

   for Level_Parameter use (Width           => 16#1000#,
                            Height          => 16#1001#,
                            Internal_Format => 16#1003#,
                            Red_Size        => 16#805C#,
                            Green_Size      => 16#805D#,
                            Blue_Size       => 16#805E#,
                            Alpha_Size      => 16#805F#,
                            Depth           => 16#8071#,
                            Compressed_Image_Size => 16#86A0#,
                            Compressed      => 16#86A1#,
                            Depth_Size      => 16#884A#,
                            Stencil_Size    => 16#88F1#,
                            Red_Type        => 16#8C10#,
                            Green_Type      => 16#8C11#,
                            Blue_Type       => 16#8C12#,
                            Alpha_Type      => 16#8C13#,
                            Depth_Type      => 16#8C16#,
                            Shared_Size     => 16#8C3F#,
                            Samples         => 16#9106#,
                            Fixed_Sample_Locations => 16#9107#,
                            Buffer_Offset   => 16#919D#,
                            Buffer_Size     => 16#919E#);
   for Level_Parameter'Size use Low_Level.Enum'Size;

end GL.Enums.Textures;
