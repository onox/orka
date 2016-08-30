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

   type Parameter is (Border_Color, Border, Mag_Filter, Min_Filter, Wrap_S,
                      Wrap_T, Priority, Resident, Wrap_R, Min_LoD, Max_LoD,
                      Base_Level, Max_Level, Generate_Mipmap, Max_Anisotropy,
                      LoD_Bias, Depth, Compare_Mode, Compare_Func);

   -- needs to be declared here because of subtypes
   for Parameter use (Border_Color    => 16#1003#,
                      Border          => 16#1004#,
                      Mag_Filter      => 16#2800#,
                      Min_Filter      => 16#2801#,
                      Wrap_S          => 16#2802#,
                      Wrap_T          => 16#2803#,
                      Priority        => 16#8066#,
                      Resident        => 16#8067#,
                      Wrap_R          => 16#8072#,
                      Min_LoD         => 16#813A#,
                      Max_LoD         => 16#813B#,
                      Base_Level      => 16#813C#,
                      Max_Level       => 16#813D#,
                      Generate_Mipmap => 16#8191#,
                      Max_Anisotropy  => 16#84FE#,
                      LoD_Bias        => 16#8501#,
                      Depth           => 16#884B#,
                      Compare_Mode    => 16#884C#,
                      Compare_Func    => 16#884D#);
   for Parameter'Size use Low_Level.Enum'Size;

   subtype LoD is Parameter range Min_LoD .. Max_Level;

   type Compare_Kind is (None, Compare_R_To_Texture);

   type Env_Target is (Texture_Env, Filter_Control, Point_Sprite);

   type Env_Parameter is (Alpha_Scale, Env_Mode, Env_Color, LoD_Bias, Combine_RGB,
                          Combine_Alpha, RGB_Scale, Src0_RGB, Src1_RGB, Src2_RGB,
                          Src0_Alpha, Src1_Alpha, Src2_Alpha, Operand0_RGB,
                          Operand1_RGB, Operand2_RGB, Operand0_Alpha,
                          Operand1_Alpha, Operand2_Alpha, Coord_Replace);
   
   type Level_Parameter is (Width, Height, Internal_Format, Red_Size,
                            Green_Size, Blue_Size, Alpha_Size, Luminance_Size,
                            Intensity_Size,  Compressed_Image_Size, Compressed,
                            Depth_Size, Red_Type, Green_Type, Blue_Type,
                            Alpha_Type, Depth_Type, Buffer_Offset, Buffer_Size);
   
   Texture_Unit_Start_Rep : constant := 16#84C0#;
   
private

   for Compare_Kind use (None => 0, Compare_R_To_Texture => 16#884E#);
   for Compare_Kind'Size use Low_Level.Enum'Size;

   for Env_Target use (Texture_Env    => 16#2300#,
                       Filter_Control => 16#8500#,
                       Point_Sprite   => 16#8861#);
   for Env_Target'Size use Low_Level.Enum'Size;

   for Env_Parameter use (Alpha_Scale    => 16#0D1C#,
                          Env_Mode       => 16#2200#,
                          Env_Color      => 16#2201#,
                          LoD_Bias       => 16#8501#,
                          Combine_RGB    => 16#8571#,
                          Combine_Alpha  => 16#8572#,
                          RGB_Scale      => 16#8573#,
                          Src0_RGB       => 16#8580#,
                          Src1_RGB       => 16#8581#,
                          Src2_RGB       => 16#8582#,
                          Src0_Alpha     => 16#8588#,
                          Src1_Alpha     => 16#8589#,
                          Src2_Alpha     => 16#858A#,
                          Operand0_RGB   => 16#8590#,
                          Operand1_RGB   => 16#8591#,
                          Operand2_RGB   => 16#8592#,
                          Operand0_Alpha => 16#8598#,
                          Operand1_Alpha => 16#8599#,
                          Operand2_Alpha => 16#859A#,
                          Coord_Replace  => 16#8862#);
   for Env_Parameter'Size use Low_Level.Enum'Size;
   
   for Level_Parameter use (Width           => 16#1000#,
                            Height          => 16#1001#,
                            Internal_Format => 16#1002#,
                            Red_Size        => 16#805C#,
                            Green_Size      => 16#805D#,
                            Blue_Size       => 16#805E#,
                            Alpha_Size      => 16#805F#,
                            Luminance_Size  => 16#8060#,
                            Intensity_Size  => 16#8061#,
                            Compressed_Image_Size => 16#86A0#,
                            Compressed      => 16#86A1#,
                            Depth_Size      => 16#884A#,
                            Red_Type        => 16#8C10#,
                            Green_Type      => 16#8C11#,
                            Blue_Type       => 16#8C12#,
                            Alpha_Type      => 16#8C13#,
                            Depth_Type      => 16#8C14#,
                            Buffer_Offset   => 16#919D#,
                            Buffer_Size     => 16#919E#);
   for Level_Parameter'Size use Low_Level.Enum'Size;
   
end GL.Enums.Textures;
