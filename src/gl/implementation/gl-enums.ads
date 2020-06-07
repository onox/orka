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

with GL.Low_Level;

private package GL.Enums is
   pragma Preelaborate;

   type Shader_Param is (Shader_Type, Delete_Status, Compile_Status,
                         Info_Log_Length, Shader_Source_Length);

   type Program_Param is (Program_Binary_Retrievable_Hint, Program_Separable,
                          Compute_Work_Group_Size, Program_Binary_Length,
                          Geometry_Vertices_Out,
                          Geometry_Input_Type, Geometry_Output_Type,
                          Active_Uniform_Block_Max_Name_Length,
                          Active_Uniform_Blocks, Delete_Status,
                          Link_Status, Validate_Status, Info_Log_Length,
                          Attached_Shaders,
                          Active_Uniforms, Active_Uniform_Max_Length,
                          Active_Attributes, Active_Attribute_Max_Length,
                          Active_Atomic_Counter_Buffers);

   type Program_Set_Param is (Program_Binary_Retrievable_Hint, Program_Separable);

   type Program_Pipeline_Param is (Active_Program,
                                   Fragment_Shader, Vertex_Shader,
                                   Validate_Status, Info_Log_Length,
                                   Geometry_Shader,
                                   Tess_Evaluation_Shader, Tess_Control_Shader);

   type Program_Interface is (Atomic_Counter_Buffer,
                              Uniform,
                              Uniform_Block,
                              Program_Input,
                              Program_Output,
                              Buffer_Variable,
                              Shader_Storage_Block,
                              Vertex_Subroutine,
                              Tess_Control_Subroutine,
                              Tess_Evaluation_Subroutine,
                              Geometry_Subroutine,
                              Fragment_Subroutine,
                              Compute_Subroutine,
                              Vertex_Subroutine_Uniform,
                              Tess_Control_Subroutine_Uniform,
                              Tess_Evaluation_Subroutine_Uniform,
                              Geometry_Subroutine_Uniform,
                              Fragment_Subroutine_Uniform,
                              Compute_Subroutine_Uniform);

   type Program_Interface_Param is (Active_Resources, Max_Name_Length,
                                    Max_Num_Active_Variables, Max_Num_Compatible_Subroutines);

   type Program_Resource_Param is (Num_Compatible_Subroutines,
                                   Compatible_Subroutines,
                                   Is_Per_Patch,
                                   Name_Length, Resource_Type,
                                   Array_Size, Offset, Block_Index,
                                   Array_Stride, Matrix_Stride, Is_Row_Major,
                                   Atomic_Counter_Buffer_Index,
                                   Buffer_Binding, Buffer_Data_Size,
                                   Num_Active_Variables, Active_Variables,
                                   Referenced_By_Vertex_Shader,
                                   Referenced_By_Tess_Control_Shader,
                                   Referenced_By_Tess_Evaluation_Shader,
                                   Referenced_By_Geometry_Shader,
                                   Referenced_By_Fragment_Shader,
                                   Referenced_By_Compute_Shader,
                                   Top_Level_Array_Size,
                                   Top_Level_Array_Stride,
                                   Location, Location_Index);

   type Program_Resource_Array is array (Positive range <>) of Program_Resource_Param;

   type Pixel_Store_Param is (Unpack_Alignment, Pack_Alignment,
                              Unpack_Compressed_Block_Width, Unpack_Compressed_Block_Height,
                              Unpack_Compressed_Block_Depth, Unpack_Compressed_Block_Size,
                              Pack_Compressed_Block_Width, Pack_Compressed_Block_Height,
                              Pack_Compressed_Block_Depth, Pack_Compressed_Block_Size);
   --  Table 8.1 and 18.1 of the OpenGL specification
   
   type Framebuffer_Param is (Default_Width, Default_Height, Default_Layers,
                              Default_Samples, Default_Fixed_Sample_Locations);

   -----------------------------------------------------------------------------

   type Framebuffer_Kind is (Read, Draw);

   type Buffer_Kind is (Parameter_Buffer, Element_Array_Buffer,
                        Pixel_Pack_Buffer, Pixel_Unpack_Buffer, Uniform_Buffer,
                        Draw_Indirect_Buffer,
                        Shader_Storage_Buffer, Dispatch_Indirect_Buffer,
                        Query_Buffer, Atomic_Counter_Buffer);

   type Only_Depth_Buffer is (Depth_Buffer);
   type Only_Stencil_Buffer is (Stencil_Buffer);
   type Only_Depth_Stencil_Buffer is (Depth_Stencil_Buffer);
   type Only_Color_Buffer is (Color_Buffer);

private

   for Shader_Param use (Shader_Type          => 16#8B4F#,
                         Delete_Status        => 16#8B80#,
                         Compile_Status       => 16#8B81#,
                         Info_Log_Length      => 16#8B84#,
                         Shader_Source_Length => 16#8B88#);
   for Shader_Param'Size use Low_Level.Enum'Size;

   for Program_Param use (Program_Binary_Retrievable_Hint       => 16#8257#,
                          Program_Separable                     => 16#8258#,
                          Compute_Work_Group_Size               => 16#8267#,
                          Program_Binary_Length                 => 16#8741#,
                          Geometry_Vertices_Out                 => 16#8916#,
                          Geometry_Input_Type                   => 16#8917#,
                          Geometry_Output_Type                  => 16#8918#,
                          Active_Uniform_Block_Max_Name_Length  => 16#8A35#,
                          Active_Uniform_Blocks                 => 16#8A36#,
                          Delete_Status                         => 16#8B4F#,
                          Link_Status                           => 16#8B82#,
                          Validate_Status                       => 16#8B83#,
                          Info_Log_Length                       => 16#8B84#,
                          Attached_Shaders                      => 16#8B85#,
                          Active_Uniforms                       => 16#8B86#,
                          Active_Uniform_Max_Length             => 16#8B87#,
                          Active_Attributes                     => 16#8B89#,
                          Active_Attribute_Max_Length           => 16#8B8A#,
                          Active_Atomic_Counter_Buffers         => 16#92D9#);
   for Program_Param'Size use Low_Level.Enum'Size;

   for Program_Set_Param use (Program_Binary_Retrievable_Hint => 16#8257#,
                              Program_Separable               => 16#8258#);
   for Program_Set_Param'Size use Low_Level.Enum'Size;

   for Program_Pipeline_Param use (Active_Program         => 16#8259#,
                                   Fragment_Shader        => 16#8B30#,
                                   Vertex_Shader          => 16#8B31#,
                                   Validate_Status        => 16#8B83#,
                                   Info_Log_Length        => 16#8B84#,
                                   Geometry_Shader        => 16#8DD9#,
                                   Tess_Evaluation_Shader => 16#8E87#,
                                   Tess_Control_Shader    => 16#8E88#);
   for Program_Pipeline_Param'Size use Low_Level.Enum'Size;

   for Program_Interface use (Atomic_Counter_Buffer              => 16#92C0#,
                              Uniform                            => 16#92E1#,
                              Uniform_Block                      => 16#92E2#,
                              Program_Input                      => 16#92E3#,
                              Program_Output                     => 16#92E4#,
                              Buffer_Variable                    => 16#92E5#,
                              Shader_Storage_Block               => 16#92E6#,
                              Vertex_Subroutine                  => 16#92E8#,
                              Tess_Control_Subroutine            => 16#92E9#,
                              Tess_Evaluation_Subroutine         => 16#92EA#,
                              Geometry_Subroutine                => 16#92EB#,
                              Fragment_Subroutine                => 16#92EC#,
                              Compute_Subroutine                 => 16#92ED#,
                              Vertex_Subroutine_Uniform          => 16#92EE#,
                              Tess_Control_Subroutine_Uniform    => 16#92EF#,
                              Tess_Evaluation_Subroutine_Uniform => 16#92F0#,
                              Geometry_Subroutine_Uniform        => 16#92F1#,
                              Fragment_Subroutine_Uniform        => 16#92F2#,
                              Compute_Subroutine_Uniform         => 16#92F3#);
   for Program_Interface'Size use Low_Level.Enum'Size;

   for Program_Interface_Param use (Active_Resources               => 16#92F5#,
                                    Max_Name_Length                => 16#92F6#,
                                    Max_Num_Active_Variables       => 16#92F7#,
                                    Max_Num_Compatible_Subroutines => 16#92F8#);
   for Program_Interface_Param'Size use Low_Level.Enum'Size;

   for Program_Resource_Param use (Num_Compatible_Subroutines           => 16#8E4A#,
                                   Compatible_Subroutines               => 16#8E4B#,
                                   Is_Per_Patch                         => 16#92E7#,
                                   Name_Length                          => 16#92F9#,
                                   Resource_Type                        => 16#92FA#,
                                   Array_Size                           => 16#92FB#,
                                   Offset                               => 16#92FC#,
                                   Block_Index                          => 16#92FD#,
                                   Array_Stride                         => 16#92FE#,
                                   Matrix_Stride                        => 16#92FF#,
                                   Is_Row_Major                         => 16#9300#,
                                   Atomic_Counter_Buffer_Index          => 16#9301#,
                                   Buffer_Binding                       => 16#9302#,
                                   Buffer_Data_Size                     => 16#9303#,
                                   Num_Active_Variables                 => 16#9304#,
                                   Active_Variables                     => 16#9305#,
                                   Referenced_By_Vertex_Shader          => 16#9306#,
                                   Referenced_By_Tess_Control_Shader    => 16#9307#,
                                   Referenced_By_Tess_Evaluation_Shader => 16#9308#,
                                   Referenced_By_Geometry_Shader        => 16#9309#,
                                   Referenced_By_Fragment_Shader        => 16#930A#,
                                   Referenced_By_Compute_Shader         => 16#930B#,
                                   Top_Level_Array_Size                 => 16#930C#,
                                   Top_Level_Array_Stride               => 16#930D#,
                                   Location                             => 16#930E#,
                                   Location_Index                       => 16#930F#);
   for Program_Resource_Param'Size use Low_Level.Enum'Size;

   for Pixel_Store_Param use (Unpack_Alignment    => 16#0CF5#,
                              Pack_Alignment      => 16#0D05#,
                              Unpack_Compressed_Block_Width  => 16#9127#,
                              Unpack_Compressed_Block_Height => 16#9128#,
                              Unpack_Compressed_Block_Depth  => 16#9129#,
                              Unpack_Compressed_Block_Size   => 16#912A#,
                              Pack_Compressed_Block_Width  => 16#912B#,
                              Pack_Compressed_Block_Height => 16#912C#,
                              Pack_Compressed_Block_Depth  => 16#912D#,
                              Pack_Compressed_Block_Size   => 16#912E#);
   for Pixel_Store_Param'Size use Low_Level.Enum'Size;

   for Framebuffer_Param use (Default_Width                  => 16#9310#,
                              Default_Height                 => 16#9311#,
                              Default_Layers                 => 16#9312#,
                              Default_Samples                => 16#9313#,
                              Default_Fixed_Sample_Locations => 16#9314#);
   for Framebuffer_Param'Size use Low_Level.Enum'Size;

   -----------------------------------------------------------------------------

   for Framebuffer_Kind use (Read => 16#8CA8#,
                             Draw => 16#8CA9#);
   for Framebuffer_Kind'Size use Low_Level.Enum'Size;

   for Buffer_Kind use (Parameter_Buffer          => 16#80EE#,
                        Element_Array_Buffer      => 16#8893#,
                        Pixel_Pack_Buffer         => 16#88EB#,
                        Pixel_Unpack_Buffer       => 16#88EC#,
                        Uniform_Buffer            => 16#8A11#,
                        Draw_Indirect_Buffer      => 16#8F3F#,
                        Shader_Storage_Buffer     => 16#90D2#,
                        Dispatch_Indirect_Buffer  => 16#90EE#,
                        Query_Buffer              => 16#9192#,
                        Atomic_Counter_Buffer     => 16#92C0#);
   for Buffer_Kind'Size use Low_Level.Enum'Size;

   for Only_Depth_Buffer use (Depth_Buffer => 16#1801#);
   for Only_Depth_Buffer'Size use Low_Level.Enum'Size;

   for Only_Stencil_Buffer use (Stencil_Buffer => 16#1802#);
   for Only_Stencil_Buffer'Size use Low_Level.Enum'Size;

   for Only_Depth_Stencil_Buffer use (Depth_Stencil_Buffer => 16#84F9#);
   for Only_Depth_Stencil_Buffer'Size use Low_Level.Enum'Size;

   for Only_Color_Buffer use (Color_Buffer => 16#1800#);
   for Only_Color_Buffer'Size use Low_Level.Enum'Size;

end GL.Enums;
