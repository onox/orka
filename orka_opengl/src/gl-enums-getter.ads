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

package GL.Enums.Getter is
   pragma Preelaborate;

   type Parameter is
     (Polygon_Mode,
      Unpack_Alignment,
      Pack_Alignment,
      Major_Version,
      Minor_Version,
      Num_Extensions,
      Context_Flags,
      Context_Reset_Notification,
      Max_Compute_Shared_Memory_Size,
      Num_Shading_Language_Versions,
      Context_Release_Behavior,
      Max_Texture_Max_Anisotropy,
      Timestamp,
      Min_Map_Buffer_Alignment,
      Max_Compute_Shader_Storage_Blocks,
      Max_Shader_Storage_Buffer_Bindings,
      Max_Shader_Storage_Block_Size,
      Max_Compute_Fixed_Group_Invocations,
      Unpack_Compressed_Block_Width,
      Unpack_Compressed_Block_Height,
      Unpack_Compressed_Block_Depth,
      Unpack_Compressed_Block_Size,
      Pack_Compressed_Block_Width,
      Pack_Compressed_Block_Height,
      Pack_Compressed_Block_Depth,
      Pack_Compressed_Block_Size,
      Max_Debug_Message_Length,
      Debug_Logged_Messages,
      Max_Compute_Work_Group_Count,
      Max_Compute_Fixed_Group_Size,
      Max_Framebuffer_Width,
      Max_Framebuffer_Height,
      Max_Framebuffer_Layers,
      Max_Framebuffer_Samples,
      Max_Compute_Variable_Group_Invocations,
      Max_Compute_Variable_Group_Size);

   type String_Parameter is
     (Vendor, Renderer, Version, Extensions, Shading_Language_Version);

private

   for Parameter use
     (Polygon_Mode                  => 16#0B40#,
      Unpack_Alignment              => 16#0CF5#,
      Pack_Alignment                => 16#0D05#,
      Major_Version                 => 16#821B#,
      Minor_Version                 => 16#821C#,
      Num_Extensions                => 16#821D#,
      Context_Flags                 => 16#821E#,
      Context_Reset_Notification    => 16#8256#,
      Max_Compute_Shared_Memory_Size => 16#8262#,
      Num_Shading_Language_Versions => 16#82E9#,
      Context_Release_Behavior      => 16#82FB#,
      Max_Texture_Max_Anisotropy    => 16#84FF#,
      Timestamp                     => 16#8E28#,
      Min_Map_Buffer_Alignment      => 16#90BC#,
      Max_Compute_Shader_Storage_Blocks  => 16#90DB#,
      Max_Shader_Storage_Buffer_Bindings => 16#90DD#,
      Max_Shader_Storage_Block_Size      => 16#90DE#,
      Max_Compute_Fixed_Group_Invocations => 16#90EB#,
      Unpack_Compressed_Block_Width  => 16#9127#,
      Unpack_Compressed_Block_Height => 16#9128#,
      Unpack_Compressed_Block_Depth  => 16#9129#,
      Unpack_Compressed_Block_Size   => 16#912A#,
      Pack_Compressed_Block_Width    => 16#912B#,
      Pack_Compressed_Block_Height   => 16#912C#,
      Pack_Compressed_Block_Depth    => 16#912D#,
      Pack_Compressed_Block_Size     => 16#912E#,
      Max_Debug_Message_Length      => 16#9143#,
      Debug_Logged_Messages         => 16#9145#,
      Max_Compute_Work_Group_Count  => 16#91BE#,
      Max_Compute_Fixed_Group_Size  => 16#91BF#,
      Max_Framebuffer_Width         => 16#9315#,
      Max_Framebuffer_Height        => 16#9316#,
      Max_Framebuffer_Layers        => 16#9317#,
      Max_Framebuffer_Samples       => 16#9318#,
      Max_Compute_Variable_Group_Invocations => 16#9344#,
      Max_Compute_Variable_Group_Size        => 16#9345#);
   for Parameter'Size use Low_Level.Enum'Size;

   for String_Parameter use
     (Vendor                   => 16#1F00#,
      Renderer                 => 16#1F01#,
      Version                  => 16#1F02#,
      Extensions               => 16#1F03#,
      Shading_Language_Version => 16#8B8C#);
   for String_Parameter'Size use Low_Level.Enum'Size;

end GL.Enums.Getter;
