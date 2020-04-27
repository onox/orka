--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2013 Felix Krause <contact@flyx.org>
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

with GL.Buffers;
with GL.Low_Level.Enums;
with GL.Objects.Textures;
with GL.Pixels.Extensions;
with GL.Types.Colors;

private with GL.Enums;

package GL.Objects.Framebuffers is
   pragma Preelaborate;

   use all type GL.Low_Level.Enums.Texture_Kind;

   type Framebuffer_Status is (Undefined, Complete, Incomplete_Attachment,
                               Incomplete_Missing_Attachment,
                               Incomplete_Draw_Buffer, Incomplete_Read_Buffer,
                               Unsupported, Incomplete_Multisample,
                               Incomplete_Layer_Targets);

   type Attachment_Point is (Depth_Stencil_Attachment,
                             Color_Attachment_0, Color_Attachment_1,
                             Color_Attachment_2, Color_Attachment_3,
                             Color_Attachment_4, Color_Attachment_5,
                             Color_Attachment_6, Color_Attachment_7,
                             Color_Attachment_8, Color_Attachment_9,
                             Color_Attachment_10, Color_Attachment_11,
                             Color_Attachment_12, Color_Attachment_13,
                             Color_Attachment_14, Color_Attachment_15,
                             Depth_Attachment, Stencil_Attachment);

   type Default_Attachment_Point is
     (Front_Left, Front_Right, Back_Left, Back_Right, Depth, Stencil);

   type Attachment_List is array (Positive range <>) of Attachment_Point;

   type Default_Attachment_List is array (Positive range <>) of Default_Attachment_Point;

   function Valid_Attachment
     (Attachment : Attachment_Point;
      Texture    : Textures.Texture) return Boolean
   with Pre => not Texture.Compressed and Texture.Allocated;

   type Framebuffer is new GL_Object with private;

   overriding
   procedure Initialize_Id (Object : in out Framebuffer);

   overriding
   procedure Delete_Id (Object : in out Framebuffer);

   overriding
   function Identifier (Object : Framebuffer) return Types.Debug.Identifier is
     (Types.Debug.Framebuffer);

   procedure Set_Draw_Buffer
     (Object   : Framebuffer;
      Selector : Buffers.Color_Buffer_Selector)
   with Pre => (if Object = Default_Framebuffer then
                  Selector in Buffers.Default_Color_Buffer_Selector | Buffers.None
                else
                  Selector in Buffers.Explicit_Color_Buffer_Selector | Buffers.None);

   procedure Set_Draw_Buffers
     (Object : Framebuffer;
      List   : Buffers.Color_Buffer_List)
   with Pre => (if Object = Default_Framebuffer then
                  (for all S of List =>
                     S in Buffers.Default_Color_Buffer_Selector | Buffers.None)
                else
                  (for all S of List =>
                     S in Buffers.Explicit_Color_Buffer_Selector | Buffers.None));

   procedure Set_Read_Buffer (Object : Framebuffer; Selector : Buffers.Color_Buffer_Selector)
     with Pre => (if Object = Default_Framebuffer then
                    Selector in Buffers.Default_Color_Buffer_Selector | Buffers.None
                  else
                    Selector in Buffers.Explicit_Color_Buffer_Selector | Buffers.None);

   procedure Attach_Texture
     (Object     : Framebuffer;
      Attachment : Attachment_Point;
      Texture    : Textures.Texture;
      Level      : Textures.Mipmap_Level)
   with Pre => Object /= Default_Framebuffer and
                 Valid_Attachment (Attachment, Texture) and
                 (if Texture.Kind in
                    Texture_2D_Multisample | Texture_2D_Multisample_Array
                  then Level = 0);

   procedure Attach_Texture_Layer
     (Object     : Framebuffer;
      Attachment : Attachment_Point;
      Texture    : Textures.Texture;
      Level      : Textures.Mipmap_Level;
      Layer      : Natural)
   with Pre => Object /= Default_Framebuffer and
                 Valid_Attachment (Attachment, Texture) and
                 Texture.Layered;

   procedure Detach (Object : Framebuffer; Attachment : Attachment_Point)
     with Pre => Object /= Default_Framebuffer;

   procedure Invalidate_Data
     (Object      : Framebuffer;
      Attachments : Attachment_List)
   with Pre => Object /= Default_Framebuffer;

   procedure Invalidate_Sub_Data
     (Object        : Framebuffer;
      Attachments   : Attachment_List;
      X, Y          : Int;
      Width, Height : Size)
   with Pre => Object /= Default_Framebuffer;

   procedure Invalidate_Data
     (Object      : Framebuffer;
      Attachments : Default_Attachment_List)
   with Pre => Object = Default_Framebuffer;

   procedure Invalidate_Sub_Data
     (Object        : Framebuffer;
      Attachments   : Default_Attachment_List;
      X, Y          : Int;
      Width, Height : Size)
   with Pre => Object = Default_Framebuffer;

   procedure Set_Default_Width   (Object : Framebuffer; Value : Size);
   procedure Set_Default_Height  (Object : Framebuffer; Value : Size);
   procedure Set_Default_Layers  (Object : Framebuffer; Value : Size);
   procedure Set_Default_Samples (Object : Framebuffer; Value : Size);

   function Default_Width   (Object : Framebuffer) return Size;
   function Default_Height  (Object : Framebuffer) return Size;
   function Default_Layers  (Object : Framebuffer) return Size;
   function Default_Samples (Object : Framebuffer) return Size;

   function Max_Framebuffer_Width return Size
     with Post => Max_Framebuffer_Width'Result >= 16_384;

   function Max_Framebuffer_Height return Size
     with Post => Max_Framebuffer_Height'Result >= 16_384;

   function Max_Framebuffer_Layers return Size
     with Post => Max_Framebuffer_Layers'Result >= 2_048;

   function Max_Framebuffer_Samples return Size
     with Post => Max_Framebuffer_Samples'Result >= 4;

   procedure Set_Default_Fixed_Sample_Locations (Object : Framebuffer; Value : Boolean);

   function Default_Fixed_Sample_Locations (Object : Framebuffer) return Boolean;

   procedure Blit (Read_Object, Draw_Object : Framebuffer;
                   Src_X0, Src_Y0, Src_X1, Src_Y1,
                   Dst_X0, Dst_Y0, Dst_X1, Dst_Y1 : Int;
                   Mask : Buffers.Buffer_Bits;
                   Filter : Textures.Magnifying_Function);
   --  Copy a rectangle of pixels in Read_Object framebuffer to a region
   --  in Draw_Object framebuffer

   procedure Clear_Color_Buffer
     (Object      : Framebuffer;
      Index       : Buffers.Draw_Buffer_Index;
      Format_Type : Pixels.Extensions.Format_Type;
      Value       : Colors.Color);

   procedure Clear_Depth_Buffer   (Object : Framebuffer; Value : Buffers.Depth);

   procedure Clear_Stencil_Buffer (Object : Framebuffer; Value : Buffers.Stencil_Index);

   procedure Clear_Depth_And_Stencil_Buffer (Object : Framebuffer;
                                             Depth_Value   : Buffers.Depth;
                                             Stencil_Value : Buffers.Stencil_Index);

   type Framebuffer_Target (<>) is tagged limited private;

   procedure Bind (Target : Framebuffer_Target;
                   Object : Framebuffer'Class);

   function Status
     (Object : Framebuffer;
      Target : Framebuffer_Target'Class) return Framebuffer_Status;

   Read_Target  : constant Framebuffer_Target;
   Draw_Target  : constant Framebuffer_Target;

   function Default_Framebuffer return Framebuffer;

private

   for Framebuffer_Status use (Undefined                     => 16#8219#,
                               Complete                      => 16#8CD5#,
                               Incomplete_Attachment         => 16#8CD6#,
                               Incomplete_Missing_Attachment => 16#8CD7#,
                               Incomplete_Draw_Buffer        => 16#8CDB#,
                               Incomplete_Read_Buffer        => 16#8CDC#,
                               Unsupported                   => 16#8CDD#,
                               Incomplete_Multisample        => 16#8D56#,
                               Incomplete_Layer_Targets      => 16#8DA8#);
   for Framebuffer_Status'Size use Low_Level.Enum'Size;

   for Attachment_Point use (Depth_Stencil_Attachment => 16#821A#,
                             Color_Attachment_0       => 16#8CE0#,
                             Color_Attachment_1       => 16#8CE1#,
                             Color_Attachment_2       => 16#8CE2#,
                             Color_Attachment_3       => 16#8CE3#,
                             Color_Attachment_4       => 16#8CE4#,
                             Color_Attachment_5       => 16#8CE5#,
                             Color_Attachment_6       => 16#8CE6#,
                             Color_Attachment_7       => 16#8CE7#,
                             Color_Attachment_8       => 16#8CE8#,
                             Color_Attachment_9       => 16#8CE9#,
                             Color_Attachment_10      => 16#8CEA#,
                             Color_Attachment_11      => 16#8CEB#,
                             Color_Attachment_12      => 16#8CEC#,
                             Color_Attachment_13      => 16#8CED#,
                             Color_Attachment_14      => 16#8CEE#,
                             Color_Attachment_15      => 16#8CEF#,
                             Depth_Attachment         => 16#8D00#,
                             Stencil_Attachment       => 16#8D20#);
   for Attachment_Point'Size use Low_Level.Enum'Size;

   for Default_Attachment_Point use
     (Front_Left  => 16#0400#,
      Front_Right => 16#0401#,
      Back_Left   => 16#0402#,
      Back_Right  => 16#0403#,
      Depth       => 16#1801#,
      Stencil     => 16#1802#);
   for Default_Attachment_Point'Size use Low_Level.Enum'Size;

   pragma Convention (C, Attachment_List);
   pragma Convention (C, Default_Attachment_List);

   type Framebuffer is new GL_Object with null record;

   type Framebuffer_Target (Kind : Enums.Framebuffer_Kind) is
     tagged limited null record;

   Read_Target  : constant Framebuffer_Target :=
     Framebuffer_Target'(Kind => Enums.Read);
   Draw_Target : constant Framebuffer_Target :=
     Framebuffer_Target'(Kind => Enums.Draw);

end GL.Objects.Framebuffers;
