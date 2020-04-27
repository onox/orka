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

with Ada.Unchecked_Conversion;

with GL.API;
with GL.Enums.Getter;
with GL.Pixels.Queries;

package body GL.Objects.Framebuffers is

   function Valid_Attachment
     (Attachment : Attachment_Point;
      Texture    : Textures.Texture) return Boolean
   is
      Format : GL.Pixels.Internal_Format renames Texture.Internal_Format;
   begin
      case Attachment is
         when Depth_Stencil_Attachment =>
            return GL.Pixels.Extensions.Depth_Stencil_Format (Format);
         when Depth_Attachment =>
            return GL.Pixels.Extensions.Depth_Format (Format);
         when Stencil_Attachment =>
            return GL.Pixels.Extensions.Stencil_Format (Format);
         when others =>
            return GL.Pixels.Queries.Color_Renderable (Format, Texture.Kind);
      end case;
   end Valid_Attachment;

   function Status
     (Object : Framebuffer;
      Target : Framebuffer_Target'Class) return Framebuffer_Status is
   begin
      return API.Check_Named_Framebuffer_Status (Object.Reference.GL_Id, Target.Kind);
   end Status;

   procedure Set_Draw_Buffer
     (Object   : Framebuffer;
      Selector : Buffers.Color_Buffer_Selector)
   is
      subtype Index_Type is Buffers.Draw_Buffer_Index;
   begin
      Object.Set_Draw_Buffers
        ((Index_Type'First => Selector,
          Index_Type'First + 1 .. Index_Type'Last => Buffers.None));
   end Set_Draw_Buffer;

   procedure Set_Draw_Buffers
     (Object : Framebuffer;
      List   : Buffers.Color_Buffer_List) is
   begin
      API.Named_Framebuffer_Draw_Buffers (Object.Reference.GL_Id, List'Length, List);
   end Set_Draw_Buffers;

   procedure Set_Read_Buffer (Object : Framebuffer; Selector : Buffers.Color_Buffer_Selector) is
   begin
      API.Named_Framebuffer_Read_Buffer (Object.Reference.GL_Id, Selector);
   end Set_Read_Buffer;

   procedure Attach_Texture
     (Object     : Framebuffer;
      Attachment : Attachment_Point;
      Texture    : Textures.Texture;
      Level      : Textures.Mipmap_Level) is
   begin
      API.Named_Framebuffer_Texture (Object.Reference.GL_Id, Attachment,
                                     Texture.Raw_Id, Level);
   end Attach_Texture;

   procedure Attach_Texture_Layer
     (Object     : Framebuffer;
      Attachment : Attachment_Point;
      Texture    : Textures.Texture;
      Level      : Textures.Mipmap_Level;
      Layer      : Natural) is
   begin
      API.Named_Framebuffer_Texture_Layer (Object.Reference.GL_Id, Attachment,
                                           Texture.Raw_Id, Level, Int (Layer));
   end Attach_Texture_Layer;

   procedure Detach (Object : Framebuffer; Attachment : Attachment_Point) is
   begin
      API.Named_Framebuffer_Texture (Object.Reference.GL_Id, Attachment, 0, 0);
   end Detach;

   procedure Invalidate_Data
     (Object      : Framebuffer;
      Attachments : Attachment_List) is
   begin
      API.Invalidate_Named_Framebuffer_Data
        (Object.Reference.GL_Id, Attachments'Length, Attachments);
   end Invalidate_Data;

   procedure Invalidate_Sub_Data
     (Object        : Framebuffer;
      Attachments   : Attachment_List;
      X, Y          : Int;
      Width, Height : Size) is
   begin
      API.Invalidate_Named_Framebuffer_Sub_Data
        (Object.Reference.GL_Id, Attachments'Length, Attachments, X, Y, Width, Height);
   end Invalidate_Sub_Data;

   procedure Invalidate_Data
     (Object : Framebuffer;
      Attachments : Default_Attachment_List) is
   begin
      API.Invalidate_Named_Framebuffer_Data
        (Object.Reference.GL_Id, Attachments'Length, Attachments);
   end Invalidate_Data;

   procedure Invalidate_Sub_Data
     (Object        : Framebuffer;
      Attachments   : Default_Attachment_List;
      X, Y          : Int;
      Width, Height : Size) is
   begin
      API.Invalidate_Named_Framebuffer_Sub_Data
        (Object.Reference.GL_Id, Attachments'Length, Attachments, X, Y, Width, Height);
   end Invalidate_Sub_Data;

   procedure Blit (Read_Object, Draw_Object : Framebuffer;
                   Src_X0, Src_Y0, Src_X1, Src_Y1,
                   Dst_X0, Dst_Y0, Dst_X1, Dst_Y1 : Int;
                   Mask : Buffers.Buffer_Bits;
                   Filter : Textures.Magnifying_Function)
   is
      use type Low_Level.Bitfield;

      function Convert is new Ada.Unchecked_Conversion
        (Buffers.Buffer_Bits, Low_Level.Bitfield);

      Raw_Bits : constant Low_Level.Bitfield
        := Convert (Mask) and 2#0100010100000000#;
   begin
      API.Blit_Named_Framebuffer (Read_Object.Reference.GL_Id,
                                  Draw_Object.Reference.GL_Id,
                                  Src_X0, Src_Y0, Src_X1, Src_Y1,
                                  Dst_X0, Dst_Y0, Dst_X1, Dst_Y1,
                                  Raw_Bits, Filter);
   end Blit;

   procedure Set_Default_Width (Object : Framebuffer; Value : Size) is
   begin
      API.Named_Framebuffer_Parameter_Size (Object.Reference.GL_Id,
                                            Enums.Default_Width, Value);
   end Set_Default_Width;

   function Default_Width (Object : Framebuffer) return Size is
      Ret : aliased Size;
   begin
      API.Get_Named_Framebuffer_Parameter_Size
        (Object.Reference.GL_Id, Enums.Default_Width, Ret'Unchecked_Access);
      return Ret;
   end Default_Width;

   function Max_Framebuffer_Width return Size is
      Ret : Size := 16_384;
   begin
      API.Get_Size (Enums.Getter.Max_Framebuffer_Width, Ret);
      return Ret;
   end Max_Framebuffer_Width;

   procedure Set_Default_Height (Object : Framebuffer; Value : Size) is
   begin
      API.Named_Framebuffer_Parameter_Size (Object.Reference.GL_Id,
                                            Enums.Default_Height,
                                            Value);
   end Set_Default_Height;

   function Default_Height (Object : Framebuffer) return Size is
      Ret : aliased Size;
   begin
      API.Get_Named_Framebuffer_Parameter_Size
        (Object.Reference.GL_Id, Enums.Default_Height, Ret'Unchecked_Access);
      return Ret;
   end Default_Height;

   function Max_Framebuffer_Height return Size is
      Ret : Size := 16_384;
   begin
      API.Get_Size (Enums.Getter.Max_Framebuffer_Height, Ret);
      return Ret;
   end Max_Framebuffer_Height;

   procedure Set_Default_Layers (Object : Framebuffer; Value : Size) is
   begin
      API.Named_Framebuffer_Parameter_Size (Object.Reference.GL_Id,
                                            Enums.Default_Layers, Value);
   end Set_Default_Layers;

   function Default_Layers (Object : Framebuffer) return Size is
      Ret : aliased Size;
   begin
      API.Get_Named_Framebuffer_Parameter_Size
        (Object.Reference.GL_Id, Enums.Default_Layers, Ret'Unchecked_Access);
      return Ret;
   end Default_Layers;

   function Max_Framebuffer_Layers return Size is
      Ret : Size := 2_048;
   begin
      API.Get_Size (Enums.Getter.Max_Framebuffer_Layers, Ret);
      return Ret;
   end Max_Framebuffer_Layers;

   procedure Set_Default_Samples (Object : Framebuffer; Value : Size) is
   begin
      API.Named_Framebuffer_Parameter_Size (Object.Reference.GL_Id,
                                            Enums.Default_Samples,
                                            Value);
   end Set_Default_Samples;

   function Default_Samples (Object : Framebuffer) return Size is
      Ret : aliased Size;
   begin
      API.Get_Named_Framebuffer_Parameter_Size
        (Object.Reference.GL_Id, Enums.Default_Samples, Ret'Unchecked_Access);
      return Ret;
   end Default_Samples;

   function Max_Framebuffer_Samples return Size is
      Ret : Size := 4;
   begin
      API.Get_Size (Enums.Getter.Max_Framebuffer_Samples, Ret);
      return Ret;
   end Max_Framebuffer_Samples;

   procedure Set_Default_Fixed_Sample_Locations (Object : Framebuffer; Value : Boolean) is
   begin
      API.Named_Framebuffer_Parameter_Bool (Object.Reference.GL_Id,
                                            Enums.Default_Fixed_Sample_Locations,
                                            Low_Level.Bool (Value));
   end Set_Default_Fixed_Sample_Locations;

   function Default_Fixed_Sample_Locations (Object : Framebuffer) return Boolean is
      Ret : aliased Low_Level.Bool;
   begin
      API.Get_Named_Framebuffer_Parameter_Bool
        (Object.Reference.GL_Id, Enums.Default_Fixed_Sample_Locations,
         Ret'Unchecked_Access);
      return Boolean (Ret);
   end Default_Fixed_Sample_Locations;

   overriding
   procedure Initialize_Id (Object : in out Framebuffer) is
      New_Id : UInt := 0;
   begin
      API.Create_Framebuffers (1, New_Id);
      Object.Reference.GL_Id := New_Id;
      Object.Reference.Initialized := True;
   end Initialize_Id;

   overriding
   procedure Delete_Id (Object : in out Framebuffer) is
      Arr : constant Low_Level.UInt_Array := (1 => Object.Reference.GL_Id);
   begin
      API.Delete_Framebuffers (1, Arr);
      Object.Reference.GL_Id := 0;
      Object.Reference.Initialized := False;
   end Delete_Id;

   procedure Bind (Target : Framebuffer_Target;
                   Object : Framebuffer'Class) is
   begin
      API.Bind_Framebuffer (Target.Kind, Object.Reference.GL_Id);
   end Bind;

   procedure Clear_Color_Buffer
     (Object      : Framebuffer;
      Index       : Buffers.Draw_Buffer_Index;
      Format_Type : Pixels.Extensions.Format_Type;
      Value       : Colors.Color)
   is
      use all type GL.Pixels.Extensions.Format_Type;
   begin
      case Format_Type is
         when Float_Or_Normalized_Type =>
            API.Clear_Named_Framebuffer_Color_Real
              (Object.Reference.GL_Id, Enums.Color_Buffer, Index, Value);
         when Int_Type =>
            API.Clear_Named_Framebuffer_Color_Signed_Int
              (Object.Reference.GL_Id, Enums.Color_Buffer, Index, Value);
         when Unsigned_Int_Type =>
            API.Clear_Named_Framebuffer_Color_Unsigned_Int
              (Object.Reference.GL_Id, Enums.Color_Buffer, Index, Value);
         when Depth_Type =>
            raise Constraint_Error;
      end case;
   end Clear_Color_Buffer;

   procedure Clear_Depth_Buffer (Object : Framebuffer; Value : Buffers.Depth) is
      Aliased_Value : aliased Buffers.Depth := Value;
   begin
      API.Clear_Named_Framebuffer_Depth
        (Object.Reference.GL_Id, Enums.Depth_Buffer, 0, Aliased_Value);
   end Clear_Depth_Buffer;

   procedure Clear_Stencil_Buffer (Object : Framebuffer; Value : Buffers.Stencil_Index) is
      Aliased_Value : aliased Buffers.Stencil_Index := Value;
   begin
      API.Clear_Named_Framebuffer_Stencil
        (Object.Reference.GL_Id, Enums.Stencil_Buffer, 0, Aliased_Value);
   end Clear_Stencil_Buffer;

   procedure Clear_Depth_And_Stencil_Buffer (Object : Framebuffer;
                                             Depth_Value   : Buffers.Depth;
                                             Stencil_Value : Buffers.Stencil_Index) is
   begin
      API.Clear_Named_Framebuffer_Depth_Stencil
        (Object.Reference.GL_Id, Enums.Depth_Stencil_Buffer, 0, Depth_Value, Stencil_Value);
   end Clear_Depth_And_Stencil_Buffer;

   -----------------------------------------------------------------------------

   type Default_Framebuffer_Type is new Framebuffer with null record;

   overriding
   procedure Initialize_Id (Object : in out Default_Framebuffer_Type) is null;

   overriding
   procedure Delete_Id (Object : in out Default_Framebuffer_Type) is null;

   Default_FB : constant Default_Framebuffer_Type
     := Default_Framebuffer_Type'(GL_Object with null record);

   function Default_Framebuffer return Framebuffer is (Framebuffer (Default_FB));

end GL.Objects.Framebuffers;
