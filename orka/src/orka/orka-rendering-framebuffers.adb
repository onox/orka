--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2016 onox <denkpadje@gmail.com>
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
with GL.Viewports;

with Orka.Containers.Bounded_Vectors;
with Orka.Strings;

package body Orka.Rendering.Framebuffers is

   package FB renames GL.Objects.Framebuffers;

   package Attachment_Vectors is new Containers.Bounded_Vectors (Positive, Attachment_Point);

   package Default_Attachment_Vectors is new Containers.Bounded_Vectors
     (Positive, FB.Default_Attachment_Point);

   function Create_Framebuffer
     (Width, Height : Size;
      Samples       : Size := 0) return Framebuffer is
   begin
      return Result : Framebuffer :=
        (Default => False,
         Width   => Width,
         Height  => Height,
         Samples => Samples,
         others  => <>)
      do
         Result.GL_Framebuffer.Set_Default_Width (Width);
         Result.GL_Framebuffer.Set_Default_Height (Height);

         Result.GL_Framebuffer.Set_Default_Samples (Samples);

         Result.Set_Draw_Buffers ((0 => GL.Buffers.Color_Attachment0));
         Result.Set_Read_Buffer (GL.Buffers.Color_Attachment0);
      end return;
   end Create_Framebuffer;

   -----------------------------------------------------------------------------

   function Create_Default_Framebuffer
     (Width, Height : Natural) return Framebuffer is
   begin
      return Result : Framebuffer :=
        (Default => True,
         Width   => Size (Width),
         Height  => Size (Height),
         Samples => 0,
         GL_Framebuffer => GL.Objects.Framebuffers.Default_Framebuffer,
         others  => <>)
      do
         --  Assumes a double-buffered context (Front_Left for single-buffered)
         Result.Set_Draw_Buffers ((0 => GL.Buffers.Back_Left));
         Result.Set_Read_Buffer (GL.Buffers.Back_Left);
      end return;
   end Create_Default_Framebuffer;

   -----------------------------------------------------------------------------

   function Width   (Object : Framebuffer) return Size is (Object.Width);
   function Height  (Object : Framebuffer) return Size is (Object.Height);
   function Samples (Object : Framebuffer) return Size is (Object.Samples);

   function Image (Object : Framebuffer) return String is
      Width  : constant String := Orka.Strings.Trim (Object.Width'Image);
      Height : constant String := Orka.Strings.Trim (Object.Height'Image);

      Default : constant String := (if Object.Default then " default" else "");
   begin
      return Width & Orka.Strings.Unicode (" × ") & Height & Default & " framebuffer";
   end Image;

   procedure Use_Framebuffer (Object : Framebuffer) is
      use GL.Objects.Framebuffers;
   begin
      GL.Objects.Framebuffers.Draw_Target.Bind (Object.GL_Framebuffer);

      --  Adjust viewport
      GL.Viewports.Set_Viewports
        ((0 => (X      => 0.0,
                Y      => 0.0,
                Width  => Single (Object.Width),
                Height => Single (Object.Height))
        ));

      --  Check attachments
      if not Object.Default then
         declare
            Status : constant Framebuffer_Status := Object.GL_Framebuffer.Status (Draw_Target);
         begin
            if Status /= Complete then
               raise Framebuffer_Incomplete_Error with Status'Image;
            end if;
         end;
      end if;
   end Use_Framebuffer;

   procedure Set_Default_Values (Object : in out Framebuffer; Values : Buffer_Values) is
   begin
      Object.Defaults := Values;
   end Set_Default_Values;

   function Default_Values (Object : Framebuffer) return Buffer_Values is (Object.Defaults);

   procedure Set_Read_Buffer
     (Object : Framebuffer;
      Buffer : GL.Buffers.Color_Buffer_Selector) is
   begin
      Object.GL_Framebuffer.Set_Read_Buffer (Buffer);
   end Set_Read_Buffer;

   procedure Set_Draw_Buffers
     (Object  : in out Framebuffer;
      Buffers : GL.Buffers.Color_Buffer_List) is
   begin
      Object.GL_Framebuffer.Set_Draw_Buffers (Buffers);
      Object.Draw_Buffers.Replace_Element (Buffers);
   end Set_Draw_Buffers;

   procedure Clear
     (Object : Framebuffer;
      Mask   : GL.Buffers.Buffer_Bits := (others => True))
   is
      Depth_Stencil : constant Boolean
        := Object.Default or else Object.Has_Attachment (FB.Depth_Stencil_Attachment);
      Depth         : constant Boolean := Object.Has_Attachment (FB.Depth_Attachment);
      Stencil       : constant Boolean := Object.Has_Attachment (FB.Stencil_Attachment);
   begin
      if Mask.Depth or Mask.Stencil then
         if Mask.Depth and Mask.Stencil and Depth_Stencil then
            --  This procedure is used because it may be faster for
            --  combined depth/stencil textures
            Object.GL_Framebuffer.Clear_Depth_And_Stencil_Buffer
              (Depth_Value   => Object.Defaults.Depth,
               Stencil_Value => Object.Defaults.Stencil);
         else
            if Mask.Depth and (Depth_Stencil or Depth) then
               Object.GL_Framebuffer.Clear_Depth_Buffer (Object.Defaults.Depth);
            end if;

            if Mask.Stencil and (Depth_Stencil or Stencil) then
               Object.GL_Framebuffer.Clear_Stencil_Buffer (Object.Defaults.Stencil);
            end if;
         end if;
      end if;

      if Mask.Color then
         declare
            procedure Clear_Attachments (List : GL.Buffers.Color_Buffer_List) is
               package PE renames GL.Pixels.Extensions;

               use all type GL.Buffers.Color_Buffer_Selector;
            begin
               for Index in List'Range loop
                  declare
                     Buffer : GL.Buffers.Color_Buffer_Selector renames List (Index);
                  begin
                     if Buffer /= None then
                        if Object.Default then
                           Object.GL_Framebuffer.Clear_Color_Buffer
                             (Index, PE.Float_Or_Normalized_Type, Object.Defaults.Color);
                        else
                           declare
                              Point : constant Attachment_Point := Color_Attachment_Point'Val
                                (GL.Buffers.Color_Buffer_Selector'Pos (Buffer)
                                   - GL.Buffers.Color_Buffer_Selector'Pos
                                       (GL.Buffers.Color_Attachment0)
                                   + Color_Attachment_Point'Pos (Color_Attachment_Point'First));
                           begin
                              if Object.Has_Attachment (Point) then
                                 declare
                                    Format : constant GL.Pixels.Internal_Format
                                      := Object.Attachments (Point).Constant_Reference.Description.Format;
                                 begin
                                    Object.GL_Framebuffer.Clear_Color_Buffer
                                      (Index, PE.Texture_Format_Type (Format),
                                       Object.Defaults.Color);
                                 end;
                              end if;
                           end;
                        end if;
                     end if;
                  end;
               end loop;
            end Clear_Attachments;
         begin
            Object.Draw_Buffers.Query_Element (Clear_Attachments'Access);
         end;
      end if;
   end Clear;

   procedure Invalidate_Non_Default
     (Object : Framebuffer;
      Mask   : GL.Buffers.Buffer_Bits)
   is
      Attachments : Attachment_Vectors.Vector (Capacity => Attachment_Array'Length);

      Depth_Stencil : constant Boolean := Object.Has_Attachment (FB.Depth_Stencil_Attachment);
      Depth         : constant Boolean := Object.Has_Attachment (FB.Depth_Attachment);
      Stencil       : constant Boolean := Object.Has_Attachment (FB.Stencil_Attachment);

      procedure Invalidate_Attachments (Elements : Attachment_Vectors.Element_Array) is
      begin
         Object.GL_Framebuffer.Invalidate_Data (FB.Attachment_List (Elements));
      end Invalidate_Attachments;
   begin
      if Mask.Depth or Mask.Stencil then
         if Mask.Depth and Mask.Stencil and Depth_Stencil then
            Attachments.Append (FB.Depth_Stencil_Attachment);
         else
            if Mask.Depth and (Depth_Stencil or Depth) then
               Attachments.Append (FB.Depth_Attachment);
            end if;

            if Mask.Stencil and (Depth_Stencil or Stencil) then
               Attachments.Append (FB.Stencil_Attachment);
            end if;
         end if;
      end if;

      if Mask.Color then
         for Attachment in Color_Attachment_Point loop
            if Object.Has_Attachment (Attachment) then
               Attachments.Append (Attachment);
            end if;
         end loop;
      end if;

      Attachments.Query (Invalidate_Attachments'Access);
   end Invalidate_Non_Default;

   procedure Invalidate_Default
     (Object : Framebuffer;
      Mask   : GL.Buffers.Buffer_Bits)
   is
      Attachments : Default_Attachment_Vectors.Vector (Capacity => 3);

      procedure Invalidate_Attachments (Elements : Default_Attachment_Vectors.Element_Array) is
      begin
         Object.GL_Framebuffer.Invalidate_Data (FB.Default_Attachment_List (Elements));
      end Invalidate_Attachments;
   begin
      if Mask.Depth then
         Attachments.Append (FB.Depth);
      end if;

      if Mask.Stencil then
         Attachments.Append (FB.Stencil);
      end if;

      if Mask.Color then
         --  Assumes a double-buffered context (Front_Left for single-buffered)
         Attachments.Append (FB.Back_Left);
      end if;

      Attachments.Query (Invalidate_Attachments'Access);
   end Invalidate_Default;

   procedure Invalidate
     (Object : Framebuffer;
      Mask   : GL.Buffers.Buffer_Bits) is
   begin
      if Object.Default then
         Object.Invalidate_Default (Mask);
      else
         Object.Invalidate_Non_Default (Mask);
      end if;
   end Invalidate;

   procedure Resolve_To
     (Object, Subject : Framebuffer;
      Mask : GL.Buffers.Buffer_Bits := (Color => True, others => False))
   is
      use all type GL.Objects.Framebuffers.Interpolation_Function;
   begin
      FB.Blit
        (Object.GL_Framebuffer, Subject.GL_Framebuffer,
         0, 0, Object.Width, Object.Height,
         0, 0, Subject.Width, Subject.Height,
         Mask,
         (if Mask.Depth or Mask.Stencil then
            Nearest
          elsif Object.Samples > 0 and Subject.Samples = 0
            and (Object.Width /= Subject.Width or Object.Height /= Subject.Height)
          then
            Scaled_Resolve_Nicest
          else Linear));
   end Resolve_To;

   procedure Attach
     (Object     : in out Framebuffer;
      Texture    : Rendering.Textures.Texture;
      Attachment : Color_Attachment_Point := FB.Color_Attachment_0;
      Level      : Textures.Mipmap_Level  := 0)
   is
      procedure Attach
        (Object     : in out Framebuffer;
         Attachment : Attachment_Point;
         Texture    : Rendering.Textures.Texture;
         Level      : Textures.Mipmap_Level) is
      begin
         Object.GL_Framebuffer.Attach_Texture (Attachment, Texture.GL_Texture, Level);
         Object.Attachments (Attachment) := Attachment_Holder.To_Holder (Texture);
      end Attach;

      use all type GL.Pixels.Internal_Format;
   begin
      case Texture.Description.Format is
         when Depth24_Stencil8 | Depth32F_Stencil8 =>
            Attach (Object, FB.Depth_Stencil_Attachment, Texture, Level);
         when Depth_Component16 | Depth_Component24 | Depth_Component32F =>
            Attach (Object, FB.Depth_Attachment, Texture, Level);
         when Stencil_Index8 =>
            Attach (Object, FB.Stencil_Attachment, Texture, Level);
         when others =>
            Attach (Object, Attachment, Texture, Level);
      end case;
   end Attach;

   procedure Attach_Layer
     (Object     : in out Framebuffer;
      Texture    : Rendering.Textures.Texture;
      Attachment : Attachment_Point;
      Layer      : Natural;
      Level      : Textures.Mipmap_Level := 0) is
   begin
      Object.GL_Framebuffer.Attach_Texture_Layer (Attachment, Texture.GL_Texture, Level, Layer => Layer);
      Object.Attachments (Attachment) := Attachment_Holder.To_Holder (Texture);
   end Attach_Layer;

   procedure Detach
     (Object     : in out Framebuffer;
      Attachment : Attachment_Point) is
   begin
      Object.GL_Framebuffer.Detach (Attachment);
      Object.Attachments (Attachment).Clear;
   end Detach;

   function Has_Attachment
     (Object     : Framebuffer;
      Attachment : Attachment_Point) return Boolean
   is (not Object.Attachments (Attachment).Is_Empty);

end Orka.Rendering.Framebuffers;
