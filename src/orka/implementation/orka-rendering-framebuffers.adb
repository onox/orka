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

with GL.Pixels;
with GL.Window;
with GL.Low_Level.Enums;

package body Orka.Rendering.Framebuffers is

   package FB renames GL.Objects.Framebuffers;
   package Textures renames GL.Objects.Textures;

   function Create_Framebuffer
     (Width, Height : Size;
      Color_Texture : GL.Objects.Textures.Texture) return Framebuffer
   is
      Depth_Buffer : Textures.Texture (GL.Low_Level.Enums.Texture_2D);
   begin
      return Result : Framebuffer
        (Default => False,
         Width   => Width,
         Height  => Height,
         Samples => 0)
      do
         Depth_Buffer.Allocate_Storage (1, 1, GL.Pixels.Depth32F_Stencil8, Width, Height, 1);

         Result.GL_Framebuffer.Attach_Texture (FB.Color_Attachment_0, Color_Texture, 0);
         Result.GL_Framebuffer.Attach_Texture (FB.Depth_Stencil_Attachment, Depth_Buffer, 0);

         Result.Color_Attachment := Attachment_Holder.To_Holder (Color_Texture);
         Result.Depth_Attachment := Attachment_Holder.To_Holder (Depth_Buffer);
      end return;
   end Create_Framebuffer;

   function Create_Framebuffer
     (Width, Height : Size;
      Color_Texture, Depth_Texture : GL.Objects.Textures.Texture) return Framebuffer is
   begin
      return Result : Framebuffer
        (Default => False,
         Width   => Width,
         Height  => Height,
         Samples => 0)
      do
         Result.GL_Framebuffer.Attach_Texture (FB.Color_Attachment_0, Color_Texture, 0);
         Result.GL_Framebuffer.Attach_Texture (FB.Depth_Stencil_Attachment, Depth_Texture, 0);
         --  TODO What if a Depth or Stencil-only attachment is needed?
         --    --> check format of Depth_Texture

         Result.Color_Attachment := Attachment_Holder.To_Holder (Color_Texture);
         Result.Depth_Attachment := Attachment_Holder.To_Holder (Depth_Texture);
      end return;
   end Create_Framebuffer;

   function Create_Framebuffer
     (Width, Height, Samples : Size;
      Context : Contexts.Context) return Framebuffer
   is
      Color_Buffer, Depth_Buffer : Textures.Texture (GL.Low_Level.Enums.Texture_2D_Multisample);
   begin
      return Result : Framebuffer
        (Default => False,
         Width   => Width,
         Height  => Height,
         Samples => Samples)
      do
         --  Allocate and attach multisampled textures
         Color_Buffer.Allocate_Storage
           (1, Samples, GL.Pixels.RGBA8, Width, Height, 1);
         Depth_Buffer.Allocate_Storage
           (1, Samples, GL.Pixels.Depth32F_Stencil8, Width, Height, 1);
         --  TODO What if a different format is needed?
         --  TODO What if a Depth or Stencil-only format is needed?

         Result.GL_Framebuffer.Attach_Texture (FB.Color_Attachment_0, Color_Buffer, 0);
         Result.GL_Framebuffer.Attach_Texture (FB.Depth_Stencil_Attachment, Depth_Buffer, 0);
         --  TODO Make sure attachment matches format of Depth_Buffer

         Result.Color_Attachment := Attachment_Holder.To_Holder (Color_Buffer);
         Result.Depth_Attachment := Attachment_Holder.To_Holder (Depth_Buffer);
      end return;
   end Create_Framebuffer;

   function Get_Default_Framebuffer
     (Window : Orka.Windows.Window'Class) return Framebuffer
   is (Create_Default_Framebuffer (Size (Window.Width), Size (Window.Height)));

   function Create_Default_Framebuffer
     (Width, Height : Size) return Framebuffer is
   begin
      return
        (Default => True,
         Width   => Width,
         Height  => Height,
         Samples => 0,
         GL_Framebuffer => GL.Objects.Framebuffers.Default_Framebuffer,
         others  => <>);
   end Create_Default_Framebuffer;

   function GL_Framebuffer (Object : Framebuffer) return GL.Objects.Framebuffers.Framebuffer
     is (Object.GL_Framebuffer);

   procedure Use_Framebuffer (Object : Framebuffer) is
      use GL.Objects.Framebuffers;
   begin
      GL.Objects.Framebuffers.Draw_Target.Bind (Object.GL_Framebuffer);

      --  Adjust viewport
      GL.Window.Set_Viewports
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

   procedure Resolve_To
     (Object, Subject : Framebuffer;
      Mask : GL.Buffers.Buffer_Bits := (Color => True, others => False)) is
   begin
      --  Note: simultaneously resolving and scaling requires
      --  GL_EXT_framebuffer_multisample_blit_scaled
      GL.Objects.Framebuffers.Blit
        (Object.GL_Framebuffer, Subject.GL_Framebuffer,
         0, 0, Object.Width, Object.Height,
         0, 0, Subject.Width, Subject.Height,
         Mask, GL.Objects.Textures.Nearest);
   end Resolve_To;

end Orka.Rendering.Framebuffers;
