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

with GL.Objects.Renderbuffers;
with GL.Pixels;
with GL.Toggles;
with GL.Window;

package body Orka.Framebuffers is

   function Create_Framebuffer
     (Width, Height : Size;
      Color_Texture : GL.Objects.Textures.Texture'Class) return Framebuffer
   is
      package FB renames GL.Objects.Framebuffers;

      Depth_Buffer : GL.Objects.Renderbuffers.Renderbuffer;
   begin
      return Result : Framebuffer
        (Default => False,
         Width   => Width,
         Height  => Height,
         Samples => 0)
      do
         Depth_Buffer.Allocate (GL.Pixels.Depth24_Stencil8, Width, Height);

         Result.GL_Framebuffer.Attach_Texture (FB.Color_Attachment_0, Color_Texture, 0);
         Result.GL_Framebuffer.Attach_Renderbuffer (FB.Depth_Stencil_Attachment, Depth_Buffer);

         Result.Color_Attachment := Attachment_Holder.To_Holder (Color_Texture);
         Result.Depth_Attachment := Attachment_Holder.To_Holder (Depth_Buffer);
      end return;
   end Create_Framebuffer;

   function Create_Framebuffer (Width, Height, Samples : Size) return Framebuffer is
      package FB renames GL.Objects.Framebuffers;

      Color_Buffer, Depth_Buffer : GL.Objects.Renderbuffers.Renderbuffer;
   begin
      return Result : Framebuffer
        (Default => False,
         Width   => Width,
         Height  => Height,
         Samples => Samples)
      do
         --  Enable MSAA
         GL.Toggles.Enable (GL.Toggles.Multisample);

         --  Allocate and attach multisampled render buffers
         Color_Buffer.Allocate (GL.Pixels.RGBA8, Width, Height, Samples);
         Depth_Buffer.Allocate (GL.Pixels.Depth_Component24, Width, Height, Samples);

         Result.GL_Framebuffer.Attach_Renderbuffer (FB.Color_Attachment_0, Color_Buffer);
         Result.GL_Framebuffer.Attach_Renderbuffer (FB.Depth_Attachment, Depth_Buffer);

         Result.Color_Attachment := Attachment_Holder.To_Holder (Color_Buffer);
         Result.Depth_Attachment := Attachment_Holder.To_Holder (Depth_Buffer);
      end return;
   end Create_Framebuffer;

   function Create_Default_Framebuffer (Width, Height : Size) return Framebuffer is
   begin
      return Result : Framebuffer
        (Default => True,
         Width   => Width,
         Height  => Height,
         Samples => 0)
      do
         Result.GL_Framebuffer := GL.Objects.Framebuffers.Default_Framebuffer;
      end return;
   end Create_Default_Framebuffer;

   function GL_Framebuffer (Object : Framebuffer) return GL.Objects.Framebuffers.Framebuffer
     is (Object.GL_Framebuffer);

   procedure Use_Framebuffer (Object : Framebuffer) is
      use GL.Objects.Framebuffers;
   begin
      GL.Objects.Framebuffers.Draw_Target.Bind (Object.GL_Framebuffer);

      --  Adjust viewport
      GL.Window.Set_Viewport (0, 0, Object.Width, Object.Height);

      --  Check attachments
      if not Object.Default then
         declare
            Status : constant Framebuffer_Status := Object.GL_Framebuffer.Status (Draw_Target);
         begin
            if Status /= Complete then
               raise Framebuffer_Incomplete_Error with Framebuffer_Status'Image (Status);
            end if;
         end;
      end if;
   end Use_Framebuffer;

   procedure Draw
     (Object  : Framebuffer;
      Program : in out Orka.Programs.Program;
      Mesh    : Orka.Meshes.Vertex_Format;
      Offset, Count : Size) is
   begin
      Use_Framebuffer (Object);
      Program.Use_Program;
      Mesh.Draw (Count, Offset);
   end Draw;

   procedure Draw_Indirect
     (Object  : Framebuffer;
      Program : in out Orka.Programs.Program;
      Mesh    : Orka.Meshes.Vertex_Format;
      Buffer  : Orka.Buffers.Buffer) is
   begin
      Use_Framebuffer (Object);
      Program.Use_Program;
      Mesh.Draw_Indirect (Buffer);
   end Draw_Indirect;

   procedure Resolve_To (Object : Framebuffer; Subject : Framebuffer) is
   begin
      GL.Objects.Framebuffers.Blit
        (Object.GL_Framebuffer, Subject.GL_Framebuffer,
         0, 0, Object.Width, Object.Height,
         0, 0, Subject.Width, Subject.Height,
         (Color => True, others => False), GL.Objects.Textures.Nearest);
   end Resolve_To;

end Orka.Framebuffers;
