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

with Ada.Containers.Indefinite_Holders;

with GL.Buffers;
with GL.Objects.Framebuffers;
with GL.Objects.Textures;
with GL.Types;

with Orka.Contexts;
with Orka.Rendering.Buffers;
with Orka.Windows;

package Orka.Rendering.Framebuffers is
   pragma Preelaborate;

   use GL.Types;

   type Framebuffer
     (Default : Boolean;
      Width, Height, Samples : Size) is tagged private
   with Type_Invariant => (if Framebuffer.Default then Framebuffer.Samples = 0);

   type Framebuffer_Ptr is not null access Framebuffer;

   use type GL.Objects.Framebuffers.Framebuffer;
   use all type GL.Objects.Textures.Dimension_Count;

   function Create_Framebuffer
     (Width, Height : Size;
      Color_Texture : GL.Objects.Textures.Texture) return Framebuffer
   with Pre  => Color_Texture.Dimensions = Two,
        Post => not Create_Framebuffer'Result.Default;

   function Create_Framebuffer
     (Width, Height : Size;
      Color_Texture, Depth_Texture : GL.Objects.Textures.Texture) return Framebuffer
   with Pre  => Color_Texture.Dimensions = Two and Depth_Texture.Dimensions = Two,
        Post => not Create_Framebuffer'Result.Default;

   function Create_Framebuffer
     (Width, Height, Samples : Size;
      Context : Contexts.Context) return Framebuffer
   with Pre  => Samples > 0 and Context.Enabled (Contexts.Multisample),
        Post => not Create_Framebuffer'Result.Default;

   function Get_Default_Framebuffer
     (Window : Orka.Windows.Window'Class) return Framebuffer
   with Post => Get_Default_Framebuffer'Result.Default;

   function Create_Default_Framebuffer
     (Width, Height : Size) return Framebuffer
   with Post => Create_Default_Framebuffer'Result.Default;

   function GL_Framebuffer (Object : Framebuffer) return GL.Objects.Framebuffers.Framebuffer
     with Inline;

   procedure Use_Framebuffer (Object : Framebuffer);
   --  Use the framebuffer during rendering
   --
   --  The viewport is adjusted to the size of the framebuffer.

   procedure Resolve_To
     (Object, Subject : Framebuffer;
      Mask : GL.Buffers.Buffer_Bits := (Color => True, others => False))
   with Pre => Object /= Subject;

   Framebuffer_Incomplete_Error : exception;

private

   package Attachment_Holder is new Ada.Containers.Indefinite_Holders
     (Element_Type => GL.Objects.GL_Object'Class, "=" => GL.Objects."=");

   type Framebuffer (Default : Boolean; Width, Height, Samples : Size) is tagged record
      GL_Framebuffer : GL.Objects.Framebuffers.Framebuffer;
      Color_Attachment : Attachment_Holder.Holder;
      Depth_Attachment : Attachment_Holder.Holder;
   end record;

end Orka.Rendering.Framebuffers;
