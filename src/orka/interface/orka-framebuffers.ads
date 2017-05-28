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

with GL.Objects.Framebuffers;
with GL.Objects.Textures;
with GL.Types;

with Orka.Buffers;

package Orka.Framebuffers is
   pragma Preelaborate;

   use GL.Types;

   type Framebuffer
     (Default : Boolean;
      Width, Height, Samples : Size) is tagged private
   with Type_Invariant => (if Framebuffer.Default then Framebuffer.Samples = 0);

   type Framebuffer_Ptr is not null access Framebuffer;

   use type GL.Objects.Framebuffers.Framebuffer;

   function Create_Framebuffer
     (Width, Height : Size;
      Color_Texture : GL.Objects.Textures.Texture'Class) return Framebuffer
   with Post => not Create_Framebuffer'Result.Default;

   function Create_Framebuffer (Width, Height, Samples : Size) return Framebuffer
     with Pre  => Samples > 0,
          Post => not Create_Framebuffer'Result.Default;

   function Create_Default_Framebuffer (Width, Height : Size) return Framebuffer
     with Inline, Post => Create_Default_Framebuffer'Result.Default
       and Create_Default_Framebuffer'Result.GL_Framebuffer = GL.Objects.Framebuffers.Default_Framebuffer;

   function GL_Framebuffer (Object : Framebuffer) return GL.Objects.Framebuffers.Framebuffer
     with Inline;

   procedure Use_Framebuffer (Object : Framebuffer);
   --  Use the framebuffer during rendering
   --
   --  The viewport is adjusted to the size of the framebuffer.

   procedure Resolve_To (Object : Framebuffer; Subject : Framebuffer)
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

end Orka.Framebuffers;
