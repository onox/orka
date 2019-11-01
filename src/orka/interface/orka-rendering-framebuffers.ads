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

private with Ada.Containers.Indefinite_Holders;

with GL.Buffers;
with GL.Objects.Framebuffers;
with GL.Objects.Textures;
with GL.Types.Colors;

with Orka.Contexts;
with Orka.Rendering.Buffers;
with Orka.Rendering.Textures;
with Orka.Windows;

package Orka.Rendering.Framebuffers is
   pragma Preelaborate;

   use GL.Types;

   package FB renames GL.Objects.Framebuffers;
   package Textures renames GL.Objects.Textures;

   subtype Color_Attachment_Point is FB.Attachment_Point
     range FB.Color_Attachment_0 .. FB.Color_Attachment_15;

   type Use_Point_Array is array (Rendering.Framebuffers.Color_Attachment_Point) of Boolean;
   --  TODO Use as formal parameter in procedure Invalidate

   type Buffer_Values is record
      Color   : Colors.Color             := (0.0, 0.0, 0.0, 0.0);
      Depth   : GL.Buffers.Depth         := 1.0;
      Stencil : GL.Buffers.Stencil_Index := 0;
   end record;

   type Framebuffer
     (Default : Boolean;
      Width, Height, Samples : Size) is tagged private
   with Type_Invariant => (if Framebuffer.Default then Framebuffer.Samples = 0);

   type Framebuffer_Ptr is not null access Framebuffer;

   function Create_Framebuffer
     (Width, Height, Samples : Size;
      Context : Contexts.Context'Class) return Framebuffer
   with Pre  => Samples > 0 and Context.Enabled (Contexts.Multisample),
        Post => not Create_Framebuffer'Result.Default;

   function Create_Framebuffer (Width, Height : Size) return Framebuffer
     with Post => not Create_Framebuffer'Result.Default;

   -----------------------------------------------------------------------------

   function Get_Default_Framebuffer
     (Window : Orka.Windows.Window'Class) return Framebuffer
   with Post => Get_Default_Framebuffer'Result.Default;

   function Create_Default_Framebuffer
     (Width, Height : Size) return Framebuffer
   with Post => Create_Default_Framebuffer'Result.Default;

   -----------------------------------------------------------------------------

   function GL_Framebuffer (Object : Framebuffer) return FB.Framebuffer
     with Inline;

   -----------------------------------------------------------------------------

   function Image (Object : Framebuffer) return String;
   --  Return a description of the given framebuffer

   procedure Use_Framebuffer (Object : Framebuffer);
   --  Use the framebuffer during rendering
   --
   --  The viewport is adjusted to the size of the framebuffer.

   procedure Set_Default_Values (Object : in out Framebuffer; Values : Buffer_Values);
   --  Set the default values for the color buffers and depth and stencil
   --  buffers

   function Default_Values (Object : Framebuffer) return Buffer_Values;
   --  Return the current default values used when clearing the attached
   --  textures

   procedure Set_Read_Buffer
     (Object : Framebuffer;
      Buffer : GL.Buffers.Color_Buffer_Selector);
   --  Set the buffer to use when blitting to another framebuffer with
   --  procedure Resolve_To

   procedure Set_Draw_Buffers
     (Object  : in out Framebuffer;
      Buffers : GL.Buffers.Color_Buffer_List);
   --  Set the buffers to use when drawing to output variables in a fragment
   --  shader, when calling procedure Clear, or when another framebuffer
   --  blits its read buffer to this framebuffer with procedure Resolve_To

   procedure Clear
     (Object : Framebuffer;
      Mask   : GL.Buffers.Buffer_Bits := (others => True));
   --  Clear the attached textures for which the mask is True using
   --  the default values set with Set_Default_Values
   --
   --  For clearing to be effective, the following conditions must apply:
   --
   --    - Write mask off
   --    - Rasterizer discard disabled
   --    - Scissor test off or scissor rectangle set to the desired region
   --    - Called procedure Set_Draw_Buffers with a list of attachments
   --
   --  If a combined depth/stencil texture has been attached, the depth
   --  and stencil components can be cleared separately, but it may be
   --  faster to clear both components.

   procedure Invalidate
     (Object : Framebuffer;
      Mask   : GL.Buffers.Buffer_Bits);
   --  Invalidate the attached textures for which the mask is True

   procedure Resolve_To
     (Object, Subject : Framebuffer;
      Mask : GL.Buffers.Buffer_Bits := (Color => True, others => False))
   with Pre => Object /= Subject and
                 (Mask.Color or Mask.Depth or Mask.Stencil) and
                 (if Object.Samples > 0 and Subject.Samples > 0 then
                    Object.Samples = Subject.Samples);
   --  Copy one or more buffers, resolving multiple samples and scaling
   --  if necessary, from the source to the destination framebuffer
   --
   --  If a buffer is specified in the mask, then the buffer should exist
   --  in both framebuffers, otherwise the buffer is not copied. Call
   --  Set_Read_Buffer and Set_Draw_Buffers to control which buffer is read
   --  from and which buffers are written to.
   --
   --  Format of color buffers may differ and will be converted (if
   --  supported). Formats of depth and stencil buffers must match.
   --
   --  Note: simultaneously resolving multiple samples and scaling
   --  requires GL_EXT_framebuffer_multisample_blit_scaled.

   procedure Attach
     (Object     : in out Framebuffer;
      Attachment : FB.Attachment_Point;
      Texture    : Textures.Texture;
      Level      : Textures.Mipmap_Level := 0;
      Layer      : Natural := 0)
   with Pre  => (not Object.Default and Texture.Allocated and
                  (if Attachment in Color_Attachment_Point then
                    (Object.Width  = Texture.Width  (Level) and
                     Object.Height = Texture.Height (Level))) and
                  (if not Texture.Layered then Layer = 0))
                or else raise Constraint_Error with
                  "Cannot attach " & Rendering.Textures.Image (Texture, Level) &
                  " to " & Object.Image,
        Post => Object.Has_Attachment (Attachment);
   --  Attach the texture to the attachment point
   --
   --  The internal format of the texture must be valid for the given
   --  attachment point.
   --
   --  If the texture is three-dimensional (3D, 1D/2D array, cube
   --  map [array], or 2D multisampled array) then Layer is used to
   --  attach the selected 1D/2D layer to the attachment point.

   procedure Attach
     (Object  : in out Framebuffer;
      Texture : Textures.Texture;
      Level   : Textures.Mipmap_Level := 0;
      Layer   : Natural := 0);
   --  Attach the texture to an attachment point based on the internal
   --  format of the texture
   --
   --  Internally calls the procedure Attach above.
   --
   --  If the texture is color renderable, it will always be attached to
   --  Color_Attachment_0. If you need to attach a texture to a different
   --  color attachment point then use the other procedure Attach directly.

   procedure Detach
     (Object     : in out Framebuffer;
      Attachment : FB.Attachment_Point)
   with Pre  => not Object.Default,
        Post => not Object.Has_Attachment (Attachment);
   --  Detach any texture currently attached to the given attachment point

   function Has_Attachment
     (Object     : Framebuffer;
      Attachment : FB.Attachment_Point) return Boolean;

   Framebuffer_Incomplete_Error : exception;

private

   package Attachment_Holder is new Ada.Containers.Indefinite_Holders
     (Element_Type => Textures.Texture, "=" => Textures."=");

   package Color_Buffer_List_Holder is new Ada.Containers.Indefinite_Holders
     (Element_Type => GL.Buffers.Color_Buffer_List, "=" => GL.Buffers."=");

   type Attachment_Array is array (FB.Attachment_Point)
     of Attachment_Holder.Holder;

   type Framebuffer (Default : Boolean; Width, Height, Samples : Size) is tagged record
      GL_Framebuffer : FB.Framebuffer;
      Attachments    : Attachment_Array;
      Defaults       : Buffer_Values;
      Draw_Buffers   : Color_Buffer_List_Holder.Holder;
   end record;

end Orka.Rendering.Framebuffers;
