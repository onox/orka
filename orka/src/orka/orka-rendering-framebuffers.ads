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
with GL.Types.Colors;

with Orka.Rendering.Textures;

package Orka.Rendering.Framebuffers is
   pragma Preelaborate;

   use GL.Types;
   use all type Rendering.Textures.Format_Kind;

   subtype Attachment_Point is GL.Objects.Framebuffers.Attachment_Point;

   use all type Attachment_Point;

   subtype Color_Attachment_Point is Attachment_Point
     range Color_Attachment_0 .. Color_Attachment_7;

   type Use_Point_Array is array (Rendering.Framebuffers.Color_Attachment_Point) of Boolean;
   --  TODO Use as formal parameter in procedure Invalidate

   type Buffer_Values is record
      Color   : Colors.Color             := [0.0, 0.0, 0.0, 1.0];
      Depth   : GL.Buffers.Depth         := 0.0;
      Stencil : GL.Buffers.Stencil_Index := 0;
   end record;

   type Framebuffer (Default : Boolean) is tagged private;

   function Create_Framebuffer
     (Width, Height : Size;
      Samples       : Size := 0) return Framebuffer
   with Post => not Create_Framebuffer'Result.Default;

   -----------------------------------------------------------------------------

   function Create_Default_Framebuffer
     (Width, Height : Natural) return Framebuffer
   with Post => Create_Default_Framebuffer'Result.Default;

   -----------------------------------------------------------------------------

   function Width   (Object : Framebuffer) return Size;
   function Height  (Object : Framebuffer) return Size;
   function Samples (Object : Framebuffer) return Size;

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
   --  of color buffers requires GL_EXT_framebuffer_multisample_blit_scaled.
   --  If this extension is not present, then two separate calls to this
   --  procedure are needed.

   procedure Attach
     (Object     : in out Framebuffer;
      Texture    : Rendering.Textures.Texture;
      Attachment : Color_Attachment_Point := Color_Attachment_0;
      Level      : Rendering.Textures.Mipmap_Level  := 0)
   with Pre  => (not Object.Default and
                  (if Rendering.Textures.Get_Format_Kind (Texture.Description.Format) = Color then
                    Texture.Size (Level) (X .. Y) = [Object.Width, Object.Height]))
                or else raise Constraint_Error with
                  "Cannot attach " & Texture.Image (Level) &
                  " to " & Object.Image,
        Post => (if Rendering.Textures.Get_Format_Kind (Texture.Description.Format) = Color then
                   Object.Has_Attachment (Attachment));
   --  Attach the texture to an attachment point based on the internal
   --  format of the texture or to the given attachment point if the
   --  texture is color renderable
   --
   --  The internal format of the texture must be valid for the given
   --  attachment point.
   --
   --  If one of the attached textures is layered (3D, 1D/2D array, cube
   --  map [array], or 2D multisampled array), then all attachments must
   --  have the same kind.
   --
   --  If the texture is layered and you want to attach a specific layer,
   --  then you must call the procedure Attach_Layer below instead.
   --
   --  All attachments of the framebuffer must have the same amount of
   --  samples and they must all have fixed sample locations, or none of
   --  them must have them.

   procedure Attach_Layer
     (Object     : in out Framebuffer;
      Texture    : Rendering.Textures.Texture;
      Attachment : Attachment_Point;
      Layer      : Natural;
      Level      : Rendering.Textures.Mipmap_Level := 0)
   with Pre  => (not Object.Default and
                  Rendering.Textures.Has_Layers (Texture.Kind) and
                  (if Attachment in Color_Attachment_Point then
                    Texture.Size (Level) (X .. Y) = [Object.Width, Object.Height]))
                or else raise Constraint_Error with
                  "Cannot attach layer of " & Texture.Image (Level) &
                  " to " & Object.Image,
        Post => Object.Has_Attachment (Attachment);
   --  Attach the selected 1D/2D layer of the texture to the attachment point
   --
   --  The internal format of the texture must be valid for the given
   --  attachment point.
   --
   --  The texture must be layered (3D, 1D/2D array, cube
   --  map [array], or 2D multisampled array).

   procedure Detach
     (Object     : in out Framebuffer;
      Attachment : Attachment_Point)
   with Pre  => not Object.Default,
        Post => not Object.Has_Attachment (Attachment);
   --  Detach any texture currently attached to the given attachment point

   function Has_Attachment
     (Object     : Framebuffer;
      Attachment : Attachment_Point) return Boolean;

   Framebuffer_Incomplete_Error : exception;

private

   package Attachment_Holder is new Ada.Containers.Indefinite_Holders
     (Element_Type => Rendering.Textures.Texture, "=" => Rendering.Textures."=");

   package Color_Buffer_List_Holder is new Ada.Containers.Indefinite_Holders
     (Element_Type => GL.Buffers.Color_Buffer_List, "=" => GL.Buffers."=");

   type Attachment_Array is array (Attachment_Point)
     of Attachment_Holder.Holder;

   type Framebuffer (Default : Boolean) is tagged record
      GL_Framebuffer : GL.Objects.Framebuffers.Framebuffer;
      Attachments    : Attachment_Array;
      Defaults       : Buffer_Values;
      Draw_Buffers   : Color_Buffer_List_Holder.Holder;
      Width, Height, Samples : Size;
   end record;

end Orka.Rendering.Framebuffers;
