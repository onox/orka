--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2020 onox <denkpadje@gmail.com>
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

private with GL.Low_Level.Enums;

private with Orka.Rendering.Buffers;
private with Orka.Rendering.Programs.Uniforms;
private with Orka.Rendering.Framebuffers;
private with Orka.Types;

with GL.Objects.Textures;
with GL.Types;

with Orka.Resources.Locations;

package Orka.Rendering.Effects.Filters is
   pragma Preelaborate;

   use type GL.Types.Size;

   function Gaussian_Kernel (Radius : GL.Types.Size) return GL.Types.Single_Array
     with Pre => Radius mod 2 = 0;

   type Separable_Filter is tagged limited private;

   function Create_Filter
     (Location : Resources.Locations.Location_Ptr;
      Subject  : GL.Objects.Textures.Texture;
      Kernel   : GL.Types.Single_Array) return Separable_Filter
   with Pre => Kernel'Length mod 2 = 0;
   --  Create a separable filter
   --
   --  The kernel must consist of a sequence of offsets, followed by a
   --  sequence of weights.

   procedure Render (Object : in out Separable_Filter; Passes : Positive := 1);
   --  Apply the filter to the texture given to the constructor of the filter
   --  in one or more passes
   --
   --  For better performance, it is recommended to first downsample a
   --  texture to half the size (with procedure Resolve_To in the package
   --  Orka.Rendering.Framebuffers) before applying the filter to it. However,
   --  this can visibly reduce the quality of the image for very small kernels.

private

   package LE renames GL.Low_Level.Enums;

   type Separable_Filter is tagged limited record
      Program_Blur       : Rendering.Programs.Program;
      Uniform_Horizontal : Rendering.Programs.Uniforms.Uniform (LE.Bool_Type);

      Buffer_Weights     : Rendering.Buffers.Buffer (Types.Single_Type);

      Framebuffer_H : Rendering.Framebuffers.Framebuffer (Default => False);
      Framebuffer_V : Rendering.Framebuffers.Framebuffer (Default => False);

      Texture_H : GL.Objects.Textures.Texture (LE.Texture_Rectangle);
      Texture_V : GL.Objects.Textures.Texture (LE.Texture_Rectangle);
   end record;

end Orka.Rendering.Effects.Filters;
