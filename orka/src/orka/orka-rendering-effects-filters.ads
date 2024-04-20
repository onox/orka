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

with Orka.Contexts;
with Orka.Frame_Graphs;
with Orka.Rendering.Textures;
with Orka.Resources.Locations;

private with Orka.Rendering.Buffers;
private with Orka.Rendering.Shaders.Uniforms;
private with Orka.Rendering.Shaders.Objects;
private with Orka.Types;

package Orka.Rendering.Effects.Filters is
   pragma Preelaborate;

   use type Orka.Rendering.Textures.LE.Texture_Kind;

   function Gaussian_Kernel (Radius : Size) return Float_32_Array
     with Pre => Radius mod 2 = 0;

   type Separable_Filter (Context : not null access constant Orka.Contexts.Context'Class) is tagged limited private;

   function Create_Filter
     (Context  : aliased Orka.Contexts.Context'Class;
      Location : Resources.Locations.Location_Ptr;
      Kernel   : Float_32_Array) return Separable_Filter
   with Pre => Kernel'Length mod 2 = 0;
   --  Create a separable filter
   --
   --  The kernel must consist of a sequence of offsets, followed by a
   --  sequence of weights.

   function Create_Graph
     (Object : Separable_Filter;
      Color  : Orka.Rendering.Textures.Texture_Description;
      Passes : Positive := 1) return Orka.Frame_Graphs.Frame_Graph
   with Pre => Color.Kind = Orka.Rendering.Textures.LE.Texture_Rectangle;
   --  Apply the filter to the given texture in one or more passes
   --
   --  For better performance, it is recommended to first downsample a
   --  texture to half the size (with procedure Resolve_To in the package
   --  Orka.Rendering.Framebuffers) before applying the filter to it. However,
   --  this can visibly reduce the quality of the image for very small kernels.

   -----------------------------------------------------------------------------

   type Moving_Average_Filter (Context : not null access constant Orka.Contexts.Context'Class) is tagged limited private;

   function Create_Filter
     (Context  : aliased Orka.Contexts.Context'Class;
      Location : Resources.Locations.Location_Ptr;
      Radius   : Size) return Moving_Average_Filter;
   --  Create a filter that computes the moving average per row in a
   --  compute shader for a O(1) time complexity, giving a consistent
   --  performance independent of the radius

   function Create_Graph
     (Object : in out Moving_Average_Filter;
      Color  : Orka.Rendering.Textures.Texture_Description;
      Passes : Positive := 2) return Orka.Frame_Graphs.Frame_Graph
   with Pre => Color.Kind = Orka.Rendering.Textures.LE.Texture_Rectangle;
   --  Apply the filter to the given texture in one or more passes
   --
   --  Passes is 2 by default to get a Gaussian blur instead of a box blur.
   --
   --  For better performance, it is recommended to first downsample the
   --  texture. Due to the O(1) time complexity, for very large kernel this
   --  filter should give better performance than the separable filter with
   --  a Gaussian kernel.

private

   package LE renames Orka.Rendering.Textures.LE;

   type Separable_Filter_Program_Callback (Data : not null access Separable_Filter; Horizontal : Boolean) is
     limited new Orka.Frame_Graphs.Program_Callback with null record;

   overriding procedure Run (Object : Separable_Filter_Program_Callback);

   type Separable_Filter (Context : not null access constant Orka.Contexts.Context'Class)  is tagged limited record
      Program            : Rendering.Shaders.Objects.Shader_Objects;
      Uniform_Horizontal : Rendering.Shaders.Uniforms.Uniform (LE.Bool_Type);

      Buffer_Weights     : Rendering.Buffers.Buffer (Types.Single_Type);

      Callback_Horizontal : aliased Separable_Filter_Program_Callback (Separable_Filter'Access, True);
      Callback_Vertical   : aliased Separable_Filter_Program_Callback (Separable_Filter'Access, False);
   end record;

   type Moving_Average_Filter_Program_Callback (Data : not null access Moving_Average_Filter; Horizontal : Boolean) is
     limited new Orka.Frame_Graphs.Program_Callback with null record;

   overriding procedure Run (Object : Moving_Average_Filter_Program_Callback);

   type Moving_Average_Filter (Context : not null access constant Orka.Contexts.Context'Class) is tagged limited record
      Program            : Rendering.Shaders.Objects.Shader_Objects;
      Uniform_Horizontal : Rendering.Shaders.Uniforms.Uniform (LE.Bool_Type);

      Columns, Rows : Unsigned_32;

      Callback_Horizontal : aliased Moving_Average_Filter_Program_Callback (Moving_Average_Filter'Access, True);
      Callback_Vertical   : aliased Moving_Average_Filter_Program_Callback (Moving_Average_Filter'Access, False);
   end record;

end Orka.Rendering.Effects.Filters;
