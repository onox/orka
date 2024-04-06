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

with Ada.Numerics.Generic_Elementary_Functions;

with GL.Compute;
with GL.Types;

with Orka.Rendering.Drawing;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.States;

package body Orka.Rendering.Effects.Filters is

   package EF is new Ada.Numerics.Generic_Elementary_Functions (Float_64);

   function Gaussian_Kernel (Radius : Size) return Float_32_Array is
      Sigma : constant Float_64 :=
        ((Float_64 (Radius) - 1.0) * 0.5 - 1.0) * 0.3 + 0.8;

      Denominator : constant Float_64 :=
        EF.Sqrt (2.0 * Ada.Numerics.Pi * Sigma ** 2);

      Kernel : Float_64_Array (0 .. Radius);
      Sum    : Float_64 := 0.0;
   begin
      for Index in Kernel'Range loop
         Kernel (Index) := EF.Exp (-Float_64 (Index**2) / (2.0 * Sigma ** 2))
           / Denominator;
         Sum := Sum + Kernel (Index) * (if Index > 0 then 2.0 else 1.0);
         --  Kernel array only stores the positive side of the curve, but
         --  we need to compute the area of the whole curve for normalization
      end loop;

      --  Normalize the weights to prevent the image from becoming darker
      for Index in Kernel'Range loop
         Kernel (Index) := Kernel (Index) / Sum;
      end loop;

      declare
         --  Use bilinear texture filtering hardware [1].
         --
         --  Weight (t1, t2) = Weight (t1) + Weight (t2)
         --
         --                    offset (t1) * weight (t1) + offset (t2) * weight (t2)
         --  Offset (t1, t2) = -----------------------------------------------------
         --                                        weight (t1, t2)
         --
         --  [1] http://rastergrid.com/blog/2010/09/efficient-gaussian-blur-with-linear-sampling/
         Weights : Float_32_Array (0 .. Radius / 2);
         Offsets : Float_32_Array (0 .. Radius / 2);
      begin
         Weights (Weights'First) := Float_32 (Kernel (Weights'First));
         Offsets (Offsets'First) := 0.0;

         --  Weights
         for Index in Weights'First + 1 .. Weights'Last loop
            declare
               T1 : constant Size := Index * 2 - 1;
               T2 : constant Size := Index * 2;
            begin
               Weights (Index) := Float_32 (Kernel (T1) + Kernel (T2));
            end;
         end loop;

         --  Offsets
         for Index in Offsets'First + 1 .. Offsets'Last loop
            declare
               T1 : constant Size := Index * 2 - 1;
               T2 : constant Size := Index * 2;

               W12 : constant Float_32 := Weights (Index);
            begin
               if W12 > 0.0 then
                  Offsets (Index) := Float_32
                    ((Float_64 (T1) * Kernel (T1) + Float_64 (T2) * Kernel (T2)) / Float_64 (W12));
               else
                  Offsets (Index) := 0.0;
               end if;
            end;
         end loop;

         return Offsets & Weights;
      end;
   end Gaussian_Kernel;

   function Create_Filter
     (Context  : aliased Orka.Contexts.Context'Class;
      Location : Resources.Locations.Location_Ptr;
      Kernel   : Float_32_Array) return Separable_Filter
   is
      use Rendering.Buffers;
      use Rendering.Programs;
      use Rendering.Programs.Shaders;
   begin
      return Result : Separable_Filter :=
        (Buffer_Weights => Create_Buffer ((others => False), Kernel),
         Program        => (Vertex_Shader   => Create_Program (Location, Vertex_Shader, "oversized-triangle.vert"),
                            Fragment_Shader => Create_Program (Location, Fragment_Shader, "effects/blur.frag"),
                            others          => Empty),
         Context => Context'Access,
         others => <>)
      do
         Result.Uniform_Horizontal := Result.Program (Fragment_Shader).Value.Uniform ("horizontal");
      end return;
   end Create_Filter;

   overriding procedure Run (Object : Separable_Filter_Program_Callback) is
      use all type Orka.Rendering.Buffers.Indexed_Buffer_Target;
   begin
      Object.Data.Context.Bind_Shaders (Object.Data.Program);
      Object.Data.Buffer_Weights.Bind (Shader_Storage, 0);

      Object.Data.Uniform_Horizontal.Set_Boolean (Object.Horizontal);
      Orka.Rendering.Drawing.Draw (GL.Types.Triangles, 0, 3);
   end Run;

   function Create_Graph
     (Object : Separable_Filter;
      Color  : Orka.Rendering.Textures.Texture_Description;
      Passes : Positive := 1) return Orka.Frame_Graphs.Frame_Graph
   is
      use Orka.Frame_Graphs;

      Graph : aliased Orka.Frame_Graphs.Frame_Graph
        (Maximum_Passes    => Render_Pass_Index (Passes) * 2,  --  1 pass for horizontal and 1 pass for vertical
         Maximum_Handles   => Passes * 4,  --  4x read (because of framebuffer attachment) + 2x write
         Maximum_Resources => Handle_Type (Passes) * 4 + 1);
      --  R1 -> PH -> R2 -> PV -> R3
      --        ^                 |
      --        |                 v
      --        +---------------- +

      State : constant Orka.Rendering.States.State := (others => <>);

      Resources_Horizontal : constant Orka.Frame_Graphs.Resource_Array (1 .. Passes) :=
        [for I in 1 .. Passes =>
          (Name        => +("sep-filter-horizontal-" & I'Image),
           Description => Color,
           others      => <>)];

      Resources_Vertical : constant Orka.Frame_Graphs.Resource_Array (1 .. Passes) :=
        [for I in 1 .. Passes =>
          (Name        => +("sep-filter-vertical-" & I'Image),
           Description => Color,
           others      => <>)];

      Resource_Color_Input : constant Orka.Frame_Graphs.Resource :=
        (Name        => +"sep-filter-color",
         Description => Color,
         others      => <>);
   begin
      for Index in 1 .. Passes loop
         declare
            Pass_Horizontal : Render_Pass'Class :=
              Graph.Add_Pass ("sep-filter-horizontal", State, Object.Callback_Horizontal'Unchecked_Access);
            Pass_Vertical   : Render_Pass'Class :=
              Graph.Add_Pass ("sep-filter-vertical", State, Object.Callback_Vertical'Unchecked_Access);
         begin
            Pass_Horizontal.Add_Input ((if Index > 1 then Resources_Vertical (Index - 1) else Resource_Color_Input), Texture_Read, 0);
            Pass_Horizontal.Add_Output (Resources_Horizontal (Index), Framebuffer_Attachment, 0);

            Pass_Vertical.Add_Input (Resources_Horizontal (Index), Texture_Read, 0);
            Pass_Vertical.Add_Output (Resources_Vertical (Index), Framebuffer_Attachment, 0);
         end;
      end loop;

      Graph.Import ([Resource_Color_Input]);
      Graph.Export ([Resources_Vertical (Resources_Vertical'Last)]);

      return Graph;
   end Create_Graph;

   -----------------------------------------------------------------------------

   function Create_Filter
     (Context  : aliased Orka.Contexts.Context'Class;
      Location : Resources.Locations.Location_Ptr;
      Radius   : Size) return Moving_Average_Filter
   is
      use Rendering.Programs;
      use Rendering.Programs.Shaders;
   begin
      return Result : Moving_Average_Filter :=
        (Program => (Compute_Shader => Create_Program (Location, Compute_Shader, "effects/moving-average-blur.comp"),
                     others         => Empty),
         Context => Context'Access,
         others  => <>)
      do
         Result.Uniform_Horizontal := Result.Program (Compute_Shader).Value.Uniform ("horizontal");
         Result.Program (Compute_Shader).Value.Uniform ("radius").Set_Int (Radius);
      end return;
   end Create_Filter;

   overriding procedure Run (Object : Moving_Average_Filter_Program_Callback) is
      use all type Orka.Rendering.Programs.Shader_Kind;
   begin
      Object.Data.Context.Bind_Shaders (Object.Data.Program);
      Object.Data.Uniform_Horizontal.Set_Boolean (Object.Horizontal);
      GL.Compute.Dispatch_Compute (X => (if Object.Horizontal then Object.Data.Rows else Object.Data.Columns));
   end Run;

   function Create_Graph
     (Object : in out Moving_Average_Filter;
      Color  : Orka.Rendering.Textures.Texture_Description;
      Passes : Positive := 2) return Orka.Frame_Graphs.Frame_Graph
   is
      use all type Orka.Rendering.Programs.Shader_Kind;

      Size : constant Size_3D := Color.Size;

      Work_Group_Size : constant Float_32 :=
        Float_32 (Object.Program (Compute_Shader).Value.Compute_Work_Group_Size (X));

      use Orka.Frame_Graphs;

      Graph : aliased Orka.Frame_Graphs.Frame_Graph
        (Maximum_Passes    => Render_Pass_Index (Passes) * 2,  --  1 pass for horizontal and 1 pass for vertical
         Maximum_Handles   => Passes * 2,  --  2x read + 2x write
         Maximum_Resources => Handle_Type (Passes) * 2 + 1);
      --  R1 -> PH -> R2 -> PV -> R3
      --        ^                 |
      --        |                 v
      --        +---------------- +

      State : constant Orka.Rendering.States.State := (others => <>);

      Resources_Horizontal : constant Orka.Frame_Graphs.Resource_Array (1 .. Passes) :=
        [for I in 1 .. Passes =>
          (Name        => +("avg-filter-horizontal-" & I'Image),
           Description => Color,
           others      => <>)];

      Resources_Vertical : constant Orka.Frame_Graphs.Resource_Array (1 .. Passes) :=
        [for I in 1 .. Passes =>
          (Name        => +("avg-filter-vertical-" & I'Image),
           Description => Color,
           others      => <>)];

      Resource_Color_Input : constant Orka.Frame_Graphs.Resource :=
        (Name        => +"avg-filter-color",
         Description => Color,
         others      => <>);
   begin
      for Index in 1 .. Passes loop
         declare
            Pass_Horizontal : Render_Pass'Class :=
              Graph.Add_Pass ("avg-filter-horizontal", State, Object.Callback_Horizontal'Unchecked_Access);
            Pass_Vertical   : Render_Pass'Class :=
              Graph.Add_Pass ("avg-filter-vertical", State, Object.Callback_Vertical'Unchecked_Access);
         begin
            Pass_Horizontal.Add_Input ((if Index > 1 then Resources_Vertical (Index - 1) else Resource_Color_Input), Texture_Read, 0);
            Pass_Horizontal.Add_Output (Resources_Horizontal (Index), Image_Store, 1);

            Pass_Vertical.Add_Input (Resources_Horizontal (Index), Texture_Read, 0);
            Pass_Vertical.Add_Output (Resources_Vertical (Index), Image_Store, 1);
         end;
      end loop;

      Graph.Import ([Resource_Color_Input]);
      Graph.Export ([Resources_Vertical (Resources_Vertical'Last)]);

      Object.Columns := Unsigned_32
        (Float_32'Ceiling (Float_32 (Size (X)) / Work_Group_Size));
      Object.Rows := Unsigned_32
        (Float_32'Ceiling (Float_32 (Size (Y)) / Work_Group_Size));

      return Graph;
   end Create_Graph;

end Orka.Rendering.Effects.Filters;
