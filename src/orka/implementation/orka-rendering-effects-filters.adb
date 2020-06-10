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

with GL.Barriers;
with GL.Compute;
with GL.Toggles;

with Orka.Rendering.Drawing;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Textures;

package body Orka.Rendering.Effects.Filters is

   package EF is new Ada.Numerics.Generic_Elementary_Functions (GL.Types.Double);

   use type GL.Types.Double;

   function Gaussian_Kernel (Radius : GL.Types.Size) return GL.Types.Single_Array is
      Sigma : constant GL.Types.Double :=
        ((GL.Types.Double (Radius) - 1.0) * 0.5 - 1.0) * 0.3 + 0.8;

      Denominator : constant GL.Types.Double :=
        EF.Sqrt (2.0 * Ada.Numerics.Pi * Sigma ** 2);

      Kernel : GL.Types.Double_Array (0 .. Radius);
      Sum    : GL.Types.Double := 0.0;
   begin
      for Index in Kernel'Range loop
         Kernel (Index) := EF.Exp (-GL.Types.Double (Index**2) / (2.0 * Sigma ** 2))
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
         Weights : GL.Types.Single_Array (0 .. Radius / 2);
         Offsets : GL.Types.Single_Array (0 .. Radius / 2);

         use type GL.Types.Single_Array;
         use GL.Types;
      begin
         Weights (Weights'First) := Single (Kernel (Weights'First));
         Offsets (Offsets'First) := 0.0;

         --  Weights
         for Index in Weights'First + 1 .. Weights'Last loop
            declare
               T1 : constant Size := Index * 2 - 1;
               T2 : constant Size := Index * 2;
            begin
               Weights (Index) := Single (Kernel (T1) + Kernel (T2));
            end;
         end loop;

         --  Offsets
         for Index in Offsets'First + 1 .. Offsets'Last loop
            declare
               T1 : constant Size := Index * 2 - 1;
               T2 : constant Size := Index * 2;

               W12 : constant Single := Weights (Index);
            begin
               if W12 > 0.0 then
                  Offsets (Index) := Single ((Double (T1) * Kernel (T1) + Double (T2) * Kernel (T2))
                    / Double (W12));
               else
                  Offsets (Index) := 0.0;
               end if;
            end;
         end loop;

         return Offsets & Weights;
      end;
   end Gaussian_Kernel;

   function Create_Filter
     (Location : Resources.Locations.Location_Ptr;
      Subject  : GL.Objects.Textures.Texture;
      Kernel   : GL.Types.Single_Array) return Separable_Filter
   is
      use all type LE.Texture_Kind;
      pragma Assert (Subject.Kind = LE.Texture_Rectangle);

      use Rendering.Buffers;
      use Rendering.Framebuffers;
      use Rendering.Programs;

      Width  : constant GL.Types.Size := Subject.Width  (0);
      Height : constant GL.Types.Size := Subject.Height (0);
   begin
      return Result : Separable_Filter :=
        (Buffer_Weights => Create_Buffer ((others => False), Kernel),
         Program_Blur   => Create_Program (Modules.Module_Array'
           (Modules.Create_Module (Location, VS => "oversized-triangle.vert"),
            Modules.Create_Module (Location, FS => "effects/blur.frag"))),
         Framebuffer_H => Create_Framebuffer (Width, Height),
         Framebuffer_V => Create_Framebuffer (Width, Height),
         Texture_H     => Subject,
         others => <>)
      do
         Result.Uniform_Horizontal := Result.Program_Blur.Uniform ("horizontal");

         Result.Texture_V.Allocate_Storage (Subject);

         Result.Framebuffer_H.Attach (Result.Texture_H);
         Result.Framebuffer_V.Attach (Result.Texture_V);
      end return;
   end Create_Filter;

   procedure Render (Object : in out Separable_Filter; Passes : Positive := 1) is
      use all type Orka.Rendering.Buffers.Indexed_Buffer_Target;
   begin
      GL.Toggles.Disable (GL.Toggles.Depth_Test);

      Object.Program_Blur.Use_Program;
      Object.Buffer_Weights.Bind (Shader_Storage, 0);

      for Pass in 1 .. Passes loop
         --  Horizontal pass: Texture_H => Texture_V
         Object.Uniform_Horizontal.Set_Boolean (True);
         Orka.Rendering.Textures.Bind (Object.Texture_H, Orka.Rendering.Textures.Texture, 0);

         Object.Framebuffer_V.Use_Framebuffer;
         Orka.Rendering.Drawing.Draw (GL.Types.Triangles, 0, 3);

         --  Vertical pass: Texture_V => Texture_H
         Object.Uniform_Horizontal.Set_Boolean (False);
         Orka.Rendering.Textures.Bind (Object.Texture_V, Orka.Rendering.Textures.Texture, 0);

         Object.Framebuffer_H.Use_Framebuffer;
         Orka.Rendering.Drawing.Draw (GL.Types.Triangles, 0, 3);
      end loop;

      GL.Toggles.Enable (GL.Toggles.Depth_Test);
   end Render;

   -----------------------------------------------------------------------------

   function Create_Filter
     (Location : Resources.Locations.Location_Ptr;
      Subject  : GL.Objects.Textures.Texture;
      Radius   : GL.Types.Size) return Moving_Average_Filter
   is
      use all type LE.Texture_Kind;
      pragma Assert (Subject.Kind = LE.Texture_Rectangle);

      use Rendering.Programs;
      use type GL.Types.Single;

      Width  : constant GL.Types.Size := Subject.Width  (0);
      Height : constant GL.Types.Size := Subject.Height (0);
   begin
      return Result : Moving_Average_Filter :=
        (Program_Blur => Create_Program
           (Modules.Create_Module (Location, CS => "effects/moving-average-blur.comp")),
         Texture_H => Subject,
         others    => <>)
      do
         Result.Uniform_Horizontal := Result.Program_Blur.Uniform ("horizontal");
         Result.Program_Blur.Uniform ("radius").Set_Int (Radius);

         Result.Texture_V.Allocate_Storage (Subject);

         declare
            Work_Group_Size : constant GL.Types.Single :=
             GL.Types.Single (Result.Program_Blur.Compute_Work_Group_Size (GL.X));
         begin
            Result.Columns := GL.Types.UInt
              (GL.Types.Single'Ceiling (GL.Types.Single (Width) / Work_Group_Size));
            Result.Rows := GL.Types.UInt
              (GL.Types.Single'Ceiling (GL.Types.Single (Height) / Work_Group_Size));
         end;
      end return;
   end Create_Filter;

   procedure Render (Object : in out Moving_Average_Filter; Passes : Positive := 2) is
   begin
      Object.Program_Blur.Use_Program;

      for Pass in 1 .. Passes loop
         --  Horizontal pass: Texture_H => Texture_V
         Object.Uniform_Horizontal.Set_Boolean (True);

         Orka.Rendering.Textures.Bind (Object.Texture_H, Orka.Rendering.Textures.Texture, 0);
         Orka.Rendering.Textures.Bind (Object.Texture_V, Orka.Rendering.Textures.Image, 1);
         GL.Compute.Dispatch_Compute (X => Object.Rows);

         GL.Barriers.Memory_Barrier
           ((Shader_Image_Access | Texture_Fetch => True, others => False));

         --  Vertical pass: Texture_V => Texture_H
         Object.Uniform_Horizontal.Set_Boolean (False);

         Orka.Rendering.Textures.Bind (Object.Texture_V, Orka.Rendering.Textures.Texture, 0);
         Orka.Rendering.Textures.Bind (Object.Texture_H, Orka.Rendering.Textures.Image, 1);
         GL.Compute.Dispatch_Compute (X => Object.Columns);

         GL.Barriers.Memory_Barrier
           ((Shader_Image_Access | Texture_Fetch => True, others => False));
      end loop;
   end Render;

end Orka.Rendering.Effects.Filters;
