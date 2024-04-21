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

with Ada.Characters.Latin_1;
with Ada.Numerics.Generic_Elementary_Functions;

with GL.Types;

with Orka.Rendering.Drawing;
with Orka.Rendering.States;
with Orka.Registry;
with Orka.Resources.Locations;
with Orka.Transforms.Doubles.Matrices;
with Orka.Transforms.Doubles.Vectors;
with Orka.Transforms.Doubles.Vector_Conversions;
with Orka.Transforms.Singles.Vectors;

package body Orka.Features.Atmosphere.Rendering is

   package L1 renames Ada.Characters.Latin_1;
   package EF is new Ada.Numerics.Generic_Elementary_Functions (Float_64);

   Binding_Buffer_Matrices : constant := 0;
   Binding_Buffer_Metadata : constant := 1;

   Altitude_Hack_Threshold : constant := 8000.0;

   function Create_Atmosphere
     (Context    : aliased Orka.Contexts.Context'Class;
      Data       : aliased Model_Data;
      Textures   : Precomputed_Textures;
      Parameters : Model_Parameters := (others => <>)) return Atmosphere
   is
      Location : constant Orka.Resources.Locations.Location_Ptr := Orka.Registry.Location ("orka-atmosphere");

      Atmosphere_Model : constant Model := Create_Model (Context, Data);

      Sky_GLSL : constant String := Resources.Convert
        (Orka.Resources.Byte_Array'(Location.Read_Data ("sky.frag").Get));

      use Orka.Rendering.Buffers;
      use all type Orka.Rendering.Shaders.Shader_Kind;
      use Orka.Rendering.Shaders.Objects;
      use Orka.Rendering.Shaders.Modules;

      Sky_Shader : constant String :=
        "#version 420 core" & L1.LF &
        (if Data.Luminance /= None then "#define USE_LUMINANCE" & L1.LF else "") &
        "const float kLengthUnitInMeters = " & Data.Length_Unit_In_Meters'Image & ";" & L1.LF &
        Sky_GLSL & L1.LF;

      Shader_Module : constant Orka.Rendering.Shaders.Modules.Shader_Module := Atmosphere_Model.Get_Shader;
   begin
      return Result : Atmosphere :=
        (Program         =>
           [Vertex_Shader   => Create_Shader (Vertex_Shader, "orka-atmosphere:sky.vert"),
            Fragment_Shader => Create_Shader (Shader_Module_Array'
              [Create_Module_From_Source (Fragment_Shader, Sky_Shader), Shader_Module]),
            others          => Empty],
         Buffer_Matrices => Create_Buffer ((Dynamic_Storage => True, others => False), Orka.Types.Single_Matrix_Type, 2),
         Buffer_Metadata => Create_Buffer ((Dynamic_Storage => True, others => False), Orka.Types.Single_Vector_Type, 5),
         Context         => Context'Access,
         Module          => Shader_Module,
         Textures        => Textures,
         Parameters      => Parameters,
         Bottom_Radius   => Data.Bottom_Radius,
         Distance_Scale  => 1.0 / Data.Length_Unit_In_Meters,
         others          => <>)
      do
         Result.Uniform_Ground_Hack   := Result.Program (Fragment_Shader).Value.Uniform ("ground_hack");
         Result.Uniform_Camera_Offset := Result.Program (Fragment_Shader).Value.Uniform ("camera_offset");
      end return;
   end Create_Atmosphere;

   function Shader_Module (Object : Atmosphere) return Orka.Rendering.Shaders.Modules.Shader_Module is (Object.Module);

   function Flattened_Vector
     (Parameters : Model_Parameters;
      Direction  : Orka.Transforms.Doubles.Vectors.Vector4)
   return Orka.Transforms.Doubles.Vectors.Vector4 is
      Altitude   : constant := 0.0;
      Flattening : Float_64 renames Parameters.Flattening;

      E2 : constant Float_64 := 2.0 * Flattening - Flattening**2;

      N : constant Float_64 := Parameters.Semi_Major_Axis /
        EF.Sqrt (1.0 - E2 * Direction (Orka.Z)**2);
   begin
      return
        [Direction (Orka.X) * (N + Altitude),
         Direction (Orka.Y) * (N + Altitude),
         Direction (Orka.Z) * (N * (1.0 - E2) + Altitude),
         1.0];
   end Flattened_Vector;

   package Matrices renames Orka.Transforms.Doubles.Matrices;

   procedure Set_Data
     (Object : in out Atmosphere;
      Camera : Cameras.Camera_Ptr;
      Planet : Behaviors.Behavior_Ptr;
      Star   : Behaviors.Behavior_Ptr)
   is
      function "*" (Left : Matrices.Matrix4; Right : Matrices.Vector4) return Matrices.Vector4
        renames Matrices."*";

      function "*" (Left, Right : Matrices.Matrix4) return Matrices.Matrix4 renames Matrices."*";

      use Orka.Transforms.Doubles.Vectors;
      use Orka.Transforms.Doubles.Vector_Conversions;

      use all type Orka.Transforms.Doubles.Vectors.Vector4;

      Planet_To_Camera : constant Vector4 := Camera.View_Position - Planet.Position;
      Planet_To_Star   : constant Vector4 := Star.Position - Planet.Position;
      Camera_To_Star   : constant Vector4 := Star.Position - Camera.View_Position;

      procedure Apply_Hacks is
         GL_To_Geo : constant Matrices.Matrix4 := Matrices.R
           (Matrices.Vectors.Normalize ([1.0, 1.0, 1.0, 0.0]),
            (2.0 / 3.0) * Ada.Numerics.Pi);
         Earth_Tilt : constant Matrices.Matrix4 := Matrices.R
           (Matrices.Vectors.Normalize ([1.0, 0.0, 0.0, 0.0]),
            Object.Parameters.Axial_Tilt);

         Inverse_Inertial    : constant Matrices.Matrix4 := Earth_Tilt * GL_To_Geo;
         Camera_Normal_Inert : constant Vector4 := Normalize (Inverse_Inertial * Planet_To_Camera);

         Actual_Surface   : constant Vector4 := Flattened_Vector
           (Object.Parameters, Camera_Normal_Inert);
         Expected_Surface : constant Vector4 := Camera_Normal_Inert * Object.Bottom_Radius;

         Offset   : constant Vector4  := Expected_Surface - Actual_Surface;
         Altitude : constant Float_64 := Norm (Planet_To_Camera) - Norm (Actual_Surface);
      begin
         Object.Uniform_Ground_Hack.Set_Boolean (Altitude < Altitude_Hack_Threshold);
         Object.Uniform_Camera_Offset.Set_Vector (Convert (Offset * Object.Distance_Scale));
      end Apply_Hacks;

      --  Use distance to star and its radius instead of the
      --  Sun_Angular_Radius of Model_Data
      Angular_Radius : constant Float_64 :=
        EF.Arctan (Object.Parameters.Star_Radius, Norm (Camera_To_Star));
   begin
      if Object.Parameters.Flattening > 0.0 then
         Apply_Hacks;
      else
         Object.Uniform_Ground_Hack.Set_Boolean (False);
         Object.Uniform_Camera_Offset.Set_Vector (Orka.Transforms.Singles.Vectors.Zero_Vector);
      end if;

      Object.Buffer_Metadata.Set_Data (Orka.Types.Singles.Vector4_Array'[
         Orka.Transforms.Singles.Vectors.Zero_Vector,
         Convert (-Planet_To_Camera * Object.Distance_Scale),
         Convert (Normalize (Planet_To_Star)),
         Convert (Normalize (Camera_To_Star)),
         [Float_32 (EF.Cos (Angular_Radius)), 0.0, 0.0, 0.0]
      ]);

      Object.Buffer_Matrices.Set_Data (Orka.Types.Singles.Matrix4_Array'[Camera.View_Matrix, Camera.Projection_Matrix]);
   end Set_Data;

   overriding procedure Run (Object : Atmosphere_Program_Callback) is
      use all type Orka.Rendering.Buffers.Indexed_Buffer_Target;
   begin
      Bind_Textures (Object.Data.Textures);

      Object.Data.Buffer_Matrices.Bind (Uniform, Binding_Buffer_Matrices);
      Object.Data.Buffer_Metadata.Bind (Uniform, Binding_Buffer_Metadata);

      Object.Data.Context.Bind_Shaders (Object.Data.Program);

      Orka.Rendering.Drawing.Draw (GL.Types.Triangles, 0, 3);
   end Run;

   function Create_Graph
     (Object       : Atmosphere;
      Color, Depth : Orka.Rendering.Textures.Texture_Description) return Orka.Frame_Graphs.Frame_Graph
   is
      use Orka.Frame_Graphs;

      Graph : aliased Orka.Frame_Graphs.Frame_Graph
        (Maximum_Passes    => 1,
         Maximum_Handles   => 2,
         Maximum_Resources => 4);

      State : constant Orka.Rendering.States.State := (Depth_Func => GL.Types.GEqual, others => <>);
      Pass  : Render_Pass'Class := Graph.Add_Pass ("atmosphere", State, Object.Callback'Unchecked_Access);

      Resource_Color_V1 : constant Orka.Frame_Graphs.Resource :=
        (Name        => +"atmosphere-color",
         Description => Color,
         others      => <>);

      Resource_Depth_V1 : constant Orka.Frame_Graphs.Resource :=
        (Name        => +"atmosphere-depth",
         Description => Depth,
         others      => <>);

      Resource_Color_V2 : constant Orka.Frame_Graphs.Resource :=
        Pass.Add_Input_Output (Resource_Color_V1, Framebuffer_Attachment, 0);
      Resource_Depth_V2 : constant Orka.Frame_Graphs.Resource :=
        Pass.Add_Input_Output (Resource_Depth_V1, Framebuffer_Attachment, 1);
   begin
      Graph.Import ([Resource_Color_V1, Resource_Depth_V1]);
      Graph.Export ([Resource_Color_V2, Resource_Depth_V2]);

      return Graph;
   end Create_Graph;

end Orka.Features.Atmosphere.Rendering;
