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

with GL.Buffers;

with Orka.Rendering.Drawing;
with Orka.Transforms.Doubles.Matrices;
with Orka.Transforms.Doubles.Vectors;
with Orka.Transforms.Doubles.Vector_Conversions;
with Orka.Transforms.Singles.Vectors;

package body Orka.Features.Atmosphere.Rendering is

   package EF is new Ada.Numerics.Generic_Elementary_Functions (GL.Types.Double);

   Default_Exposure : constant := 10.0;

   Altitude_Hack_Threshold : constant := 8000.0;

   function Create_Atmosphere
     (Data       : aliased Model_Data;
      Location   : Resources.Locations.Location_Ptr;
      Parameters : Model_Parameters := (others => <>)) return Atmosphere
   is
      Atmosphere_Model : constant Model := Create_Model (Data'Access, Location);

      Sky_GLSL : constant String
        := Resources.Convert (Orka.Resources.Byte_Array'(Location.Read_Data ("sky.frag").Get));

      use Ada.Characters.Latin_1;
      use Rendering.Programs;
      use Rendering.Programs.Modules;

      Sky_Shader : constant String :=
        "#version 420 core" & LF &
        (if Data.Luminance /= None then "#define USE_LUMINANCE" & LF else "") &
        "const float kLengthUnitInMeters = " & Data.Length_Unit_In_Meters'Image & ";" & LF &
        Sky_GLSL & LF;

      Shader_Module : constant Rendering.Programs.Modules.Module := Atmosphere_Model.Get_Shader;
   begin
      return Result : Atmosphere :=
        (Program => Create_Program (Modules.Module_Array'
           (Modules.Create_Module (Location, VS => "sky.vert"),
            Modules.Create_Module_From_Sources (FS => Sky_Shader),
            Shader_Module)),
         Module  => Shader_Module,
         Parameters     => Parameters,
         Bottom_Radius  => Data.Bottom_Radius,
         Distance_Scale => 1.0 / Data.Length_Unit_In_Meters,
         Exposure_Scale => Default_Exposure * (if Data.Luminance /= None then 1.0e-5 else 1.0),
         others => <>)
      do
         Result.Uniform_Ground_Hack   := Result.Program.Uniform ("ground_hack");
         Result.Uniform_Camera_Offset := Result.Program.Uniform ("camera_offset");

         Result.Uniform_Camera_Pos := Result.Program.Uniform ("camera_pos");
         Result.Uniform_Planet_Pos := Result.Program.Uniform ("planet_pos");

         Result.Uniform_View := Result.Program.Uniform ("view");
         Result.Uniform_Proj := Result.Program.Uniform ("proj");

         Result.Uniform_Sun_Dir  := Result.Program.Uniform ("sun_direction");
         Result.Uniform_Exposure := Result.Program.Uniform ("exposure");

         Result.Uniform_Star_Dir  := Result.Program.Uniform ("star_direction");
         Result.Uniform_Star_Size := Result.Program.Uniform ("star_size");
      end return;
   end Create_Atmosphere;

   function Shader_Module (Object : Atmosphere) return Orka.Rendering.Programs.Modules.Module is
     (Object.Module);

   function Flattened_Vector
     (Parameters : Model_Parameters;
      Direction  : Orka.Transforms.Doubles.Vectors.Vector4)
   return Orka.Transforms.Doubles.Vectors.Vector4 is
      Altitude   : constant := 0.0;
      Flattening : GL.Types.Double renames Parameters.Flattening;

      E2 : constant GL.Types.Double := 2.0 * Flattening - Flattening**2;

      N : constant GL.Types.Double := Parameters.Semi_Major_Axis /
        EF.Sqrt (1.0 - E2 * Direction (Orka.Z)**2);
   begin
      return
        (Direction (Orka.X) * (N + Altitude),
         Direction (Orka.Y) * (N + Altitude),
         Direction (Orka.Z) * (N * (1.0 - E2) + Altitude),
         1.0);
   end Flattened_Vector;

   package Matrices renames Orka.Transforms.Doubles.Matrices;

   procedure Render
     (Object   : in out Atmosphere;
      Camera   : Cameras.Camera_Ptr;
      Planet   : Behaviors.Behavior_Ptr;
      Star     : Behaviors.Behavior_Ptr;
      Exposure : Exposure_Type := 1.0)
   is
      function "*" (Left : Matrices.Matrix4; Right : Matrices.Vector4) return Matrices.Vector4
        renames Matrices."*";

      function "*" (Left, Right : Matrices.Matrix4) return Matrices.Matrix4 renames Matrices."*";

      function Far_Plane (Value : GL.Types.Compare_Function) return GL.Types.Compare_Function is
        (case Value is
           when Less | LEqual     => LEqual,
           when Greater | GEqual  => GEqual,
           when others            => raise Constraint_Error);

      Original_Function : constant GL.Types.Compare_Function := GL.Buffers.Depth_Function;

      use Orka.Transforms.Doubles.Vectors;
      use Orka.Transforms.Doubles.Vector_Conversions;

      use all type Orka.Transforms.Doubles.Vectors.Vector4;

      Planet_To_Camera : constant Vector4 := Camera.View_Position - Planet.Position;
      Planet_To_Star   : constant Vector4 := Star.Position - Planet.Position;
      Camera_To_Star   : constant Vector4 := Star.Position - Camera.View_Position;

      procedure Apply_Hacks is
         GL_To_Geo : constant Matrices.Matrix4 := Matrices.R
           (Matrices.Vectors.Normalize ((1.0, 1.0, 1.0, 0.0)),
            (2.0 / 3.0) * Ada.Numerics.Pi);
         Earth_Tilt : constant Matrices.Matrix4 := Matrices.R
           (Matrices.Vectors.Normalize ((1.0, 0.0, 0.0, 0.0)),
            Object.Parameters.Axial_Tilt);

         Inverse_Inertial    : constant Matrices.Matrix4 := Earth_Tilt * GL_To_Geo;
         Camera_Normal_Inert : constant Vector4 := Normalize (Inverse_Inertial * Planet_To_Camera);

         Actual_Surface   : constant Vector4 := Flattened_Vector
           (Object.Parameters, Camera_Normal_Inert);
         Expected_Surface : constant Vector4 := Camera_Normal_Inert * Object.Bottom_Radius;

         Offset   : constant Vector4 := Expected_Surface - Actual_Surface;
         Altitude : constant GL.Types.Double := Length (Planet_To_Camera) - Length (Actual_Surface);
      begin
         Object.Uniform_Ground_Hack.Set_Boolean (Altitude < Altitude_Hack_Threshold);
         Object.Uniform_Camera_Offset.Set_Vector (Convert (Offset * Object.Distance_Scale));
      end Apply_Hacks;
   begin
      if Object.Parameters.Flattening > 0.0 then
         Apply_Hacks;
      else
         Object.Uniform_Ground_Hack.Set_Boolean (False);
         Object.Uniform_Camera_Offset.Set_Vector (Orka.Transforms.Singles.Vectors.Zero_Point);
      end if;

      Object.Uniform_Camera_Pos.Set_Vector (Orka.Transforms.Singles.Vectors.Zero_Point);
      Object.Uniform_Planet_Pos.Set_Vector (Convert (-Planet_To_Camera * Object.Distance_Scale));

      Object.Uniform_Sun_Dir.Set_Vector (Convert (Normalize (Planet_To_Star)));
      Object.Uniform_Star_Dir.Set_Vector (Convert (Normalize (Camera_To_Star)));

      --  Use distance to star and its radius instead of the
      --  Sun_Angular_Radius of Model_Data
      declare
         Angular_Radius : constant GL.Types.Double :=
           EF.Arctan (Object.Parameters.Star_Radius, Length (Camera_To_Star));
      begin
         Object.Uniform_Star_Size.Set_Single (GL.Types.Single (EF.Cos (Angular_Radius)));
      end;

      Object.Uniform_View.Set_Matrix (Camera.View_Matrix);
      Object.Uniform_Proj.Set_Matrix (Camera.Lens.Projection_Matrix);

      Object.Uniform_Exposure.Set_Single (GL.Types.Single (Exposure * Object.Exposure_Scale));

      Object.Program.Use_Program;

      GL.Buffers.Set_Depth_Function (Far_Plane (Original_Function));
      Orka.Rendering.Drawing.Draw (GL.Types.Triangles, 0, 3);
      GL.Buffers.Set_Depth_Function (Original_Function);
   end Render;

end Orka.Features.Atmosphere.Rendering;
