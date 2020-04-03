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
with Orka.Rendering.Programs.Modules;
with Orka.Resources;
with Orka.Transforms.Doubles.Vectors;
with Orka.Transforms.Doubles.Vector_Conversions;
with Orka.Transforms.Singles.Vectors;

package body Orka.Features.Atmosphere.Rendering is

   package EF is new Ada.Numerics.Generic_Elementary_Functions (GL.Types.Double);

   function Convert (Bytes : Resources.Byte_Array) return String renames Resources.Convert;

   Default_Exposure : constant := 10.0;

   function Create_Atmosphere
     (Data     : not null access constant Model_Data;
      Location : Resources.Locations.Location_Ptr) return Atmosphere
   is
      Atmosphere_Model : constant Model := Create_Model (Data, Location);

      Sky_GLSL : constant String
        := Convert (Orka.Resources.Byte_Array'(Location.Read_Data ("sky.frag").Get));

      use Ada.Characters.Latin_1;
      use Rendering.Programs;
      use Rendering.Programs.Modules;

      Sky_Shader : constant String :=
        "#version 420 core" & LF &
        (if Data.Luminance /= None then
           "#define USE_LUMINANCE" & LF
         else "") &
        "const float kLengthUnitInMeters = " & Data.Length_Unit_In_Meters'Image & ";" & LF &
        Sky_GLSL & LF;
   begin
      return Result : Atmosphere :=
        (Program => Create_Program (Modules.Module_Array'
           (Modules.Create_Module (Location, VS => "sky.vert"),
            Modules.Create_Module_From_Sources (FS => Sky_Shader),
            Atmosphere_Model.Get_Shader)),
         Distance_Scale => 1.0 / Data.Length_Unit_In_Meters,
         Exposure_Scale => Default_Exposure * (if Data.Luminance /= None then 1.0e-5 else 1.0),
         others => <>)
      do
         Result.Uniform_Camera_Offset := Result.Program.Uniform ("camera_offset");

         Result.Uniform_Camera_Position := Result.Program.Uniform ("camera_pos");
         Result.Uniform_Planet_Position := Result.Program.Uniform ("planet_pos");

         Result.Uniform_View := Result.Program.Uniform ("view");
         Result.Uniform_Proj := Result.Program.Uniform ("proj");

         Result.Uniform_Sun_Direction := Result.Program.Uniform ("sun_direction");
         Result.Uniform_Exposure      := Result.Program.Uniform ("exposure");

         Result.Program.Uniform ("sun_size").Set_Single
           (GL.Types.Single (EF.Cos (Data.Sun_Angular_Radius)));
      end return;
   end Create_Atmosphere;

   procedure Render
     (Object : in out Atmosphere;
      Camera : Cameras.Camera_Ptr;
      Planet : Behaviors.Behavior_Ptr;
      Sun    : Behaviors.Behavior_Ptr;
      Exposure : Exposure_Type := 1.0;
      Offset   : Cameras.Transforms.Vector4 := Cameras.Transforms.Vectors.Zero_Point)
   is
      function Far_Plane (Value : GL.Types.Compare_Function) return GL.Types.Compare_Function is
        (case Value is
           when Less | LEqual     => LEqual,
           when Greater | GEqual  => GEqual,
           when others            => raise Constraint_Error);

      Original_Function : constant GL.Types.Compare_Function := GL.Buffers.Depth_Function;

      use Orka.Transforms.Doubles.Vectors;
      use Orka.Transforms.Doubles.Vector_Conversions;

      Camera_To_Planet : constant Cameras.Transforms.Vector4 :=
        Convert ((Planet.Position - Camera.View_Position) * Object.Distance_Scale);
      Planet_To_Sun_Direction : constant Cameras.Transforms.Vector4 :=
        Convert (Normalize (Sun.Position - Planet.Position));
   begin
      Object.Uniform_Camera_Offset.Set_Vector (Offset);

      Object.Uniform_Camera_Position.Set_Vector (Orka.Transforms.Singles.Vectors.Zero_Point);
      Object.Uniform_Planet_Position.Set_Vector (Camera_To_Planet);

      Object.Uniform_Sun_Direction.Set_Vector (Planet_To_Sun_Direction);

      Object.Uniform_View.Set_Matrix (Camera.View_Matrix);
      Object.Uniform_Proj.Set_Matrix (Camera.Lens.Projection_Matrix);

      Object.Uniform_Exposure.Set_Single (GL.Types.Single (Exposure * Object.Exposure_Scale));

      Object.Program.Use_Program;

      GL.Buffers.Set_Depth_Function (Far_Plane (Original_Function));
      Orka.Rendering.Drawing.Draw (GL.Types.Triangles, 0, 3);
      GL.Buffers.Set_Depth_Function (Original_Function);
   end Render;

end Orka.Features.Atmosphere.Rendering;
