--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 onox <denkpadje@gmail.com>
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

with Ada.Text_IO;

with GL.Types;
with GL.Toggles;

with Orka.Cameras.Rotate_Around_Cameras;
with Orka.Contexts;
with Orka.Rendering.Buffers;
with Orka.Rendering.Drawing;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Programs.Uniforms;
with Orka.Resources.Locations.Directories;
with Orka.Transforms.Doubles.Vector_Conversions;
with Orka.Transforms.Singles.Matrices;
with Orka.Types;
with Orka.Windows.GLFW;

--  In this example we render many instances of a cube, each at a different
--  position.

procedure Orka_Test.Test_11_Instancing is
   Width  : constant := 500;
   Height : constant := 500;

   Library : constant Orka.Contexts.Library'Class
     := Orka.Windows.GLFW.Initialize (Major => 4, Minor => 2);

   Window : aliased Orka.Windows.Window'Class
     := Library.Create_Window (Width => Width, Height => Height, Resizable => False);

   Context : constant Orka.Contexts.Context'Class := Window.Context;

   use Orka.Rendering.Buffers;
   use Orka.Rendering.Framebuffers;
   use Orka.Rendering.Programs;
   use GL.Types;

   Instances_Dimension : constant := 40;
   Space_Between_Cubes : constant := 0.2;

   function Create_Matrices return Orka.Types.Singles.Matrix4_Array is
      package Transforms renames Orka.Transforms.Singles.Matrices;

      Distance_Multiplier : constant := 1.0 + Space_Between_Cubes;

      Matrices : Orka.Types.Singles.Matrix4_Array (1 .. Instances_Dimension**3);

      Index : Int := Matrices'First;
      X, Y, Z : Single;

      Offset : constant := Instances_Dimension / 2;
   begin
      for Index_X in 1 .. Instances_Dimension loop
         X := Single (Index_X - Offset) * Distance_Multiplier;
         for Index_Y in 1 .. Instances_Dimension loop
            Y := Single (Index_Y - Offset) * Distance_Multiplier;
            for Index_Z in 1 .. Instances_Dimension loop
               Z := Single (Index_Z - Offset) * Distance_Multiplier;

               Matrices (Index) := Transforms.T ((X, Y, Z, 0.0));
               Index := Index + 1;
            end loop;
         end loop;
      end loop;
      return Matrices;
   end Create_Matrices;

   Indices : constant UInt_Array
     := (1, 2, 0,  --  Back
         0, 2, 3,
         1, 0, 5,  --  Top
         5, 0, 4,
         5, 4, 6,  --  Front
         6, 4, 7,
         1, 5, 2,  --  Right
         2, 5, 6,
         7, 4, 3,  --  Left
         3, 4, 0,
         3, 2, 7,  --  Bottom
         7, 2, 6);

   Vertices : constant Single_Array
     := (-0.5,  0.5, -0.5, 1.0,    1.0, 1.0, 1.0, 1.0,
          0.5,  0.5, -0.5, 1.0,    0.0, 1.0, 0.0, 1.0,
          0.5, -0.5, -0.5, 1.0,    0.0, 0.0, 1.0, 1.0,
         -0.5, -0.5, -0.5, 1.0,    1.0, 0.0, 0.0, 1.0,

         -0.5,  0.5,  0.5, 1.0,    0.0, 0.0, 1.0, 1.0,
          0.5,  0.5,  0.5, 1.0,    1.0, 0.0, 0.0, 1.0,
          0.5, -0.5,  0.5, 1.0,    1.0, 1.0, 1.0, 1.0,
         -0.5, -0.5,  0.5, 1.0,    0.0, 1.0, 0.0, 1.0);
   --  vec4 in_Position
   --  vec4 in_Color

   Matrices : constant Orka.Types.Singles.Matrix4_Array := Create_Matrices;

   --  Create buffers containing attributes and indices
   Buffer_1 : constant Buffer := Create_Buffer ((others => False), Vertices);
   Buffer_2 : constant Buffer := Create_Buffer ((others => False), Indices);
   Buffer_3 : constant Buffer := Create_Buffer ((others => False), Matrices);

   use Orka.Resources;

   Location_Shaders : constant Locations.Location_Ptr
     := Locations.Directories.Create_Location ("../examples/gl/shaders");

   Program_1 : Program := Create_Program (Modules.Create_Module
     (Location_Shaders, VS => "instancing.vert", FS => "instancing.frag"));

   Uni_View  : constant Uniforms.Uniform := Program_1.Uniform ("view");
   Uni_Proj  : constant Uniforms.Uniform := Program_1.Uniform ("proj");

   FB_D : Framebuffer := Create_Default_Framebuffer (Width, Height);

   use Orka.Cameras;
   Lens : constant Lens_Ptr
     := new Camera_Lens'Class'(Create_Lens (Width, Height, 45.0, Context));
   Current_Camera : constant Camera_Ptr
     := new Camera'Class'(Camera'Class
          (Rotate_Around_Cameras.Create_Camera (Window.Pointer_Input, Lens, FB_D)));
begin
   Ada.Text_IO.Put_Line ("Instances of cube: " & Positive'Image (Matrices'Length));

   declare
      Distance_Center : Double := Double (Instances_Dimension);
   begin
      Distance_Center := Distance_Center + (Distance_Center - 1.0) * Space_Between_Cubes;

      Rotate_Around_Cameras.Rotate_Around_Camera (Current_Camera.all).Set_Radius
        (1.5 * Distance_Center);
   end;

   FB_D.Set_Default_Values ((Color => (0.0, 0.0, 0.0, 1.0), Depth => 1.0, others => <>));

   Program_1.Use_Program;

   --  Projection matrix
   Uni_Proj.Set_Matrix (Current_Camera.Projection_Matrix);

   GL.Toggles.Enable (GL.Toggles.Cull_Face);
   GL.Toggles.Enable (GL.Toggles.Depth_Test);

   Buffer_1.Bind (Shader_Storage, 0);
   Buffer_3.Bind (Shader_Storage, 1);

   while not Window.Should_Close loop
      Window.Process_Input;

      Current_Camera.Update (0.01666);
      declare
         VP : constant Transforms.Vector4 :=
           Orka.Transforms.Doubles.Vector_Conversions.Convert (Current_Camera.View_Position);

         use Transforms.Vectors;
         use Transforms;

         TM : constant Transforms.Matrix4 := Transforms.T (Transforms.Zero_Point - VP);
      begin
         Uni_View.Set_Matrix (Current_Camera.View_Matrix * TM);
      end;

      FB_D.Clear ((Color | Depth => True, others => False));

      Orka.Rendering.Drawing.Draw_Indexed (Triangles, Buffer_2, 0, Indices'Length, Matrices'Length);

      --  Swap front and back buffers and process events
      Window.Swap_Buffers;
   end loop;
end Orka_Test.Test_11_Instancing;
