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

with GL.Drawing;
with GL.Pixels;
with GL.Types;
with GL.Toggles;

with Orka.Rendering.Buffers;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Programs.Uniforms;
with Orka.Rendering.Vertex_Formats;
with Orka.Resources.Locations.Directories;
with Orka.Transforms.Singles.Matrices;
with Orka.Types;

with GL_Test.Display_Backend;

procedure Orka_Test.Test_11_Instancing is
   Initialized : constant Boolean := GL_Test.Display_Backend.Init
     (Major => 3, Minor => 3, Width => 500, Height => 500, Resizable => False);
   pragma Unreferenced (Initialized);

   package Transforms renames Orka.Transforms.Singles.Matrices;

   use Orka.Rendering.Buffers;
   use Orka.Rendering.Framebuffers;
   use Orka.Rendering.Vertex_Formats;
   use Orka.Rendering.Programs;
   use GL.Types;

   Instances_Dimension : constant := 40;
   Space_Between_Cubes : constant := 0.2;

   function Create_Matrices return Orka.Types.Singles.Matrix4_Array is
      Distance_Multiplier : constant := 1.0 + Space_Between_Cubes;

      Matrices : Orka.Types.Singles.Matrix4_Array (1 .. Instances_Dimension**3);

      Index : Int := Matrices'First;
      X, Y, Z : Single;
   begin
      for Index_X in 1 .. Instances_Dimension loop
         X := Single (Index_X) * Distance_Multiplier;
         for Index_Y in 1 .. Instances_Dimension loop
            Y := Single (Index_Y) * Distance_Multiplier;
            for Index_Z in 1 .. Instances_Dimension loop
               Z := Single (Index_Z) * Distance_Multiplier;

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

   Matrices : constant Orka.Types.Singles.Matrix4_Array := Create_Matrices;

   function Load_Mesh (Program : Orka.Rendering.Programs.Program) return Vertex_Format is
      use all type Orka.Types.Element_Type;

      Vertices : constant Single_Array
        := (-0.5,  0.5, -0.5, 1.0, 1.0, 1.0,
             0.5,  0.5, -0.5, 0.0, 1.0, 0.0,
             0.5, -0.5, -0.5, 0.0, 0.0, 1.0,
            -0.5, -0.5, -0.5, 1.0, 0.0, 0.0,

            -0.5,  0.5,  0.5, 0.0, 0.0, 1.0,
             0.5,  0.5,  0.5, 1.0, 0.0, 0.0,
             0.5, -0.5,  0.5, 1.0, 1.0, 1.0,
            -0.5, -0.5,  0.5, 0.0, 1.0, 0.0);

      --  Create buffers containing attributes and indices
      Buffer_1 : constant Buffer := Create_Buffer ((others => False), Vertices);
      Buffer_2 : constant Buffer := Create_Buffer ((others => False), Indices);
      Buffer_3 : constant Buffer := Create_Buffer ((others => False), Matrices);

      procedure Add_Vertex_Attributes (Buffer : in out Attribute_Buffer) is
      begin
         Buffer.Add_Attribute (Program.Attribute_Location ("in_Position"), 3);
         Buffer.Add_Attribute (Program.Attribute_Location ("in_Color"), 3);
         Buffer.Set_Buffer (Buffer_1);
      end Add_Vertex_Attributes;

      procedure Add_Matrix_Attribute (Buffer : in out Attribute_Buffer) is
      begin
         Buffer.Add_Attribute (Program.Attribute_Location ("in_Model") + 0, 4);
         Buffer.Add_Attribute (Program.Attribute_Location ("in_Model") + 1, 4);
         Buffer.Add_Attribute (Program.Attribute_Location ("in_Model") + 2, 4);
         Buffer.Add_Attribute (Program.Attribute_Location ("in_Model") + 3, 4);
         Buffer.Set_Per_Instance (True);  --  An alternative is to bind Buffer_3 as an SSBO
         Buffer.Set_Buffer (Buffer_3);
      end Add_Matrix_Attribute;
   begin
      Ada.Text_IO.Put_Line ("Instances of cube: " & Positive'Image (Matrices'Length));

      --  Create mesh and its attributes
      return Result : Vertex_Format := Create_Vertex_Format (Triangles, UInt_Type) do
         Result.Add_Attribute_Buffer (Single_Type, Add_Vertex_Attributes'Access);
         Result.Add_Attribute_Buffer (Single_Type, Add_Matrix_Attribute'Access);
         Result.Set_Index_Buffer (Buffer_2);
      end return;
   end Load_Mesh;

   use Orka.Resources;

   Location_Shaders : constant Locations.Location_Ptr
     := Locations.Directories.Create_Location ("../examples/gl/shaders");

   Program_1 : Program := Create_Program (Modules.Create_Module
     (Location_Shaders, VS => "instancing.vert", FS => "instancing.frag"));

   Uni_View  : constant Uniforms.Uniform := Program_1.Uniform ("view");
   Uni_Proj  : constant Uniforms.Uniform := Program_1.Uniform ("proj");

   Triangle : constant Vertex_Format := Load_Mesh (Program_1);

   Default_FB : constant Framebuffer := Create_Default_Framebuffer (500, 500);

   Mouse_X, Mouse_Y, Mouse_Z, Distance_Center : Single;
begin
   Distance_Center := Single (Instances_Dimension);
   Distance_Center := Distance_Center + (Distance_Center - 1.0) * Space_Between_Cubes;
   Distance_Center := Distance_Center / 2.0 - 0.5 + 1.2;

   GL_Test.Display_Backend.Set_Zoom_Distance (100.0);

   Program_1.Use_Program;

   --  Projection matrix
   Uni_Proj.Set_Matrix (Transforms.Infinite_Perspective (45.0, 1.0, 10.0));

   GL.Toggles.Enable (GL.Toggles.Cull_Face);
   GL.Toggles.Enable (GL.Toggles.Depth_Test);

   Triangle.GL_Vertex_Array.Bind;

   while not GL_Test.Display_Backend.Get_Window.Should_Close loop
      Default_FB.GL_Framebuffer.Clear_Color_Buffer (0, GL.Pixels.Float_Type, (0.0, 0.0, 0.0, 1.0));
      Default_FB.GL_Framebuffer.Clear_Depth_Buffer (1.0);

      Mouse_X := Single (GL_Test.Display_Backend.Get_Mouse_X);
      Mouse_Y := Single (GL_Test.Display_Backend.Get_Mouse_Y);
      Mouse_Z := Single (GL_Test.Display_Backend.Get_Zoom_Distance);

      --  View matrix
      declare
         Matrix_View : Transforms.Matrix4 := Transforms.Identity_Value;
      begin
         Transforms.Translate (Matrix_View,
           (-Distance_Center, -Distance_Center, -Distance_Center, 0.0));

         Transforms.Rotate_Y_At_Origin (Matrix_View, Mouse_X);
         Transforms.Rotate_X_At_Origin (Matrix_View, Mouse_Y);
         Transforms.Translate (Matrix_View, (0.0, 0.0, -Mouse_Z, 0.0));

         Uni_View.Set_Matrix (Matrix_View);
      end;

      GL.Drawing.Draw_Elements (Triangles, Indices'Length, UInt_Type, Matrices'Length, 0);

      GL_Test.Display_Backend.Swap_Buffers_And_Poll_Events;
   end loop;

   GL_Test.Display_Backend.Shutdown;
end Orka_Test.Test_11_Instancing;
