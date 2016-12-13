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

with Ada.Command_Line;
with Ada.Real_Time;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with GL.Buffers;
with GL.Objects.Programs.Uniforms;
with GL.Toggles;
with GL.Types;

with Orka.Programs.Modules;
with Orka.Resources.Models.glTF;
with Orka.Scenes.Singles.Trees;
with Orka.Transforms.Singles.Matrices;

with GL_Test.Display_Backend;

procedure Orka_Test.Test_6_GLTF is
   Initialized : constant Boolean := GL_Test.Display_Backend.Init
     (Major => 3, Minor => 2, Width => 1000, Height => 1000, Resizable => False);
   pragma Unreferenced (Initialized);

   package Models is new Orka.Resources.Models (Orka.Scenes.Singles.Trees);
   package glTF is new Models.glTF;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("Usage: <path to .gltf file>");
      return;
   end if;

   declare
      use type Ada.Real_Time.Time;

      A : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
      M : constant Models.Model := glTF.Load_Model (Ada.Command_Line.Argument (1));
      B : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

      use Orka.Programs;

      Program_1 : constant Program := Orka.Programs.Create_Program (Modules.Create_Module
        (VS => "../examples/orka/shaders/test-6-module-1.vert",
         FS => "../examples/orka/shaders/test-6-module-1.frag"));

      use GL.Objects.Programs.Uniforms;
      Uni_Model : constant Uniform := Program_1.GL_Program.Uniform_Location ("model");
      Uni_View  : constant Uniform := Program_1.GL_Program.Uniform_Location ("view");
      Uni_Proj  : constant Uniform := Program_1.GL_Program.Uniform_Location ("proj");
      Uni_Light : constant Uniform := Program_1.GL_Program.Uniform_Location ("lightPosition");

      Mouse_X, Mouse_Y, Mouse_Z : GL.Types.Single;
      use Orka.Transforms.Singles.Matrices;
      use type GL.Types.Single;

      Matrix_Proj : constant Matrix4 := Infinite_Perspective (45.0, 1.0, 0.1);
      Matrix_View : Matrix4;
      Vector_View : Vector4;

      function Convert_Matrix is new Ada.Unchecked_Conversion
        (Source => Orka.Transforms.Singles.Matrices.Matrix4, Target => GL.Types.Singles.Matrix4);
      function Convert_Vector is new Ada.Unchecked_Conversion
        (Source => Orka.Transforms.Singles.Matrices.Vector4, Target => GL.Types.Singles.Vector4);

      use GL.Buffers;
   begin
      Ada.Text_IO.Put_Line ("Duration gltf: " & Duration'Image (1e3 * Ada.Real_Time.To_Duration (B - A)));

      Uni_Model.Set_Single_Matrix (Convert_Matrix (Identity_Value));
      Uni_Proj.Set_Single_Matrix (Convert_Matrix (Matrix_Proj));

      GL.Toggles.Enable (GL.Toggles.Depth_Test);
      Program_1.Use_Program;

      while not GL_Test.Display_Backend.Get_Window.Should_Close loop
         Mouse_X := GL.Types.Single (GL_Test.Display_Backend.Get_Mouse_X);
         Mouse_Y := GL.Types.Single (GL_Test.Display_Backend.Get_Mouse_Y);

         if GL_Test.Display_Backend.Get_Zoom_Distance > 20.0 then
            GL_Test.Display_Backend.Set_Zoom_Distance (20.0);
         end if;
         Mouse_Z := GL.Types.Single (GL_Test.Display_Backend.Get_Zoom_Distance);

         Clear (Buffer_Bits'(Color => True, Depth => True, others => False));

         Matrix_View := (0.0, 0.0, -Mouse_Z, 0.0) + Ry (Mouse_X) * Rx (Mouse_Y) * Identity_Value;
         Uni_View.Set_Single_Matrix (Convert_Matrix (Matrix_View));

         --  Set position of light in camera space
         Vector_View := Matrix_View * (-2.0, 0.0, 0.0, 1.0);
         Uni_Light.Set_Single_Vector (Convert_Vector (Vector_View));

         M.Mesh.Draw_Indirect (M.Command_Buffer);

         GL_Test.Display_Backend.Swap_Buffers_And_Poll_Events;
      end loop;
   end;

   GL_Test.Display_Backend.Shutdown;
end Orka_Test.Test_6_GLTF;
