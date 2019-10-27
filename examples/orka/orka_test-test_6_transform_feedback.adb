--  SPDX-License-Identifier: Apache-2.0
--
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

with Ada.Text_IO;

with GL.Barriers;
with GL.Compute;
with GL.Types.Compute;

with Orka.Debug;
with Orka.Rendering.Buffers;
with Orka.Rendering.Programs.Modules;
with Orka.Resources.Locations.Directories;
with Orka.Types;

with Orka.Windows.GLFW;

procedure Orka_Test.Test_6_Transform_Feedback is
   Initialized : constant Orka.Windows.GLFW.Active_GLFW'Class
     := Orka.Windows.GLFW.Initialize (Major => 4, Minor => 5, Debug => True);
   pragma Unreferenced (Initialized);

   W : aliased Orka.Windows.Window'Class := Orka.Windows.GLFW.Create_Window
     (1, 1, Visible => False);
   pragma Unreferenced (W);

   ----------------------------------------------------------------------

   use GL.Types;

   use Orka.Rendering.Buffers;
   use Orka.Rendering.Programs;
   use Orka.Resources;

   use all type Orka.Types.Element_Type;

   Vertices : constant Single_Array := (1.0, 2.0, 3.0, 4.0, 5.0);

   VBO_1 : constant Buffer := Create_Buffer ((others => False), Vertices);
   VBO_2 : constant Buffer := Create_Buffer ((others => False), Single_Type, 3 * Vertices'Length);

   Location_Shaders : constant Locations.Location_Ptr
     := Locations.Directories.Create_Location ("../examples/orka/shaders");

   Program_1 : Program := Create_Program (Modules.Create_Module
     (Location_Shaders, CS => "test-6-module-1.comp"));
begin
   Orka.Debug.Set_Log_Messages (Enable => True);

   Program_1.Use_Program;

   declare
      Local_Size : constant GL.Types.Compute.Dimension_Size_Array
        := Program_1.GL_Program.Compute_Work_Group_Size;
   begin
      pragma Assert (Local_Size (GL.X) >= Size (VBO_1.Length));
   end;

   VBO_1.Bind_Base (Shader_Storage, 0);
   VBO_2.Bind_Base (Shader_Storage, 1);

   GL.Barriers.Memory_Barrier
     ((By_Region => False, Shader_Storage => True, others => False));

   GL.Compute.Dispatch_Compute (X => 1);

   GL.Barriers.Memory_Barrier
     ((By_Region => False, Buffer_Update => True, others => False));

   --  Copy the data (n primitives, each generating 3 elements) from the
   --  bound feedback buffer (VBO_1) to an array
   declare
      Data  : Single_Array (1 .. Size (VBO_2.Length)) := (others => 0.0);
   begin
      VBO_2.Get_Data (Data);
      for Element of Data loop
         Ada.Text_IO.Put_Line (Element'Image);
      end loop;
   end;
end Orka_Test.Test_6_Transform_Feedback;
