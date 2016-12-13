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

with GL.Buffers;
with GL.Objects.Buffers;
with GL.Types.Indirect;

with Orka.Buffers.MDI;
with Orka.Meshes.Attributes;
with Orka.Programs.Modules;

with GL_Test.Display_Backend;

procedure Orka_Test.Test_4_MDI is
   Initialized : constant Boolean := GL_Test.Display_Backend.Init
     (Major => 3, Minor => 2, Width => 500, Height => 500, Resizable => False);
   pragma Unreferenced (Initialized);

   use Orka.Meshes;
   use Orka.Programs;
   use GL.Objects.Buffers;

   function Load_Mesh (Program : Orka.Programs.Program;
                       MDI_Buffers : out Orka.Buffers.MDI.MDI_Buffers) return Mesh is
      use GL.Types;

      Vertices_1 : constant Indirect.Single_Array_Access
        := new Single_Array'(-0.3,  0.5, -1.0,
                             -0.8, -0.5, -1.0,
                              0.2, -0.5, -1.0);

      Vertices_2 : constant Indirect.Single_Array_Access
        := new Single_Array'(-0.2,  0.5, -1.0,
                              0.3, -0.5, -1.0,
                              0.8,  0.5, -1.0);

      Indices_1 : constant Indirect.UInt_Array_Access
        := new UInt_Array'(0, 1, 2);

      Indices_2 : constant Indirect.UInt_Array_Access
        := new UInt_Array'(0, 1, 2);

      --  A blue triangle on the left and a yellow triangle on the right
      Instances_IDs : constant UInt_Array
        := (2, 3);

      Instances_Buffer : Orka.Buffers.Buffer := Orka.Buffers.Create_Buffer (Static_Draw);
      MDI : Orka.Buffers.MDI.Batch := Orka.Buffers.MDI.Create_Batch (3);

      Instance_ID : Natural;
   begin
      Instances_Buffer.Set_Data (Instances_IDs);

      --  Create one VBO, one IBO, and a buffer containing the draw commands
      MDI.Append (Vertices_1, Indices_1, Instance_ID);
      MDI.Append (Vertices_2, Indices_2, Instance_ID);
      pragma Assert (MDI.Length = 2);
      MDI_Buffers := MDI.Create_Buffers (Static_Draw);

      --  Create mesh and its attributes
      return Result : Mesh := Orka.Meshes.Create_Mesh (Triangles) do
         declare
            Attributes_Pos : Attributes.Attribute_Buffer := Result.Add_Attribute_Buffer (Single_Type);
            Attributes_Ins : Attributes.Attribute_Buffer := Result.Add_Attribute_Buffer (UInt_Type);
         begin
            Attributes_Pos.Add_Attribute (Program.Attribute_Location ("in_Position"), 3);
            Attributes_Pos.Set_Buffer (MDI_Buffers.Vertex_Buffer);

            Attributes_Ins.Add_Attribute (Program.Attribute_Location ("in_InstanceID"), 1);
            Attributes_Ins.Set_Per_Instance (True);
            Attributes_Ins.Set_Buffer (Instances_Buffer);
         end;
         Result.Set_Index_Buffer (MDI_Buffers.Index_Buffer);
      end return;
   end Load_Mesh;

   use GL.Buffers;

   MDI_Buffers : Orka.Buffers.MDI.MDI_Buffers;

   Program_1 : constant Program := Orka.Programs.Create_Program (Modules.Create_Module
     (VS => "../examples/orka/shaders/test-4-module-1.vert",
      FS => "../examples/orka/shaders/test-4-module-1.frag"));

   Mesh_1 : constant Mesh := Load_Mesh (Program_1, MDI_Buffers);
begin
   Program_1.Use_Program;

   while not GL_Test.Display_Backend.Get_Window.Should_Close loop
      Clear (Buffer_Bits'(Color => True, Depth => True, others => False));

      Mesh_1.Draw_Indirect (MDI_Buffers.Command_Buffer);

      GL_Test.Display_Backend.Swap_Buffers_And_Poll_Events;
   end loop;

   GL_Test.Display_Backend.Shutdown;
end Orka_Test.Test_4_MDI;
