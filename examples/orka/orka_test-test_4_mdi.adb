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
with Orka.Meshes;
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
                       MDI_Buffers : out Orka.Buffers.MDI.MDI_Buffers) return Vertex_Format is
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

      MDI : Orka.Buffers.MDI.Batch := Orka.Buffers.MDI.Create_Batch (3);
      Instance_ID : Natural;

      procedure Add_Position_Attribute (Buffer : in out Orka.Meshes.Attribute_Buffer) is
      begin
         Buffer.Add_Attribute (Program.Attribute_Location ("in_Position"), 3);
      end Add_Position_Attribute;

      procedure Add_Instance_Attribute (Buffer : in out Orka.Meshes.Attribute_Buffer) is
      begin
         Buffer.Add_Attribute (Program.Attribute_Location ("in_InstanceID"), 1);
         Buffer.Set_Per_Instance (True);
      end Add_Instance_Attribute;
   begin
      --  Create one VBO, one IBO, and a buffer containing the draw commands
      MDI.Append (Vertices_1, Indices_1, Instance_ID);
      MDI.Append (Vertices_2, Indices_2, Instance_ID);
      pragma Assert (MDI.Length = 2);
      MDI_Buffers := MDI.Create_Buffers (Storage_Bits'(others => False));

      --  Create mesh and its attributes
      return Result : Vertex_Format := Orka.Meshes.Create_Vertex_Format (Triangles) do
         Result.Add_Attribute_Buffer (Half_Type, Add_Position_Attribute'Access);
         Result.Add_Attribute_Buffer (UInt_Type, Add_Instance_Attribute'Access);

         Result.Set_Vertex_Buffer (1, MDI_Buffers.Vertex_Buffer);
         Result.Set_Vertex_Buffer (2, MDI_Buffers.Instances_Buffer);

         Result.Set_Index_Buffer (MDI_Buffers.Index_Buffer);
      end return;
   end Load_Mesh;

   use GL.Buffers;

   MDI_Buffers : Orka.Buffers.MDI.MDI_Buffers;

   Program_1 : Program := Orka.Programs.Create_Program (Modules.Create_Module
     (VS => "../examples/orka/shaders/test-4-module-1.vert",
      FS => "../examples/orka/shaders/test-4-module-1.frag"));

   Mesh_1 : constant Vertex_Format := Load_Mesh (Program_1, MDI_Buffers);
begin
   Program_1.Use_Program;

   while not GL_Test.Display_Backend.Get_Window.Should_Close loop
      Clear (Buffer_Bits'(Color => True, Depth => True, others => False));

      Mesh_1.Draw_Indirect (MDI_Buffers.Command_Buffer);

      GL_Test.Display_Backend.Swap_Buffers_And_Poll_Events;
   end loop;

   GL_Test.Display_Backend.Shutdown;
end Orka_Test.Test_4_MDI;
