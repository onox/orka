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

with Ada.Real_Time;
with Ada.Text_IO;

with GL.Barriers;
with GL.Compute;
with GL.Types.Compute;

with Orka.Contexts;
with Orka.Debug;
with Orka.Rendering.Buffers;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Programs.Uniforms;
with Orka.Resources.Locations.Directories;
with Orka.Types;

with Orka.Windows.GLFW;

procedure Orka_Test.Test_10_Compute is
   Context : constant Orka.Contexts.Context'Class := Orka.Windows.GLFW.Create_Context
     (Version => (4, 2), Flags  => (Debug => True, others => False));

   Window : constant Orka.Windows.Window'Class
     := Orka.Windows.GLFW.Create_Window (Context, Width => 1, Height => 1, Visible => False);
   pragma Unreferenced (Window);

   ----------------------------------------------------------------------

   use Ada.Text_IO;
   use type GL.Types.Int;
   use GL.Types;

   Numbers : constant Int_Array
     := (10, 1, 8, -1, 0, -2, 3, 5, -2, -3, 2, 7, 0, 11, 0, 2);
begin
   Orka.Debug.Set_Log_Messages (Enable => True);

   declare
      use Orka.Rendering.Buffers;
      use Orka.Rendering.Programs;
      use Orka.Resources;

      Location_Shaders : constant Locations.Location_Ptr
        := Locations.Directories.Create_Location ("../examples/orka/shaders");

      Program_1 : Program := Create_Program (Modules.Create_Module
        (Location_Shaders, CS => "test-10-module-1.comp"));

      Uniform_1 : constant Uniforms.Uniform := Program_1.Uniform ("maxNumbers");

      Max_Work_Groups, Local_Size : Size;
   begin
      Program_1.Use_Program;

      --  Print some limits about compute shaders
      Put_Line ("Maximum shared size:" &
        Size'Image (GL.Compute.Max_Compute_Shared_Memory_Size));
      Put_Line ("Maximum invocations:" &
        Size'Image (GL.Compute.Max_Compute_Work_Group_Invocations));

      declare
         R : GL.Types.Compute.Dimension_Size_Array;
      begin
         R := GL.Compute.Max_Compute_Work_Group_Count;
         Put_Line ("Maximum count:" & R (GL.X)'Image & R (GL.Y)'Image & R (GL.Z)'Image);
         Max_Work_Groups := R (GL.X);

         R := GL.Compute.Max_Compute_Work_Group_Size;
         Put_Line ("Maximum size: " & R (GL.X)'Image & R (GL.Y)'Image & R (GL.Z)'Image);

         R := Program_1.Compute_Work_Group_Size;
         Put_Line ("Local size:   " & R (GL.X)'Image & R (GL.Y)'Image & R (GL.Z)'Image);
         Local_Size := R (GL.X);
      end;

      declare
         use all type Orka.Types.Element_Type;
         use type Ada.Real_Time.Time;

         Factor : constant Size := (Max_Work_Groups * Local_Size) / Numbers'Length;

         Buffer_1 : constant Buffer := Create_Buffer
           (Flags  => (Dynamic_Storage => True, others => False),
            Kind   => Int_Type,
            Length => Numbers'Length * Natural (Factor));

         A, B : Ada.Real_Time.Time;

         procedure Memory_Barrier is
         begin
            GL.Barriers.Memory_Barrier ((Shader_Storage | Buffer_Update => True, others => False));
         end Memory_Barrier;
      begin
         Put_Line ("Factor:" & Factor'Image);

         --  Upload numbers to SSBO
         for Index in 0 .. Factor - 1 loop
            Buffer_1.Set_Data (Data => Numbers, Offset => Numbers'Length * Natural (Index));
         end loop;

         Buffer_1.Bind (Shader_Storage, 0);

         A := Ada.Real_Time.Clock;
         declare
            Count : constant Size := Size (Buffer_1.Length);

            Ceiling : Size := Count + (Count rem Local_Size);
            Groups  : Size := Ceiling / Local_Size;
         begin
            Put_Line ("Numbers:" & Count'Image);
            Put_Line ("Groups: " & Groups'Image);
            pragma Assert (Groups <= Max_Work_Groups);

            --  The uniform is used to set how many numbers need to be
            --  summed. If a work group has more threads than there are
            --  numbers to be summed (happens in the last iteration),
            --  then these threads will use the number 0 in the shader.
            Uniform_1.Set_UInt (UInt (Count));

            while Groups > 0 loop
               --  Add an SSBO barrier for the next iteration
               --  and then dispatch the compute shader
               Memory_Barrier;
               GL.Compute.Dispatch_Compute (X => UInt (Groups));

               Uniform_1.Set_UInt (UInt (Groups));
               Ceiling := Groups + (Groups rem Local_Size);
               Groups  := Ceiling / Local_Size;
            end loop;

            --  Perform last iteration. Work groups in X dimension needs
            --  to be at least one.
            Memory_Barrier;
            GL.Compute.Dispatch_Compute (X => UInt (Size'Max (1, Groups)));
         end;

         Memory_Barrier;

         declare
            Output : Int_Array (1 .. 2) := (others => 0);
         begin
            Buffer_1.Get_Data (Output);
            Put_Line ("Expected Sum:" & Size'Image (Factor * 41));
            Put_Line ("Computed sum:" & Output (Output'First)'Image);

            --  Print the number of shader invocations that execute in
            --  lockstep. This requires the extension ARB_shader_ballot
            --  in the shader.
            Put_Line ("Sub-group size:" & Output (Output'Last)'Image);
         end;
         B := Ada.Real_Time.Clock;
         Put_Line (Duration'Image (1e3 * Ada.Real_Time.To_Duration (B - A)) & " ms");
      end;
   end;
end Orka_Test.Test_10_Compute;
