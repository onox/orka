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

with GL.Barriers;
with GL.Objects.Buffers;
with GL.Types;

with Orka.Transforms.Singles.Vectors;

package body Orka.Resources.Models is

   function Create_Instance
     (Object   : in out Model;
      Position : Behaviors.Transforms.Vector4;
      Culler   : Culling.Culler_Ptr) return Behaviors.Behavior_Ptr
   is
      Shapes_Count : constant Natural := Object.Scene.Shapes.Element'Length;

      --  Set-up a mapped buffer for world transform matrices
      Transforms_Buffer : constant PMB.Persistent_Mapped_Buffer
        := PMB.Create_Buffer
            (Orka.Types.Single_Matrix_Type, Shapes_Count, PMB.Write);
   begin
      --  Cannot use 'Access because we're returning a pointer to Model_Instance
      return new Model_Instance'
        (Model   => Object'Unchecked_Access,
         Scene   => Object.Scene.Scene,
         Transforms => Transforms_Buffer,
         Position   => Position,

         Culler        => Culler,
         Cull_Instance => Culling.Create_Instance (Culler, Shapes_Count),
         others => <>);
   end Create_Instance;

   overriding
   procedure After_Update
     (Object : in out Model_Instance;
      Delta_Time    : Duration;
      View_Position : Behaviors.Transforms.Vector4)
   is
      use Transforms;
      use type GL.Types.Single;
      Structural_Frame_To_GL : constant Trees.Matrix4 := Ry (-90.0) * Rx (-90.0);
      --  TODO Not efficient to re-compute this every frame (cannot put non-static
      --  code in preelaborated package)

      use Orka.Transforms.Singles.Vectors;

      procedure Write_Transforms (Cursors : Cursor_Array) is
      begin
         for Index in Cursors'Range loop
            Object.Transforms.Write_Data
              (Object.Scene.World_Transform (Cursors (Index)), Index - Cursors'First);
         end loop;
      end Write_Transforms;
   begin
      --  Compute the world transforms by multiplying the local transform
      --  of each node with the world transform of its parent. Also updates
      --  the visibility of each node.
      Object.Scene.Update_Tree (T (Structural_Frame_To_GL * (View_Position - Object.Position)));

      --  Write the world transform of the leaf nodes to the persistent mapped buffer
      Object.Model.Scene.Shapes.Query_Element (Write_Transforms'Access);
      --  Note: This requires that the structure of the model's scene tree is
      --  identical to the instance's scene so that we can re-use the cursors
   end After_Update;

   overriding
   procedure Cull (Object : in out Model_Instance) is
   begin
      Object.Cull_Instance.Cull
        (Transforms => Object.Transforms,
         Bounds     => Object.Model.Bounds,
         Commands   => Object.Model.Batch.Commands,
         Compacted_Transforms => Object.Compacted_Transforms,
         Compacted_Commands   => Object.Compacted_Commands,
         Instances => 1);
   end Cull;

   overriding
   procedure Render (Object : in out Model_Instance) is
      use all type Rendering.Buffers.Buffer_Target;
   begin
      --  TODO Only do this once per model, not for each instance
      Object.Model.Batch.Bind_Buffers_To (Object.Model.Format.all);

      Object.Compacted_Transforms.Bind_Base (Shader_Storage, 0);

      GL.Barriers.Memory_Barrier
        ((By_Region => False, Shader_Storage | Command => True, others => False));

      Object.Model.Format.Draw_Indirect (Object.Compacted_Commands);
   end Render;

   overriding
   procedure After_Render (Object : in out Model_Instance) is
   begin
      Object.Transforms.Advance_Index;
   end After_Render;

   overriding
   function Position (Object : Model_Instance) return Behaviors.Transforms.Vector4 is
     (Object.Position);

end Orka.Resources.Models;
