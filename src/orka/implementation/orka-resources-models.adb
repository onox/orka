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

with GL.Barriers;
with GL.Types;

with Orka.Transforms.Doubles.Vectors;
with Orka.Transforms.Doubles.Vector_Conversions;
with Orka.Rendering.Drawing;

package body Orka.Resources.Models is

   function Create_Group
     (Object   : aliased in out Model;
      Culler   : Culling.Culler_Ptr;
      Capacity : Positive) return Group_Access
   is
      Shapes_Count : constant Natural := Object.Scene.Shapes.Element'Length;
   begin
      return new Model_Group'
        (Model     => Object'Access,
         Instances => Model_Instances.Create_Manager
           (Capacity => Capacity, Parts => Shapes_Count),
         Cull_Instance => Culling.Create_Instance
           (Culler, Transforms => Capacity * Shapes_Count, Commands => Shapes_Count),
         others => <>);
   end Create_Group;

   procedure Add_Instance
     (Object   : access Model_Group;
      Instance : in out Model_Instance_Ptr) is
   begin
      if Instance.Group /= null then
         raise Program_Error;
      end if;

      Instance.Group := Object;
      Instance.Scene := Object.Model.Scene.Scene;
      Instance.Instance := Object.Instances.Add_Instance;
   end Add_Instance;

   procedure Remove_Instance
     (Object   : in out Model_Group;
      Instance : in out Model_Instance_Ptr) is
   begin
      if Instance.Group = null then
         raise Program_Error;
      end if;

      Instance.Group := null;
      Object.Instances.Remove_Instance (Instance.Instance);
   end Remove_Instance;

   -----------------------------------------------------------------------------

   procedure Cull (Object : in out Model_Group) is
   begin
      Object.Cull_Instance.Cull
        (Transforms => Object.Instances.Transforms,
         Bounds     => Object.Model.Bounds,
         Commands   => Object.Model.Batch.Commands.Buffer,
         Compacted_Transforms => Object.Compacted_Transforms,
         Compacted_Commands   => Object.Compacted_Commands,
         Instances => Object.Instances.Length);
   end Cull;

   procedure Render (Object : in out Model_Group) is
      use all type Rendering.Buffers.Indexed_Buffer_Target;
   begin
      Object.Model.Batch.Data.Bind (Shader_Storage, 1);
      Object.Compacted_Transforms.Bind (Shader_Storage, 0);

      GL.Barriers.Memory_Barrier
        ((By_Region => False, Shader_Storage | Command => True, others => False));

      Rendering.Drawing.Draw_Indexed_Indirect
        (GL.Types.Triangles, Object.Model.Batch.Indices.Buffer, Object.Compacted_Commands);
   end Render;

   procedure After_Render (Object : in out Model_Group) is
   begin
      Object.Instances.Complete_Frame;
   end After_Render;

   -----------------------------------------------------------------------------

   procedure Update_Transforms
     (Object : in out Model_Instance;
      View_Position : Behaviors.Vector4)
   is
      use Transforms;
      use Orka.Transforms.Doubles.Vectors;
      use Orka.Transforms.Doubles.Vector_Conversions;

      pragma Assert (Object.Group /= null);

      Position : Behaviors.Vector4
        renames Behaviors.Behavior'Class (Object).Position;

      procedure Write_Transforms (Cursors : Cursor_Array) is
      begin
         for Index in Cursors'Range loop
            Object.Group.Instances.Set_Transform
              (Value    => Object.Scene.World_Transform (Cursors (Index)),
               Instance => Object.Instance,
               Part     => Index - Cursors'First);
         end loop;
      end Write_Transforms;
   begin
      --  Compute the world transforms by multiplying the local transform
      --  of each node with the world transform of its parent. Also updates
      --  the visibility of each node.
      Object.Scene.Update_Tree (T (Convert (Position - View_Position)));

      --  Write the world transform of the leaf nodes to the persistent mapped buffer
      Object.Group.Model.Scene.Shapes.Query_Element (Write_Transforms'Access);
      --  Note: This requires that the structure of the model's scene tree is
      --  identical to the instance's scene so that we can re-use the cursors
   end Update_Transforms;

end Orka.Resources.Models;
