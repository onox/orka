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

with GL.Pixels;
with GL.Types;

with Orka.Types;

package body Orka.Resources.Models is

   function Shapes (Object : Model) return String_Vectors.Vector is
     (Object.Shapes);

   function Create_Instance (Object : Model) return Behaviors.Behavior_Ptr is
      Shapes : Shape_Array (1 .. Positive (Object.Shapes.Length));
      pragma Assert (Shapes'First = Object.Shapes.First_Index);

      --  Set-up TBO for world transform matrices
      Transforms_Buffer : constant Buffers.Buffer := Buffers.Create_Buffer
        ((Dynamic_Storage => True, others => False),
         Orka.Types.Single_Matrix_Type, Shapes'Length);

      TBO : Buffer_Texture (GL.Low_Level.Enums.Texture_Buffer);
   begin
      for Index in Shapes'Range loop
         Shapes (Index) := Object.Scene.To_Cursor (Object.Shapes.Element (Index));
      end loop;

      TBO.Attach_Buffer (GL.Pixels.RGBA32F, Transforms_Buffer.GL_Buffer);

      return new Model_Instance'
        (Scene   => Object.Scene,
         Shapes  => Shape_Array_Holder.To_Holder (Shapes),
         Format  => Object.Format,
         Batch   => Object.Batch,
         TBO     => TBO,
         Transforms => Transforms_Buffer,
         Uniform_WT => Object.Uniform_WT);
   end Create_Instance;

   function Scene_Tree (Object : in out Model_Instance) return Trees.Tree is
     (Object.Scene);

   overriding
   procedure Render (Object : in out Model_Instance) is
      World_Transforms : Orka.Types.Singles.Matrix4_Array (1 .. GL.Types.Int (Object.Shapes.Element'Length));
      pragma Assert (Positive (World_Transforms'First) = Object.Shapes.Element'First);
   begin
      Object.Uniform_WT.Set_Texture (Object.TBO, 0);

      --  Compute the world transforms by multiplying the local transform
      --  of each node with the world transform of its parent.
      Object.Scene.Update_Transforms;

      for Index in Object.Shapes.Element'Range loop
         World_Transforms (GL.Types.Int (Index)) := Object.Scene.World_Transform (Object.Shapes.Element (Index));
      end loop;

      --  TODO In a loop we should use a persistent mapped buffer instead of Set_Data
      Object.Transforms.Set_Data (World_Transforms);

      --  TODO Only do this once per model, not for each instance
      Object.Format.Set_Vertex_Buffer (1, Object.Batch.Positions);
      Object.Format.Set_Vertex_Buffer (2, Object.Batch.Normals);
      Object.Format.Set_Vertex_Buffer (3, Object.Batch.UVs);
      Object.Format.Set_Vertex_Buffer (4, Object.Batch.Instances);
      Object.Format.Set_Index_Buffer (Object.Batch.Indices);

      Object.Format.Draw_Indirect (Object.Batch.Commands);
   end Render;

end Orka.Resources.Models;
