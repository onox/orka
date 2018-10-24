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

with GL.Barriers;
with GL.Compute;
with GL.Debug;
with GL.Objects.Buffers;
with GL.Types.Compute;

with Orka.Rendering.Programs.Modules;
with Orka.Types;

package body Orka.Culling is

   use GL.Debug;
   package Messages is new GL.Debug.Messages (Third_Party, Other);

   function Create_Culler return Culler is
      use Rendering.Programs;
   begin
      return Result : Culler
        := (Program_Frustum => Create_Program (Modules.Create_Module
              (CS => "../resources/cull-frustum.comp")),
            Program_Compact => Create_Program (Modules.Create_Module
              (CS => "../resources/compact-commands.comp")),
            PS_Factory => Algorithms.Prefix_Sums.Create_Factory,
            others => <>)
      do
         Result.Uniform_CF_Instances := Result.Program_Frustum.Uniform ("instances");
         Result.Uniform_VP           := Result.Program_Frustum.Uniform ("viewProj");

         Result.Uniform_CC_Instances := Result.Program_Compact.Uniform ("instances");
      end return;
   end Create_Culler;

   function Create_Instance
     (Culler : Culler_Ptr; Count : Natural) return Cull_Instance
   is
      use GL.Objects.Buffers;
      use GL.Types;
      use Rendering.Buffers;

      Work_Group_Size : constant GL.Types.Compute.Dimension_Size_Array
        := Culler.Program_Frustum.GL_Program.Compute_Work_Group_Size;

      Local_Size : constant Natural := Natural (Work_Group_Size (GL.X));
      Padding    : constant Boolean := Count rem Local_Size /= 0;

      Work_Groups : constant Natural
        := Count / Local_Size + (if Padding then 1 else 0);
      pragma Assert (Work_Groups <= 65_535);
   begin
      return Result : constant Cull_Instance
        := (Culler => Culler,
            Work_Groups => Work_Groups,
            Prefix_Sum  => Algorithms.Prefix_Sums.Prefix_Sum
              (Culler.PS_Factory.Create_Prefix_Sum (Count)),
            Buffer_Visibles => Create_Buffer
              (Flags  => (others => False),
               Kind   => UInt_Type,
               Length => Count),
            Buffer_Indices => Create_Buffer
              (Flags  => (others => False),
               Kind   => UInt_Type,
               Length => Count),
            Compacted_Transforms => Create_Buffer
              (Flags  => (others => False),
               Kind   => Types.Single_Matrix_Type,
               Length => Count),
            Compacted_Commands => Create_Buffer
              (Flags  => (others => False),
               Kind   => Types.Elements_Command_Type,
               Length => Count))
      do
         Messages.Insert (Notification, "Created culler for" & Count'Image & " transforms");
         Messages.Insert (Notification, "  cull frustum:" &
           Work_Groups'Image & " groups x" & Local_Size'Image & " transforms");
      end return;
   end Create_Instance;

   procedure Bind (Object : in out Culler; View_Projection : Transforms.Matrix4) is
   begin
      Object.Uniform_VP.Set_Matrix (View_Projection);
   end Bind;

   -----------------------------------------------------------------------------

   procedure Memory_Barrier with Inline is
   begin
      GL.Barriers.Memory_Barrier
        ((By_Region => False, Shader_Storage | Buffer_Update => True, others => False));
   end Memory_Barrier;

   use GL.Types;
   use all type Rendering.Buffers.Buffer_Target;

   procedure Cull_Frustum
     (Object     : in out Cull_Instance;
      Transforms : Rendering.Buffers.Bindable_Buffer'Class;
      Bounds     : Rendering.Buffers.Buffer) is
   begin
      Transforms.Bind_Base (Shader_Storage, 0);
      Bounds.Bind_Base (Shader_Storage, 1);

      Object.Buffer_Visibles.Bind_Base (Shader_Storage, 2);

      Object.Culler.Program_Frustum.Use_Program;

      Memory_Barrier;
      GL.Compute.Dispatch_Compute (X => UInt (Object.Work_Groups));
   end Cull_Frustum;

   procedure Compact
     (Object     : in out Cull_Instance;
      Transforms : Rendering.Buffers.Bindable_Buffer'Class;
      Commands   : Rendering.Buffers.Buffer) is
   begin
      Object.Buffer_Indices.Bind_Base (Shader_Storage, 0);
      Object.Buffer_Visibles.Bind_Base (Shader_Storage, 1);

      Transforms.Bind_Base (Shader_Storage, 2);
      Object.Compacted_Transforms.Bind_Base (Shader_Storage, 3);

      --  Buffer Commands acts as a template (with instanceCount = 0 for
      --  every draw call), copy it to Compacted_Commands so that the
      --  compute shader does not modify it
      Commands.Copy_Data (Object.Compacted_Commands, Types.Elements_Command_Type);
      Object.Compacted_Commands.Bind_Base (Shader_Storage, 4);

      Object.Culler.Program_Compact.Use_Program;

      Memory_Barrier;
      GL.Compute.Dispatch_Compute (X => UInt (Object.Work_Groups));
   end Compact;

   procedure Cull
     (Object : in out Cull_Instance;
      Transforms : Rendering.Buffers.Bindable_Buffer'Class;
      Bounds, Commands : Rendering.Buffers.Buffer;
      Compacted_Transforms, Compacted_Commands : out Rendering.Buffers.Buffer;
      Instances : Natural) is
   begin
      Object.Culler.Uniform_CF_Instances.Set_UInt (UInt (Instances));
      Object.Culler.Uniform_CC_Instances.Set_UInt (UInt (Instances));

      --  Perform frustum culling and fill Buffer_Visibles with a 1 or 0
      --  for each part depending on whether the part intersects the frustum
      Object.Cull_Frustum (Transforms, Bounds);

      --  Store the prefix sum of Buffer_Visibles in Buffer_Indices
      Memory_Barrier;
      Object.Buffer_Visibles.Copy_Data (Object.Buffer_Indices, GL.Types.UInt_Type);
      Object.Prefix_Sum.Compute_Prefix_Sum (Object.Buffer_Indices);

      --  Create a compacted array of transforms: if a part is visible
      --  according to Buffer_Visibles, then its transform will be copied
      --  to the position denoted by the part's offset number in Buffer_Indices
      Object.Compact (Transforms, Commands);

      Compacted_Transforms := Object.Compacted_Transforms;
      Compacted_Commands   := Object.Compacted_Commands;
   end Cull;

end Orka.Culling;
