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

with GL.Barriers;
with GL.Compute;
with GL.Types.Compute;

package body Orka.Algorithms.Prefix_Sums is

   function Create_Prefix_Sum
     (Context : aliased Orka.Contexts.Context'Class;
      Length  : Positive) return Prefix_Sum
   is
      use Rendering.Buffers;
      use Rendering.Shaders;
      use Rendering.Shaders.Objects;
      use all type Types.Numeric_Type;

      Program_Prefix_Sum : constant Rendering.Shaders.Objects.Shader_Objects :=
        [Compute_Shader => Create_Shader (Compute_Shader, Path => "orka:algorithms/prefix-sum.comp"),
         others         => Empty];

      Program_Add : constant Rendering.Shaders.Objects.Shader_Objects :=
        [Compute_Shader => Create_Shader (Compute_Shader, Path => "orka:algorithms/prefix-sum-add.comp"),
         others         => Empty];

      Work_Group_Count : constant GL.Types.Compute.Dimension_Size_Array
        := GL.Compute.Max_Compute_Work_Group_Count;
      Max_Group_Count : constant Natural := Natural (Work_Group_Count (X));

      Work_Group_Size : constant GL.Types.Compute.Dimension_Size_Array
        := Program_Prefix_Sum (Compute_Shader).Value.Compute_Work_Group_Size;

      --  Multiply by 4 because the shader processes 4 numbers per invocation
      Local_Size : constant Natural := 4 * Natural (Work_Group_Size (X));

      Padding     : constant Boolean := Length rem Local_Size /= 0;
      Work_Groups : constant Natural
        := Length / Local_Size + (if Padding then 1 else 0);

      Sum_Padding     : constant Boolean := Work_Groups rem Local_Size /= 0;
      Sum_Work_Groups : constant Natural
        := Work_Groups / Local_Size + (if Sum_Padding then 1 else 0);

      pragma Assert (Work_Groups     <= Max_Group_Count);
      pragma Assert (Sum_Work_Groups <= Max_Group_Count);

      pragma Assert (Sum_Work_Groups <= Local_Size);
      --  Support prefix sum of work group totals without recursion
   begin
      return Result : constant Prefix_Sum :=
        (Program_Prefix_Sum => Program_Prefix_Sum,
         Program_Add        => Program_Add,
         Context            => Context'Access,

         Length          => Length,
         Work_Groups     => Work_Groups,
         Sum_Work_Groups => Sum_Work_Groups,

         --  Buffer_1 is the buffer given as a parameter to Compute_Prefix_Sum
         --  and contains the initial/final values
         Buffer_2 => Create_Buffer
           (Flags  => (others => False),
            Kind   => UInt_Type,
            Length => Work_Groups),
         Buffer_3 => Create_Buffer
           (Flags  => (others => False),
            Kind   => UInt_Type,
            Length => Sum_Work_Groups),
         Buffer_4 => Create_Buffer
           (Flags  => (others => False),
            Kind   => UInt_Type,
            Length => 1));
   end Create_Prefix_Sum;

   procedure Compute_Prefix_Sum
     (Object : in out Prefix_Sum;
      Buffer : Rendering.Buffers.Buffer)
   is
      use GL.Types;
      use all type Rendering.Buffers.Indexed_Buffer_Target;

      procedure Compute_Sum
        (Buffer_1, Buffer_2 : Rendering.Buffers.Buffer; Work_Groups : Natural) is
      begin
         Buffer_1.Bind (Shader_Storage, 0);
         Buffer_2.Bind (Shader_Storage, 1);

         GL.Barriers.Memory_Barrier ((Shader_Storage => True, others => False));
         GL.Compute.Dispatch_Compute (X => UInt (Work_Groups));
      end Compute_Sum;

      procedure Add
        (Buffer_1, Buffer_2 : Rendering.Buffers.Buffer; Work_Groups : Natural) is
      begin
         Buffer_1.Bind (Shader_Storage, 0);
         Buffer_2.Bind (Shader_Storage, 1);

         GL.Barriers.Memory_Barrier ((Shader_Storage => True, others => False));
         GL.Compute.Dispatch_Compute (X => UInt (Work_Groups));
      end Add;
   begin
      --  Phase 1a: Compute prefix sum
      Object.Context.Bind_Shaders (Object.Program_Prefix_Sum);

      Compute_Sum (Buffer, Object.Buffer_2, Object.Work_Groups);

      --  A prefix sum has been computed per work group. If we have multiple
      --  groups, then we need to compute offsets and add them to the individual
      --  prefix sums
      if Object.Work_Groups > 1 then
         --  Phase 2a: Apply prefix sum to buffer containing total of each work group
         --  This will be an exclusive sum, so the first element will be set to zero
         Compute_Sum (Object.Buffer_2, Object.Buffer_3, Object.Sum_Work_Groups);

         if Object.Sum_Work_Groups > 1 then
            --  Phase 3a
            Compute_Sum (Object.Buffer_3, Object.Buffer_4, 1);
            --  Buffer_4 is always unused because of the assumption that
            --  Sum_Work_Groups <= Local_Size so that we only need one work group
            --  (otherwise we would need to apply the algorithm to Buffer_4 as well)

            --  Phase 3b: There is no phase 3b because there is only one work group

            --  Phase 2b: Add offsets to the prefix sums of the totals
            Object.Context.Bind_Shaders (Object.Program_Add);
            Add (Object.Buffer_2, Object.Buffer_3, Object.Work_Groups);
         else
            --  Buffer_3 is unused, we can directly use Buffer_2 for the
            --  offsets (since it's an exclusive sum)
            Object.Context.Bind_Shaders (Object.Program_Add);
         end if;

         --  Phase 1b: Add offsets to the prefix sums
         Add (Buffer, Object.Buffer_2, Object.Work_Groups);
      else
         --  Buffer_2 is unused, we can directly use Buffer as the final result
         null;
      end if;
   end Compute_Prefix_Sum;

end Orka.Algorithms.Prefix_Sums;
