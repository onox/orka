--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2020 onox <denkpadje@gmail.com>
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

package body Orka.Algorithms.FFT is

   function Create_FFT (Context : aliased Orka.Contexts.Context'Class) return FFT is
      use Rendering.Shaders;
      use Rendering.Shaders.Objects;
   begin
      return Result : FFT :=
        (Program => [Compute_Shader => Create_Shader (Compute_Shader, Path => "orka:algorithms/fft.comp"),
                     others         => Empty],
         Context => Context'Access,
         others  => <>)
      do
         Result.Uniform_Size      := Result.Program (Compute_Shader).Value.Uniform ("size");
         Result.Uniform_Transpose := Result.Program (Compute_Shader).Value.Uniform ("transposeData");
         Result.Uniform_Inverse   := Result.Program (Compute_Shader).Value.Uniform ("inverseFFT");

         declare
            Work_Group_Size : constant Dimension_Size_Array
              := Result.Program (Compute_Shader).Value.Compute_Work_Group_Size;
         begin
            Result.Local_Size := Positive (Work_Group_Size (X));
         end;
      end return;
   end Create_FFT;

   procedure Compute_FFT
     (Object : in out FFT;
      Buffer : Rendering.Buffers.Buffer;
      Width, Height : Positive;
      Transpose, Inverse : Boolean)
   is
      use all type Rendering.Buffers.Indexed_Buffer_Target;

      Columns : constant Positive := (if Transpose then Height else Width);
      Rows    : constant Positive := (if Transpose then Width  else Height);

      pragma Assert (Columns <= Object.Local_Size);

      Rows_In_Shared : constant Size := Size (Object.Local_Size / Columns);
   begin
      Object.Uniform_Size.Set_Vector
        (Unsigned_32_Array'(Unsigned_32 (Width), Unsigned_32 (Height)));

      Object.Uniform_Transpose.Set_Boolean (Transpose);
      Object.Uniform_Inverse.Set_Boolean (Inverse);

      Object.Context.Bind_Shaders (Object.Program);

      Buffer.Bind (Shader_Storage, 0);

      GL.Barriers.Memory_Barrier ((Shader_Storage => True, others => False));
      GL.Compute.Dispatch_Compute
        (X => Unsigned_32 (Float_32'Ceiling (Float_32 (Rows) / Float_32 (Rows_In_Shared))));
   end Compute_FFT;

end Orka.Algorithms.FFT;
