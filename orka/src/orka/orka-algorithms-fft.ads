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

with Orka.Contexts;
with Orka.Rendering.Buffers;
with Orka.Types;

private with GL.Low_Level.Enums;

private with Orka.Rendering.Shaders.Objects;
private with Orka.Rendering.Shaders.Uniforms;

package Orka.Algorithms.FFT is
   pragma Preelaborate;

   type FFT (Context : not null access constant Orka.Contexts.Context'Class) is tagged limited private;

   function Create_FFT (Context : aliased Orka.Contexts.Context'Class) return FFT;

   use type Types.Element_Type;

   procedure Compute_FFT
     (Object : in out FFT;
      Buffer : Rendering.Buffers.Buffer;
      Width, Height : Positive;
      Transpose, Inverse : Boolean)
   with Pre => Buffer.Kind = Types.Single_Type
                 and Width * Height * 2 = Buffer.Length
                 and Types.Is_Power_Of_Two (if Transpose then Height else Width);

private

   package LE renames GL.Low_Level.Enums;

   type FFT (Context : not null access constant Orka.Contexts.Context'Class) is tagged limited record
      Program    : Rendering.Shaders.Objects.Shader_Objects;
      Local_Size : Positive;

      Uniform_Size      : Rendering.Shaders.Uniforms.Uniform (LE.UInt_Vec2);
      Uniform_Transpose : Rendering.Shaders.Uniforms.Uniform (LE.Bool_Type);
      Uniform_Inverse   : Rendering.Shaders.Uniforms.Uniform (LE.Bool_Type);
   end record;

end Orka.Algorithms.FFT;
