--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2015 onox <denkpadje@gmail.com>
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

with GL.Objects.Buffers;

with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Programs.Uniforms;

package body Orka.Rendering.Programs is

   function Create_Program (Modules   : Programs.Modules.Module_Array;
                            Separable : Boolean := False) return Program is
      use type GL.Types.Int;
   begin
      return Result : Program do
         Result.GL_Program.Set_Separable (Separable);

         --  Attach all shaders to the program before linking
         Programs.Modules.Attach_Shaders (Modules, Result);

         Result.GL_Program.Link;
         Programs.Modules.Detach_Shaders (Modules, Result);

         if not Result.GL_Program.Link_Status then
            raise Program_Link_Error with Result.GL_Program.Info_Log;
         end if;
      end return;
   end Create_Program;

   function Create_Program (Module    : Programs.Modules.Module;
                            Separable : Boolean := False) return Program is
   begin
      return Create_Program (Modules.Module_Array'(1 => Module), Separable);
   end Create_Program;

   procedure Use_Program (Object : in out Program) is
   begin
      Object.GL_Program.Use_Program;
   end Use_Program;

   function Compute_Work_Group_Size
     (Object : Program) return GL.Types.Compute.Dimension_Size_Array
   is (Object.GL_Program.Compute_Work_Group_Size);

   function Uniform_Sampler (Object : Program; Name : String)
     return Uniforms.Uniform_Sampler is
   begin
      return Uniforms.Create_Uniform_Sampler (Object, Name);
   end Uniform_Sampler;

   function Uniform_Image (Object : Program; Name : String)
     return Uniforms.Uniform_Image is
   begin
      return Uniforms.Create_Uniform_Image (Object, Name);
   end Uniform_Image;

   function Uniform (Object : Program; Name : String)
     return Uniforms.Uniform is
   begin
      return Uniforms.Create_Uniform_Variable (Object, Name);
   end Uniform;

   function Binding
     (Object : Program;
      Target : Buffers.Indexed_Buffer_Target;
      Name   : String) return Natural
   is
      use all type Buffers.Indexed_Buffer_Target;
   begin
      return Natural (Object.GL_Program.Buffer_Binding
        ((case Target is
           when Uniform        => GL.Objects.Buffers.Uniform,
           when Shader_Storage => GL.Objects.Buffers.Shader_Storage,
           when Atomic_Counter => GL.Objects.Buffers.Atomic_Counter),
         Name));
   end Binding;

end Orka.Rendering.Programs;
