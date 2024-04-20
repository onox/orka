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

with Orka.Rendering.Shaders.Modules;
with Orka.Rendering.Shaders.Uniforms;

package body Orka.Rendering.Shaders is

   function Create_Shader (Modules : Shaders.Modules.Shader_Module_Array; Kind : Shader_Kind) return Shader is
      pragma Assert (for all Module of Modules => Module.Kind = Kind);
   begin
      return Result : constant Shader := (Kind => Kind, others => <>) do
         Result.GL_Program.Set_Separable (True);

         --  Attach all shaders to the program before linking
         Shaders.Modules.Attach_Shaders (Modules, Result);

         Result.GL_Program.Link;
         Shaders.Modules.Detach_Shaders (Modules, Result);

         if not Result.GL_Program.Link_Status then
            raise Program_Link_Error with Result.GL_Program.Info_Log;
         end if;
      end return;
   end Create_Shader;

   function Create_Shader (Modules : Shaders.Modules.Shader_Module_Array) return Shader is
     (Create_Shader (Modules, Modules (Modules'First).Kind));

   function Create_Shader
     (Location : Orka.Resources.Locations.Location_Ptr;
      Kind     : Shader_Kind;
      Paths    : String_Array) return Shader
   is (Create_Shader (Modules.Shader_Module_Array'([for Path of Paths => Modules.Create_Module (Location, Kind, Path.all)])));

   function Create_Shader
     (Location : Orka.Resources.Locations.Location_Ptr;
      Kind     : Shader_Kind;
      Path     : String) return Shader
   is (Create_Shader ([Modules.Create_Module (Location, Kind, Path)], Kind));

   function Create_Shader_From_Source
     (Kind : Shader_Kind;
      Text : String) return Shader
   is (Create_Shader ([Modules.Create_Module_From_Source (Kind, Text)], Kind));

   function Compute_Work_Group_Size
     (Object : Shader) return Dimension_Size_Array
   is (Object.GL_Program.Compute_Work_Group_Size);

   function Uniform_Sampler (Object : Shader; Name : String) return Uniforms.Uniform_Sampler is
   begin
      return Uniforms.Create_Uniform_Sampler (Object, Name);
   end Uniform_Sampler;

   function Uniform_Image (Object : Shader; Name : String) return Uniforms.Uniform_Image is
   begin
      return Uniforms.Create_Uniform_Image (Object, Name);
   end Uniform_Image;

   function Uniform (Object : Shader; Name : String) return Uniforms.Uniform is
   begin
      return Uniforms.Create_Uniform_Variable (Object, Name);
   end Uniform;

   function Binding
     (Object : Shader;
      Target : Buffers.Indexed_Buffer_Target;
      Name   : String) return Natural
   is
      use all type Buffers.Indexed_Buffer_Target;
   begin
      return Natural (Object.GL_Program.Buffer_Binding
        ((case Target is
           when Uniform        => GL.Objects.Buffers.Uniform,
           when Shader_Storage => GL.Objects.Buffers.Shader_Storage),
         Name));
   end Binding;

end Orka.Rendering.Shaders;
