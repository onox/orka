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

   function Create_Separable_Program (Modules   : Programs.Modules.Module_Array;
                            Separable : Boolean) return Program is
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
   end Create_Separable_Program;

   function Create_Program (Modules : Programs.Modules.Module_Array) return Program is (Create_Separable_Program (Modules, False));
   function Create_Program (Module  : Programs.Modules.Module) return Program is (Create_Separable_Program ([Module], False));

   overriding function Create_Program (Modules : Programs.Modules.Module_Array) return Shader_Program is
     (Shader_Program'(Create_Separable_Program (Modules, True) with Kind => Vertex_Shader));
   overriding function Create_Program (Module  : Programs.Modules.Module) return Shader_Program is
     (Shader_Program'(Create_Separable_Program ([Module], True) with Kind => Vertex_Shader));

   function Create_Program (Module : Programs.Modules.Shader_Module) return Shader_Program is
     (Shader_Program'(Create_Separable_Program ([Programs.Modules.Module (Module)], True) with Kind => Module.Kind));

   function Create_Program (Modules : Programs.Modules.Shader_Module_Array) return Shader_Program is
     (Shader_Program'(Create_Separable_Program ([for Module of Modules => Programs.Modules.Module (Module)], True)
                        with Kind => Modules (Modules'First).Kind));

   function Create_Program
     (Location : Orka.Resources.Locations.Location_Ptr;
      Kind     : Shader_Kind;
      Paths    : String_Array) return Shader_Program
   is (Create_Program (Modules.Shader_Module_Array'([for Path of Paths => Modules.Create_Module (Location, Kind, Path.all)])));

   function Create_Program
     (Location : Orka.Resources.Locations.Location_Ptr;
      Kind     : Shader_Kind;
      Paths    : String_Array;
      Render_Modules  : Programs.Modules.Module_Array) return Shader_Program
   is
      use type Programs.Modules.Module_Array;

      Modules_From_Paths : constant Programs.Modules.Module_Array :=
        [for Path of Paths => Programs.Modules.Module (Programs.Modules.Create_Module (Location, Kind, Path.all))];

      All_Modules : constant Programs.Modules.Module_Array := Modules_From_Paths & Render_Modules;
   begin
      return Shader_Program'(Create_Separable_Program (All_Modules, True) with Kind => Kind);
   end Create_Program;

   function Create_Program
     (Location : Orka.Resources.Locations.Location_Ptr;
      Kind     : Shader_Kind;
      Path     : String) return Shader_Program
   is (Create_Program (Modules.Create_Module (Location, Kind, Path)));

   procedure Use_Program (Object : Program) is
   begin
      Object.GL_Program.Use_Program;
   end Use_Program;

   function Compute_Work_Group_Size
     (Object : Program) return Dimension_Size_Array
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
           when Shader_Storage => GL.Objects.Buffers.Shader_Storage),
         Name));
   end Binding;

end Orka.Rendering.Programs;
