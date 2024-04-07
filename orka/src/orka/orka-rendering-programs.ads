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

with GL.Objects.Programs;

with GL.Types.Compute;

with Orka.Rendering.Buffers;
with Orka.Resources.Locations;

limited with Orka.Rendering.Programs.Modules;
limited with Orka.Rendering.Programs.Uniforms;

package Orka.Rendering.Programs is
   pragma Preelaborate;

   subtype Dimension_Size_Array is GL.Types.Compute.Dimension_Size_Array;

   type Program is tagged private;

   function Create_Program (Module  : Programs.Modules.Module) return Program;
   function Create_Program (Modules : Programs.Modules.Module_Array) return Program;

   procedure Use_Program (Object : Program);
   --  Use the program during rendering

   function Compute_Work_Group_Size
     (Object : Program) return Dimension_Size_Array;

   function Uniform_Sampler (Object : Program; Name : String) return Uniforms.Uniform_Sampler;
   --  Return the uniform sampler that has the given name
   --
   --  This function is only needed in order to call procedure Verify_Compatibility
   --  to verify that the kind and format of the sampler and texture are
   --  compatible.
   --
   --  To bind a texture to a sampler, call Orka.Rendering.Textures.Bind.
   --
   --  Name must be a GLSL uniform sampler. A Uniforms.Uniform_Inactive_Error
   --  exception is raised if the name is not defined in any of the attached shaders.

   function Uniform_Image (Object : Program; Name : String) return Uniforms.Uniform_Image;
   --  Return the uniform image that has the given name
   --
   --  This function is only needed in order to call procedure Verify_Compatibility
   --  to verify that the kind and format of the image sampler and texture are
   --  compatible.
   --
   --  To bind a texture to a image sampler, call Orka.Rendering.Textures.Bind.
   --
   --  Name must be a GLSL uniform image. A Uniforms.Uniform_Inactive_Error
   --  exception is raised if the name is not defined in any of the attached shaders.

   function Uniform (Object : Program; Name : String) return Uniforms.Uniform;
   --  Return the uniform that has the given name
   --
   --  Name must be a GLSL uniform. A Uniforms.Uniform_Inactive_Error exception
   --  is raised if the name is not defined in any of the attached shaders.

   function Binding
     (Object : Program;
      Target : Buffers.Indexed_Buffer_Target;
      Name   : String) return Natural;
   --  Return the index of the binding point of a shader storage block (SSBO),
   --  uniform block (UBO), or an atomic counter buffer
   --
   --  Name must be a GLSL shader storage block or uniform block.
   --  A Uniforms.Uniform_Inactive_Error exception is raised if
   --  the name is not defined in any of the attached shaders.

   Program_Link_Error : exception;

   function GL_Program (Object : Program) return GL.Objects.Programs.Program;

   -----------------------------------------------------------------------------

   type Shader_Kind is
     (Vertex_Shader, Tess_Control_Shader, Tess_Evaluation_Shader, Geometry_Shader, Fragment_Shader, Compute_Shader);

   type Shader_Program is new Program with private;

   function Kind (Object : Shader_Program) return Shader_Kind;

   overriding function Create_Program (Module  : Programs.Modules.Module) return Shader_Program;
   overriding function Create_Program (Modules : Programs.Modules.Module_Array) return Shader_Program;

   function Create_Program (Module  : Programs.Modules.Shader_Module) return Shader_Program;
   function Create_Program (Modules : Programs.Modules.Shader_Module_Array) return Shader_Program;

   type String_Access is not null access constant String;

   type String_Array is array (Positive range <>) of String_Access;

   function Create_Program
     (Location : Orka.Resources.Locations.Location_Ptr;
      Kind     : Shader_Kind;
      Paths    : String_Array) return Shader_Program;

   function Create_Program
     (Location : Orka.Resources.Locations.Location_Ptr;
      Kind     : Shader_Kind;
      Path     : String) return Shader_Program;

   function Create_Program
     (Location : Orka.Resources.Locations.Location_Ptr;
      Kind     : Shader_Kind;
      Paths    : String_Array;
      Render_Modules  : Programs.Modules.Module_Array) return Shader_Program;

private

   type Program is tagged record
      GL_Program : GL.Objects.Programs.Program;
   end record;

   function GL_Program (Object : Program) return GL.Objects.Programs.Program is (Object.GL_Program);

   type Shader_Program is new Program with record
      Kind : Shader_Kind;
   end record;

   function Kind (Object : Shader_Program) return Shader_Kind is (Object.Kind);

end Orka.Rendering.Programs;
