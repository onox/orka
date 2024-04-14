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

   type Shader_Kind is
     (Vertex_Shader, Tess_Control_Shader, Tess_Evaluation_Shader, Geometry_Shader, Fragment_Shader, Compute_Shader);

   type Shader_Program is tagged private;

   function Kind (Object : Shader_Program) return Shader_Kind;

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

   function Create_Program_From_Source
     (Kind : Shader_Kind;
      Text : String) return Shader_Program;

   -----------------------------------------------------------------------------

   function Compute_Work_Group_Size
     (Object : Shader_Program) return Dimension_Size_Array;

   function Uniform_Sampler (Object : Shader_Program; Name : String) return Uniforms.Uniform_Sampler;
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

   function Uniform_Image (Object : Shader_Program; Name : String) return Uniforms.Uniform_Image;
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

   function Uniform (Object : Shader_Program; Name : String) return Uniforms.Uniform;
   --  Return the uniform that has the given name
   --
   --  Name must be a GLSL uniform. A Uniforms.Uniform_Inactive_Error exception
   --  is raised if the name is not defined in any of the attached shaders.

   function Binding
     (Object : Shader_Program;
      Target : Buffers.Indexed_Buffer_Target;
      Name   : String) return Natural;
   --  Return the index of the binding point of a shader storage block (SSBO),
   --  uniform block (UBO), or an atomic counter buffer
   --
   --  Name must be a GLSL shader storage block or uniform block.
   --  A Uniforms.Uniform_Inactive_Error exception is raised if
   --  the name is not defined in any of the attached shaders.

   Program_Link_Error : exception;

   -----------------------------------------------------------------------------
   --                                 Internal                                --
   -----------------------------------------------------------------------------

   function GL_Program (Object : Shader_Program) return GL.Objects.Programs.Program;

private

   type Shader_Program is tagged record
      GL_Program : GL.Objects.Programs.Program;
      Kind : Shader_Kind;
   end record;

   function GL_Program (Object : Shader_Program) return GL.Objects.Programs.Program is (Object.GL_Program);

   function Kind (Object : Shader_Program) return Shader_Kind is (Object.Kind);

end Orka.Rendering.Programs;
