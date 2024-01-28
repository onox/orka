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

private with GL.Objects.Programs;

with GL.Types.Compute;

with Orka.Rendering.Buffers;

limited with Orka.Rendering.Programs.Modules;
limited with Orka.Rendering.Programs.Uniforms;

package Orka.Rendering.Programs is
   pragma Preelaborate;

   subtype Dimension_Size_Array is GL.Types.Compute.Dimension_Size_Array;

   type Program is tagged private;

   function Create_Program (Module    : Programs.Modules.Module;
                            Separable : Boolean := False) return Program;

   function Create_Program (Modules   : Programs.Modules.Module_Array;
                            Separable : Boolean := False) return Program;

   procedure Use_Program (Object : in out Program);
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

private

   type Program is tagged record
      GL_Program : GL.Objects.Programs.Program;
   end record;

end Orka.Rendering.Programs;
